
//@ sourceMappingURL=jquery.caret.map
/*
  Implement Github like autocomplete mentions
  http://ichord.github.com/At.js

  Copyright (c) 2013 chord.luo@gmail.com
  Licensed under the MIT license.
*/


/*
本插件操作 textarea 或者 input 内的插入符
只实现了获得插入符在文本框中的位置，我设置
插入符的位置.
*/


(function() {
  (function(factory) {
    if (typeof define === 'function' && define.amd) {
      return define(['jquery'], factory);
    } else {
      return factory(window.jQuery);
    }
  })(function($) {
    "use strict";
    var EditableCaret, InputCaret, Mirror, Utils, methods, pluginName;

    pluginName = 'caret';
    EditableCaret = (function() {
      function EditableCaret($inputor) {
        this.$inputor = $inputor;
        this.domInputor = this.$inputor[0];
      }

      EditableCaret.prototype.setPos = function(pos) {
        return this.domInputor;
      };

      EditableCaret.prototype.getIEPosition = function() {
        return $.noop();
      };

      EditableCaret.prototype.getPosition = function() {
        return $.noop();
      };

      EditableCaret.prototype.getOldIEPos = function() {
        var preCaretTextRange, textRange;

        textRange = document.selection.createRange();
        preCaretTextRange = document.body.createTextRange();
        preCaretTextRange.moveToElementText(this.domInputor);
        preCaretTextRange.setEndPoint("EndToEnd", textRange);
        return preCaretTextRange.text.length;
      };

      EditableCaret.prototype.getPos = function() {
        var clonedRange, pos, range;

        if (range = this.range()) {
          clonedRange = range.cloneRange();
          clonedRange.selectNodeContents(this.domInputor);
          clonedRange.setEnd(range.endContainer, range.endOffset);
          pos = clonedRange.toString().length;
          clonedRange.detach();
          return pos;
        } else if (document.selection) {
          return this.getOldIEPos();
        }
      };

      EditableCaret.prototype.getOldIEOffset = function() {
        var range, rect;

        range = document.selection.createRange().duplicate();
        range.moveStart("character", -1);
        rect = range.getBoundingClientRect();
        return {
          height: rect.bottom - rect.top,
          left: rect.left,
          top: rect.top
        };
      };

      EditableCaret.prototype.getOffset = function(pos) {
        var clonedRange, offset, range, rect;

        offset = null;
        if (window.getSelection && (range = this.range())) {
          if (range.endOffset - 1 < 0) {
            return null;
          }
          clonedRange = range.cloneRange();
          clonedRange.setStart(range.endContainer, range.endOffset - 1);
          clonedRange.setEnd(range.endContainer, range.endOffset);
          rect = clonedRange.getBoundingClientRect();
          offset = {
            height: rect.height,
            left: rect.left + rect.width,
            top: rect.top
          };
          clonedRange.detach();
          offset;
        } else if (document.selection) {
          this.getOldIEOffset();
        }
        return Utils.adjustOffset(offset, this.$inputor);
      };

      EditableCaret.prototype.range = function() {
        var sel;

        if (!window.getSelection) {
          return;
        }
        sel = window.getSelection();
        if (sel.rangeCount > 0) {
          return sel.getRangeAt(0);
        } else {
          return null;
        }
      };

      return EditableCaret;

    })();
    InputCaret = (function() {
      function InputCaret($inputor) {
        this.$inputor = $inputor;
        this.domInputor = this.$inputor[0];
      }

      InputCaret.prototype.getIEPos = function() {
        var endRange, inputor, len, normalizedValue, pos, range, textInputRange;

        inputor = this.domInputor;
        range = document.selection.createRange();
        pos = 0;
        if (range && range.parentElement() === inputor) {
          normalizedValue = inputor.value.replace(/\r\n/g, "\n");
          len = normalizedValue.length;
          textInputRange = inputor.createTextRange();
          textInputRange.moveToBookmark(range.getBookmark());
          endRange = inputor.createTextRange();
          endRange.collapse(false);
          if (textInputRange.compareEndPoints("StartToEnd", endRange) > -1) {
            pos = len;
          } else {
            pos = -textInputRange.moveStart("character", -len);
          }
        }
        return pos;
      };

      InputCaret.prototype.getPos = function() {
        if (document.selection) {
          return this.getIEPos();
        } else {
          return this.domInputor.selectionStart;
        }
      };

      InputCaret.prototype.setPos = function(pos) {
        var inputor, range;

        inputor = this.domInputor;
        if (document.selection) {
          range = inputor.createTextRange();
          range.move("character", pos);
          range.select();
        } else if (inputor.setSelectionRange) {
          inputor.setSelectionRange(pos, pos);
        }
        return inputor;
      };

      InputCaret.prototype.getIEOffset = function(pos) {
        var h, range, textRange, x, y;

        textRange = this.domInputor.createTextRange();
        if (pos) {
          textRange.move('character', pos);
        } else {
          range = document.selection.createRange();
          textRange.moveToBookmark(range.getBookmark());
        }
        x = textRange.boundingLeft;
        y = textRange.boundingTop;
        h = textRange.boundingHeight;
        return {
          left: x,
          top: y,
          height: h
        };
      };

      InputCaret.prototype.getOffset = function(pos) {
        var $inputor, offset, position;

        $inputor = this.$inputor;
        if (document.selection) {
          return Utils.adjustOffset(this.getIEOffset(pos), $inputor);
        } else {
          offset = $inputor.offset();
          position = this.getPosition(pos);
          return offset = {
            left: offset.left + position.left,
            top: offset.top + position.top,
            height: position.height
          };
        }
      };

      InputCaret.prototype.getPosition = function(pos) {
        var $inputor, at_rect, format, html, mirror, start_range;

        $inputor = this.$inputor;
        format = function(value) {
          return value.replace(/</g, '&lt').replace(/>/g, '&gt').replace(/`/g, '&#96').replace(/"/g, '&quot').replace(/\r\n|\r|\n/g, "<br />");
        };
        if (pos === void 0) {
          pos = this.getPos();
        }
        start_range = $inputor.val().slice(0, pos);
        html = "<span>" + format(start_range) + "</span>";
        html += "<span id='caret'>|</span>";
        mirror = new Mirror($inputor);
        return at_rect = mirror.create(html).rect();
      };

      InputCaret.prototype.getIEPosition = function(pos) {
        var h, inputorOffset, offset, x, y;

        offset = this.getIEOffset(pos);
        inputorOffset = this.$inputor.offset();
        x = offset.left - inputorOffset.left;
        y = offset.top - inputorOffset.top;
        h = offset.height;
        return {
          left: x,
          top: y,
          height: h
        };
      };

      return InputCaret;

    })();
    Mirror = (function() {
      Mirror.prototype.css_attr = ["overflowY", "height", "width", "paddingTop", "paddingLeft", "paddingRight", "paddingBottom", "marginTop", "marginLeft", "marginRight", "marginBottom", "fontFamily", "borderStyle", "borderWidth", "wordWrap", "fontSize", "lineHeight", "overflowX", "text-align"];

      function Mirror($inputor) {
        this.$inputor = $inputor;
      }

      Mirror.prototype.mirrorCss = function() {
        var css,
          _this = this;

        css = {
          position: 'absolute',
          left: -9999,
          top: 0,
          zIndex: -20000,
          'white-space': 'pre-wrap'
        };
        $.each(this.css_attr, function(i, p) {
          return css[p] = _this.$inputor.css(p);
        });
        return css;
      };

      Mirror.prototype.create = function(html) {
        this.$mirror = $('<div></div>');
        this.$mirror.css(this.mirrorCss());
        this.$mirror.html(html);
        this.$inputor.after(this.$mirror);
        return this;
      };

      Mirror.prototype.rect = function() {
        var $flag, pos, rect;

        $flag = this.$mirror.find("#caret");
        pos = $flag.position();
        rect = {
          left: pos.left,
          top: pos.top,
          height: $flag.height()
        };
        this.$mirror.remove();
        return rect;
      };

      return Mirror;

    })();
    Utils = {
      adjustOffset: function(offset, $inputor) {
        if (!offset) {
          return;
        }
        offset.top += $(window).scrollTop() + $inputor.scrollTop();
        offset.left += +$(window).scrollLeft() + $inputor.scrollLeft();
        return offset;
      },
      contentEditable: function($inputor) {
        return !!($inputor[0].contentEditable && $inputor[0].contentEditable === 'true');
      }
    };
    methods = {
      pos: function(pos) {
        if (pos) {
          return this.setPos(pos);
        } else {
          return this.getPos();
        }
      },
      position: function(pos) {
        if (document.selection) {
          return this.getIEPosition(pos);
        } else {
          return this.getPosition(pos);
        }
      },
      offset: function(pos) {
        return this.getOffset(pos);
      }
    };
    $.fn.caret = function(method) {
      var caret;

      caret = Utils.contentEditable(this) ? new EditableCaret(this) : new InputCaret(this);
      if (methods[method]) {
        return methods[method].apply(caret, Array.prototype.slice.call(arguments, 1));
      } else {
        return $.error("Method " + method + " does not exist on jQuery.caret");
      }
    };
    $.fn.caret.EditableCaret = EditableCaret;
    $.fn.caret.InputCaret = InputCaret;
    $.fn.caret.Utils = Utils;
    return $.fn.caret.apis = methods;
  });

}).call(this);
/*
---------------------------------------
对jquery事件处理 bind,的扩展，让键盘事件更方便被监听
对keydown,keyup,keypress扩展
对e.keyName
对各个按键，取了易用的名字
--------------------------------------
keyEvent,目的，是用来实现：$().bind("keydwon","各种按键",function(){});
其中“各种按键”，可以是any(如果是any的话，说明只要是按键就可以了，那就直接使用：$().bind("keydown",function())了,anyLetter,anyNumber,anyF,anyArrow,以及他们每个元素的名字，元素也可以是ctrl+a之类的组合键，格式可以是String(如："a anyNumber ctrl+b"），或以["a","anyNumber","ctrl+b"]来包含各个元素，每当键盘事件发生时，就会根据你设定的。名字，可以参考源代码
1.event.keyName//这个事件值表示对的名字和event.keyCode相反,只有在本插件的情况下,才会有event.keyName
2.扩展了Array的test函数,用来测试,是不是包含某一个元素

$(document).bind("keydown","a v anyNumber ctrl+a",function(){
	alert("find");
});
$(document).bind("keydown",["a","v","anyNumber","ctrl+a"],function(e){
	alert("find"+e.keyName);
});
*/
(function(jQuery){
	Array.prototype.test=function(num){//测试数组是不是包含有num这样的元素
		for(var i in this){
			if(this[i]==num)
			return true;
		}
		return false;
	}
	jQuery.keyEventEx = {
		version: "1.0",
		keyName: {//你可以通过一个值,得出它的名字.
			8: "backspace", 9: "tab", 10: "enter", 13: "enter", 16: "shift", 17: "ctrl", 18: "alt", 19: "pause",
			20: "capsLock", 27: "esc", 32: "space",
			33: "pageUp", 34: "pageDown", 35: "end", 36: "home", 45: "insert", 46: "delete",
			37: "left", 38: "up", 39: "right",40: "down",
			48: '0', 49: '1', 50: '2', 51: '3', 52: '4', 53: '5', 54: '6', 55: '7', 56: '8', 57: '9',//主键盘上的数字 
			65: 'a', 66: 'b', 67: 'c', 68: 'd', 69: 'e', 70: 'f', 71: 'g', 72: 'h', 73: 'i', 74: 'j', 75: 'k', 76: 'l', 77: 'm',
			78: 'n', 79: 'o', 80: 'p', 81: 'q', 82: 'r', 83: 's', 84: 't', 85: 'u', 86: 'v', 87: 'w', 88: 'x', 89: 'y', 90: 'z',
			91:'windows',
			96: "0", 97: "1", 98: "2", 99: "3", 100: "4", 101: "5", 102: "6", 103: "7",104: "8", 105: "9",//小键盘上的数字
			106: "*", 107: "+", 109: "-", 110: ".", 111 : "/", 
			112: "f1", 113: "f2", 114: "f3", 115: "f4", 116: "f5", 117: "f6", 118: "f7", 119: "f8", 
			120: "f9", 121: "f10", 122: "f11", 123: "f12", 144: "numLock", 145: "scroll", 186: ";",187:"=",188:",",189:"-",190:".", 191: "/",192:"`",219:"[",
			220: "\\",221:"]", 222: "'", 224: "meta"
		},
		anyPunctuation:[186,187,188,189,190,191,192,219,220,221,222],//标点
		anyArrow: [37, 38, 39, 40],
		anyNumber: [96,97,98,99,100,101,102,103,104,105,48, 49, 50, 51, 52, 53, 54, 55, 56, 57],//小键盘上的以及注键盘上的
		anyLetter: [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90],
		anyF: [112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123],
		shiftChars: {
			"`": "~", "1": "!", "2": "@", "3": "#", "4": "$", "5": "%", "6": "^", "7": "&", 
			"8": "*", "9": "(", "0": ")", "-": "_", "=": "+", ";": ": ", "'": "\"", ",": "<", 
			".": ">",  "/": "?",  "\\": "|"
		}
		//shift abc就很简单，直接.toUpCase();
	};
	function elementTest(event,el){//测试元素，当你你按键时，与规则的键一样，就返回真
	//一个内部函数，用来处理基本情况
		function BasicElTest(keyCode,basicEl){//测试基本元素，如：a ,anyLetter,也就是非组合键。
			var ex=jQuery.keyEventEx;
			var keyName=ex.keyName[keyCode];
			var any=["anyLetter","anyNumber","anyF","anyArrow","anyPunctuation"];
			for(var i in any){//处理所有any的情况
				if(any.test(basicEl)){
					if(ex[basicEl].test(keyCode)){//检查一下,每一个 anyLetter之类的数组里,是不是含有你键盘上按键值,有,说明包含.就执行.
					return true;
					}
				}
			}
			if(keyName==basicEl){
				return true;
			}
			return false;//如果按键的值，不是你想监听的基本元素，返回false
		}
		//这个来处理组合键。
		var p=/(ctrl|alt|shift|meta|anyNumber|anyArrow|anyLetter|anyF|[a-z])\+/ig;
		var spKey=["ctrl","alt","meta","shift"];
		var ifCombine=[];//存取每一个想探测的组合键，当对应的键出现时，就存为真，当所有的都出现了，那这个组合键就出现了。
		if(p.test(el)){//说明可能是一个组合键
			var combineKeys=el.split("+");
			for(var i=0;i<combineKeys.length;i++){//这里不可以使用for(in  这样，因为，它不是简单的Array对象。
				if(spKey.test(combineKeys[i])){//说明是["ctrl","alt","meta","shift"]其中之一
					if(event[combineKeys[i]+"Key"]){
						ifCombine.push(true);
					}else{
						ifCombine.push(false);
					}
				}else{
					if(BasicElTest(event.keyCode,combineKeys[i])){
						ifCombine.push(true);
					}else{
						ifCombine.push(false);
					}
				}
			}//测试完了组合键情况，如果是合条件的组合键，执行后面的程序
			if(!ifCombine.test(false)){//全为真，所以，想要的组合键出现了
				return true;
			}
		}
		//现在要做的就是,不是组合键,才可以触发
		if(!event.ctrlKey&&!event.metaKey&&!event.altKey&&!event.shiftKey)
		{
			return BasicElTest(event.keyCode,el);
		}else{
			return false;
		}
	}
	function keyHandler( handleObj ) {
		//handleObj.data就是bind("keydown","xx",function())里的第二个值
		//handleObj.handler就是bind里的function();
		/*
		if(handleObj.data){//n当没有第二参数时，就返回到没有使用本插件的版本
		
			return handleObj.handler.apply( this, arguments );
		};
		*/
		var origHandler = handleObj.handler;//保存一下bind("keydown","x",function())里的函数
		handleObj.handler = function(e) {
			var data=e.data;
			var datas=[];
			if(!data){//如果没有data,也就是bind("keydwon",function())了，那么，就是任何按键了！
				return origHandler.apply( this, arguments );
			}
			if(typeof data==="string"){//设置第二参数的格式，不同的类型，以空格分开。
				data=data.replace(/\s/," ");
				datas=data.split(" ");
			}
			if($.isArray(data)){
				datas=data;
			}
			for(var i=0;i<datas.length;i++){
				if(elementTest(e,datas[i])){
					e.keyName=jQuery.keyEventEx.keyName[e.keyCode];//让event.keyName生效
					return origHandler.apply( this, arguments );
				}
			}
		};
	}

	jQuery.each([ "keydown", "keyup", "keypress" ], function() {
		jQuery.event.special[ this ] = { add: keyHandler };
	});

})( this.jQuery );

/*
这里的，都是自己扩展的函数，方便在处理：
1。简繁转换
2。半角全角转换
3。对中文字符的处理

*/

(function(){
var simple='皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙闭边编贬变辩辫鳖瘪濒滨宾摈饼拨钵铂驳卜补参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔点垫电淀钓调迭谍叠钉顶锭订东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞国爱赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛伞丧骚扫涩杀纱筛晒闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽绥岁孙损笋缩琐锁獭挞抬摊贪瘫滩坛谭谈叹汤烫涛绦腾誊锑题体屉条贴铁厅听烃铜统头图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝呜钨乌诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮樱婴鹰应缨莹萤营荧蝇颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰帧郑证织职执纸挚掷帜质钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆浊兹资渍踪综总纵邹诅组钻致钟么为只凶准启板里雳余链泄';
var tradition='皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃閉邊編貶變辯辮鼈癟瀕濱賓擯餅撥缽鉑駁蔔補參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締點墊電澱釣調叠諜疊釘頂錠訂東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐國愛賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽傘喪騷掃澀殺紗篩曬閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖綏歲孫損筍縮瑣鎖獺撻擡攤貪癱灘壇譚談歎湯燙濤縧騰謄銻題體屜條貼鐵廳聽烴銅統頭圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩嗚鎢烏誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲櫻嬰鷹應纓瑩螢營熒蠅穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙幀鄭證織職執紙摯擲幟質鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄濁茲資漬蹤綜總縱鄒詛組鑽緻鐘麼為隻兇準啟闆裡靂餘鍊洩';
var simpleToTradition={};
var traditionToSimple={};
for(var i=0;i<simple.length;i++){
	simpleToTradition[simple.charAt(i)]=tradition.charAt(i);
	traditionToSimple[tradition.charAt(i)]=simple.charAt(i);
}

//将char变为繁体的。如：char=门,得出的结果就是:門
String.prototype.toTraditionChars=function(){
	var len=this.length;
	var str="";;
	for(var i=0;i<len;i++){
		var test=simpleToTradition[this.charAt(i)];
		if(test!=undefined){
			str=str+test;
		}
		else{
			str=str+this.charAt(i);
		}
	}
	return str;
}
String.prototype.toSimpleChars=function(){
	var len=this.length;
	var str="";;
	for(var i=0;i<len;i++){
		var test=traditionToSimple[this.charAt(i)];
		if(test!=undefined){
			str=str+test;
		}
		else{
			str=str+this.charAt(i);
		}
	}
	return str;
}
var toChPu={
	",":"，",
	".":"。",
	"/":"÷",
	";":"；",
	"'":"‘",
	"[":"【",
	"]":"】",
	"\\":"、",
	"`":"｀",
	"-":"－",
	"=":"＝",
	"<":"《",
	">":"》",
	"?":"？",
	":":"：",
	"\"":"“",
	"{":"｛",
	"}":"｝",
	"|":"｜",
	"~":"～",
	"_":"——",
	"+":"＋",
	"!":"！",
	"@":"＠",
	"#":"＃",
	"$":"￥",	
	"%":"％",
	"^":"……",
	"&":"＆",
	"*":"×",
	"(":"（",
	")":"）"
}
//这里要注意的是单引号，双引号，都是左半边的。
String.prototype.toChinesePunctuation=function(){
  var result ="";
  for(var i=0;i<this.length;i++){
	  var test=toChPu[this.charAt(i)];
	  if(test!=undefined){
	  	result=result+toChPu[this.charAt(i)];
	  }else{
		  result=result+this.charAt(i);
	  }
  }
 return result;
}
})();//这样做目的就是让变量的作用域只在这里．不影响其他人使用这些变量名
//to double byte char也就是将含有半角的,自动转换成全角的无论是标点，字母
String.prototype.toDBC=function(){
  var result = '';
  for(var i=0; i < this.length; i++){
    var code = this.charCodeAt(i);
    if(code >= 33 && code <= 126){
      result += String.fromCharCode(this.charCodeAt(i) + 65248);
    }else if (code == 32){
      result += String.fromCharCode(this.charCodeAt(i) + 12288 - 32);
    }else{
      result +=this.charAt(i);
    }
  }
 return result;
}
//to singleByteChar也就是将全码的，自动换成半角的．无论是标点，字母
String.prototype.toSBC=function(){
  var result = '';
  for(var i=0; i < this.length; i++){
    code = this.charCodeAt(i);
    if(code >= 65281 && code <= 65374){
      result += String.fromCharCode(this.charCodeAt(i) - 65248);
    }else if (code == 12288){
      result += String.fromCharCode(this.charCodeAt(i) - 12288 + 32);
    }else{
      result += this.charAt(i);
    }
  }
 return result;
} 
/*
对码表格式有这样的要求：
1。字母少的，要排在前面（这样，当你打少的时，少的才会出现，不然，出现了一个多的，让你会很意外）
2。当你输入字母ab时，它是找到码表里以ab字母开头的，排在码表里，第一个位置的数据。
3。码表具体格式是：编码玩素+字或词，字或词（，是用来区别重码的）如：a啊，阿ni你

var ime=new InputEngine(raw,6);
var listIndex=ime.findMbRang("xu");
ime.setList(listIndex[0],listIndex[1]);
ime.setHouXianData();
ime.clearHouXianData();
alert(ime.houXianPg);
输入法引擎的使用原理：1。先把字符串的源码给数组化
2。当你输入字符时，找到你输入法字符串在数组化的码表里的开始，结束位置
3。根据你的候选数情况，给出前几个候选组，以及可以处理上一候选，下一候选，清空候选
4。客户化地显示你你得到的候选数据ime.houXianData
*/
function InputEngine(mb,houXianNum){
	this.mb=this.setMb(mb);
	this.houXianNum=houXianNum;
	InputEngine.prototype.isTradition=false;//是否为繁体方式
	InputEngine.prototype.isQuanJiaoChar=false;//是不是全角字符状态
	InputEngine.prototype.isChinesePunctuation=true;//是不是中文标点状态
	InputEngine.prototype.isChinese=true;//是不是输入中文
	InputEngine.prototype.maxLen=4;//四码定长.
	InputEngine.prototype.houXianNum=6;//设置默认的候选项为6个;
	InputEngine.prototype.houXianData=[];//[[aa,你],[aa，他],[ab，以].....]
	InputEngine.prototype.index=1;//一个标记,用来标记现在是第几组被显示的数据
	InputEngine.prototype.houXianPg=1;//候选的页数
	InputEngine.prototype._List=[];//当你输入ab后,所有符合ab开头的列表
	InputEngine.prototype.inputChars="";//你输入的那些英文,来得转换想要的数据
}
//一个码表全局数据,由字符串源文件得出一个易于处理的数组.["aaa数地","bbb大,戊",等等]
//当你输入一个字母时如“ab“，它会找到以ab开头的第一个位置，以及不是以它开头的第一个位置
InputEngine.prototype.setMb=function(raw){//根据外面的文件里的字符串,设置成javascript里的数组来处理
	var mb=[];
	var pattern = /[a-z';]+[^a-z';]+/g;//a-z ' ;都是做为编码元素的。
	pattern.compile("[a-z';]+[^a-z';]+", "g");
	// "raw" is defined in *-table.js
	while (pattern.exec(raw) != null) mb.push(RegExp.lastMatch);//得到了码表文件。放到了数组里。["aaa数地","bbb大,戊",等等]
	return mb;
}
InputEngine.prototype.findMbRang=function(inputChars) {
	if(inputChars==""){
		return [-1,-1];//当inputChars为空时,直接返回这个数值,表示找到不.
	};
	var start = -1;//始起位置
	var end=-1;
	var low = 0;
	var high = this.mb.length - 1;
	var str_len = inputChars.length;
	var pattern=/[^a-z';]/;//找到所有的，以他们做为开头的字母（编码元素）如c，就是以c开头的，第一个字母
	pattern.compile("[^a-z';]");
	while (low <= high) {
		var mid = Math.floor((low+high)/2);
		var code = this.mb[mid].substr(0,this.mb[mid].search(pattern));
		if (code.substr(0,str_len) == inputChars) {
			start = mid;
			high = mid-1;
		}
		else if (code.substr(0,str_len) > inputChars) high = mid-1;
		else low = mid+1;
	}//指到了start
	var pa=new RegExp("^"+inputChars);//找出以你输入字母开头的所有的字母位置
	pa.compile(pa);
	end=start;
	while(pa.test(this.mb[end])){
		end++;
	}
	return [start,end];//找到以某些字母开头的第一个数据，以及第一个不是以它开头的数据位置
}

/*
在满足条件的字母里，从开始，到结束位置，得出一个可以用于显示的数据数组
返回一个二维数组[[aa,你],[aa，他],[ab，以].....]。主要作用就是把那些重码字，排到数组里，方便显示
*/
InputEngine.prototype.setList=function(start,end){//[[aa,你],[aa，他],[ab，以].....]
	this._List=[];
	if(start<0||start>end){//当你发现输入法的数据不对时,直接返回一个空.比如,你输入了一个mb里没有的字符串,那么List自然也就为空
	return ;
	}
	var sliceArr=this.mb.slice(start,end);
	var i=0;
	while(i<sliceArr.length){
		var ziMu=this.mb[start+i].replace(/[^a-z';]+/,'');
		var hanZi=this.mb[start+i].replace(/[a-z';]+/,'');
		var tmpArr=hanZi.split(",");
		for(var j=0;j<tmpArr.length;j++){
			this._List.push([ziMu,tmpArr[j]]);
		}
		i++;
	}
}
//把List里的数据,前houXianNum个取出来.
InputEngine.prototype.setHouXianData=function(){
	this.index=1;//说明,每次时,都是显示第一页.
	this.houXianPg=Math.ceil(this._List.length/this.houXianNum);
		this.houXianData=[];
		var len=this._List.length;
		if(len<1){//说明为空
			this.houXianPg=0;
			this.houXianData=[];
		}else{
			//每次输入字母后,就立即得出有会多少页可显示
			var i=0;
			while(i<this.houXianNum&&i<len){
				this.houXianData.push(this._List[i++]);
			}
		}
}
InputEngine.prototype.preHouXianData=function(){
	var len=this._List.length;
	if(this.index>1){//说明至少会有前一项
		this.houXianData=[];
		var from=(this.index-1)*this.houXianNum;
		while(from<this.houXianNum*this.index	&&from<len){
			 this.houXianData.push(this._List[from++]);
		}
		this.index=this.index-1;
	}else{
		this.setHouXianData();
	}
}
InputEngine.prototype.nextHouXianData=function(){
	var len=this._List.length;
	if(this.index<this.houXianPg){//说明它有下一项.
		this.houXianData=[];
		var from=this.index*this.houXianNum;
		while(from<this.houXianNum*(this.index+1)&&from<len){
			 this.houXianData.push(this._List[from++]);
		}
		this.index=this.index+1;	
	}
}
InputEngine.prototype.selectHouXianZi=function(num){//数字用来表示,你想选择第几个汉字
	var num=Number(num);
	if(num==0){
		num=10;
	}
	if(this.houXianData.length>0&&num<11){//如果候选数据是空的，就返回一个“”字符
	return this.houXianData[num-1][1];
	}else{
		return "";
	}
}
InputEngine.prototype.clearHouXianData=function(){//让候选为空,不然,你的houXianData数据还在,你在操作这些数据时,重复运作时,会使用它的数据.事实,我们操作这些数据,只需要一次.为了防止意外的重复操作,把它清空了,这样就算再重复操作,也得不到这些数据了.
	this.inputChars="";
	this.houXianData=[];
	this.houXianPg=0;
	this.index=1;
}
//id为element对象
InputEngine.prototype.writeTo=function(id,myValue){
//myValue=myValue.toTraditionChars();
	if(this.isTradition)
	myValue=myValue.toTraditionChars();
	 if (document.selection) {
	   //For browsers like Internet Explorer
	   id.focus();
	  var sel = document.selection.createRange();
	   sel.text = myValue;
	   id.focus();
	 }
	 else if (id.selectionStart || id.selectionStart == '0') {
	   //For browsers like Firefox and Webkit based
	   var startPos = id.selectionStart;
	   var endPos = id.selectionEnd;
	   var scrollTop = id.scrollTop;
	   id.value = id.value.substring(0, startPos)+myValue+id.value.substring(endPos,id.value.length);
	   id.focus();
	   id.selectionStart = startPos + myValue.length;
	   id.selectionEnd = startPos + myValue.length;
	   id.scrollTop = scrollTop;
	 } else {
	   id.value += myValue;
	   id.focus();
	 }
}
/*

*/
//---------------------------------------------------------------
//一种输入法监听方案，如果不想依靠于jquery，你得另外使用更好的。可以使用kibo.js做不依靠于jquery的事件监听

var isLeftSingQuotation=true;//true表示左单引,false表示右单引.主要为了实现‘’，“”功能
var isLeftDoubleQuotation=true;
//下面的函数设置候选界面
	
	
//inputId是element对象,houxianId都是element对象	或jquery对象都可以
function showHouXian(ime,inputId,houxianId){
	$(houxianId).find("div").html(ime.inputChars+"<span style='float:right'>"+ime.index+"/"+ime.houXianPg+"</span>");
	var str="";
	for(var i=0;i<ime.houXianData.length;i++){
		str=str+(i+1)+"."+ime.houXianData[i][1]+ime.houXianData[i][0].replace(ime.inputChars,"")+" ";
	}
	$(houxianId).find("p").html(str);
	$(houxianId).css({display:"block"});
}
function hideHouXian(ime,houxianId){
	ime.clearHouXianData();
	$(houxianId).css({display:"none"});
}
//这个功能依赖于jquery的插件
//idelement对象;houxianId为jquery风格对象，或element对象都可以
//本函数的功能是：将监听id的keydown事件，然后将输入法得到的字符写到id里去。
InputEngine.prototype.setInputFunctionTo=function(id,houxianId){
	var self=this;
	/*
	$("#imexu").bind("keydown","anyPunctuation",function(){
		return false;
	});
	*/
	//下面的事件监听所有可能的打字运作，以及给出合适的结果。
	//var typeId=document.getElementById(id);
	$(id).bind("keydown","anyLetter",function(e){
		if(!self.isChinese){//如果是英文状态,那么,当你是全角时,就输出全角,如时是半角时,直接输出
			var char=e.keyName;
			if(self.isQuanJiaoChar){
				char=char.toDBC();//将字母换成了全角的
			}
			self.writeTo(id,char);
		}else{
			self.inputChars=self.inputChars+e.keyName;
			var range=self.findMbRang(self.inputChars);
			self.setList(range[0],range[1]);
			self.setHouXianData();
			if(self.houXianData.length==0){
				hideHouXian(self,houxianId);
			}else{
				showHouXian(self,id,houxianId);
				if(self.inputChars.length==self.maxLen){
					//alert(self.houXianData.length);
					if(self.houXianData.length<2){
						self.writeTo(id,self.selectHouXianZi(1));
						hideHouXian(self,houxianId);
					}
				}
			}
		}
		return false;
	}).bind("keydown","space enter",function(){
		if(self.inputChars){
		self.writeTo(id,self.selectHouXianZi(1));
		hideHouXian(self,houxianId);
		}else{
			return true;
		}
		return false;
	}).bind("keydown","anyNumber",function(e){//为何不能让, ;放到这里来做选项呢？原因是这里数字在inputChars为空时，想用来打数字，这样，必然使, ;要打出来，除非，你再做判断，不如就直接把这个判断放
		var num=e.keyName;
		if(self.inputChars==""){//如果不是用来选项的,那么:如果是半角的,直接输出,如果是全角的,输出全角
			if(self.isQuanJiaoChar){
				num=num.toDBC();//将数字换成了全角的
			}
			self.writeTo(id,num);
		}else{
			self.writeTo(id,self.selectHouXianZi(num));
			hideHouXian(self,houxianId);
		}
		return false
	}).bind("keydown","anyPunctuation",function(e){
		var char=e.keyName;
		//对于标点的处理原则：
		
		//如果是中文标点为真，无论是全角，还是半角，都只打出中文标点（因为中文标点不分全半，都是全）
		//如果是英文标点为真，如果是全角，输出全角，如果是半角，输出半角
	
		if(self.inputChars==""){//标点只在没有中文输入字符的情况下，才执行，当有中文输入字符，让标点执行其他功能
			if(self.isChinesePunctuation){//是中文标点,那一定打出中文标点
				char=char.toChinesePunctuation();
				if(char=="‘"){
					if(isLeftSingQuotation){//是左单引
						isLeftSingQuotation=false;
					}else{
						isLeftSingQuotation=true;
						char="’";
					}
				}
			}else{//英文标点,分为全角与半角
				if(self.isQuanJiaoChar){
					char=char.toDBC();
				}else{
				}
			}
		self.writeTo(id,char);
		}else if(char==";"){
			self.writeTo(id,self.selectHouXianZi(2));
			hideHouXian(self,houxianId);
		}else if(char=="'"){
			self.writeTo(id,self.selectHouXianZi(3));
			hideHouXian(self,houxianId);
		}
		return false;
	}).bind("keydown","shift+anyLetter",function(e){
		var char=e.keyName;
		char=char.toUpperCase();//变为大写的
		if(self.isChinese){
			char=char.toDBC();//是中文的话,就变成中文的大写ABCD
		}
		self.writeTo(id,char);
		return false;
	}).bind("keydown","shift+anyNumber",function(e){//用来输出数字上的标点
		var num=Number(e.keyName);
		var char=")!@#$%^&*(".charAt(num);//取得数字上的上档键英文字符
		if(self.isChinesePunctuation){//中文标点下,只输出中文标点,不管它的全角,半角
			char=char.toChinesePunctuation();
		}else{
			if(self.isQuanJiaoChar){//是英文标点,如果是全角输出全角
				char=char.toDBC();
			}
		}
		self.writeTo(id,char);
		return false;	
	}).bind("keydown","shift+anyPunctuation",function(e){
		var shiftPu={
			",":"<",
			".":">",
			"/":"?",
			";":":",
			"'":"\"",
			"[":"{",
			"]":"}",
			"\\":"|",
			"`":"~",
			"-":"_",
			"=":"+"
	};
	var char=shiftPu[e.keyName];
		if(self.inputChars==""){//如果不为空,说明正在输入中文呢,有些标点可以用来选汉字的,如:,.
			//取得上档键值
			if(self.isChinesePunctuation){//中文标点下,只输出中文标点,不管它的全角,半角
				char=char.toChinesePunctuation();
					if(char=="“"){
					if(isLeftDoubleQuotation){//是左双引
						isLeftDoubleQuotation=false;
					}else{
						isLeftDoubleQuotation=true;
						char="”";
					}
				}
			}else{
				if(self.isQuanJiaoChar){//是英文标点,如果是全角输出全角
					char=char.toDBC();
				}
			}
		}
		self.writeTo(id,char);
		return false;	
	}).bind("keydown","backspace",function(){
				if(self.inputChars.length>1){
			//要让backspace正确工作.有的删,就执行自定义的,没有的,就让它按默认的.
				self.inputChars=self.inputChars.substr(0,self.inputChars.length-1);
				var num=self.findMbRang(self.inputChars);
				self.setList(num[0],num[1]);
				self.setHouXianData();
				showHouXian(self,id,houxianId);
				return false;
			}else{
				hideHouXian(self,houxianId);
			return true;
			}
	}).bind("keydown","left pageUp ,",function(e){
		if(self.inputChars==""){//如果没有输入到中文引擎，就直接使用原来的功能
		return true;
		}
		self.preHouXianData();
		showHouXian(self,id,houxianId);
		return false
	}).bind("keydown","right pageDown .",function(){
		if(self.inputChars==""){
			return true;
		}
		self.nextHouXianData();
		showHouXian(self,id,houxianId);
		return false;
	});
}


/*
界面编程
注意使用ime.css。
它是对框架的总体界面表示

输入法的界面设计
如果要更改界面：
1。现更改CSS 
2。使用jquery更改一部分。



*/

$(function(){
	$(document).bind("contextmenu",function(){
	//输入法帮助文件显示
		$("#dialog-message").css({display:"block",height:window.innerHeight-30}).position({
			my:"center top",
			at:"center top",
			of:"body"
		}).find(".uiClose").click(function(){
			$("#dialog-message").fadeOut("slow");
		});
	return false;
	});
	$("body").bind("keydown","ctrl+h",function(){
		$("#dialog-message").css({display:"block",height:window.innerHeight-30}).position({
		my:"center top",
		at:"center top",
		of:"body"
		});
		return false;
	}).bind("keydown","ctrl+c",function(){
		$("#dialog-message").fadeOut("slow");
		return false;
	}).bind("keydown","ctrl+o ctrl+n ctrl+l ctrl",function(){
		return false;
	});
	
	var ime=new InputEngine(raw,5);
	//ime.setInputFunctionTo("imexu","ime-houxian");
	/*
	这个是用来设置，那些东西，得到了本输入设置的功能。
	*/
	
	$("textarea,input").each(function(index, element) {

        ime.setInputFunctionTo(element,$("#ime-houxian"));
		$(element).bind("keydown","ctrl+j",function(){//设置快捷键，这个切换输入法
		ime.isChinese=true;
		ime.isTradition=false;
		$("#ime-toolbar td:eq(1)").html("简");
			return false;
		}).bind("keydown","ctrl+f",function(){//设置快捷键，这个切换输入法
			ime.isChinese=true;
			ime.isTradition=true;
			$("#ime-toolbar td:eq(1)").html("繁");
			return false;
		}).bind("keydown","ctrl+e",function(){
			ime.isChinese=false;
			$("#ime-toolbar td:eq(1)").html("英");
			return false;
		}).bind("keydown","ctrl+q",function(){
			ime.isQuanJiaoChar=true;
			$("#ime-toolbar td:eq(2)").html("◭");
			return false;
		}).bind("keydown","ctrl+b",function(){
			ime.isQuanJiaoChar=false;
			$("#ime-toolbar td:eq(2)").html("▲");
			return false;
		}).bind("keydown","ctrl+p",function(){
			ime.isChinesePunctuation=!ime.isChinesePunctuation;
			if(ime.isChinesePunctuation){
				$("#ime-toolbar td:eq(2)").html("》");
			}else{
				$("#ime-toolbar td:eq(2)").html("＞");
			}
			return false;
		});
    });

	
	//自动去设置toolbar里的数值
	//这是是初绐化时设置的位置
	//当你想在某个里面输入时，toolbar会自动跟到那个对象上去
	if(ime.isChinesePunctuation)
	$("#ime-toolbar td:eq(3)").html("》");
	else
	$("#ime-toolbar td:eq(3)").html("＞");
	if(ime.isQuanJiaoChar)
	$("#ime-toolbar td:eq(2)").html("◭");
	else
	$("#ime-toolbar td:eq(2)").html("▲");
	
	var srf=["简","繁","英"];
	var whichSrf=0;
	if(ime.isChinese){
		if(ime.isTradition){
			$("#ime-toolbar td:eq(1)").html("繁");
		}else{
			$("#ime-toolbar td:eq(1)").html("简");
		}
	}else{
		$("#ime-toolbar td:eq(1)").html("英");
	}
	
	//让候选框有了可移动功能
//----------------------------------
//用来设置toolbar的移动，状态的改变
//------------------------	
	$("#ime-toolbar td:eq(0)").mouseenter(function(e) {
        $(this).css({color:"red"});
    }).mouseleave(function(e) {
        $(this).css({color:"black"});
    });
	$("#ime-toolbar td:eq(1)").click(function(){
		if(whichSrf==2){
			whichSrf=0;
		}else{
			whichSrf++;
		}
		switch(whichSrf){
			case 0:{
				ime.isChinese=true;
				ime.isTradition=false;break;
			}
			case 1:{
				ime.isChinese=true;
				ime.isTradition=true;break;
			}
			case 2:{
				ime.isChinese=false
			}
		}
		$(this).html(srf[whichSrf]);
	});
	$("#ime-toolbar td:eq(2)").click(function(){
		var str=$(this).html();
		if(ime.isQuanJiaoChar){
			ime.isQuanJiaoChar=false;
			str="▲";
		}else{
			ime.isQuanJiaoChar=true;
			str="◭";
		}
		$(this).html(str);
	});
	$("#ime-toolbar td:eq(3)").click(function(){
		var str=$(this).html();
		if(ime.isChinesePunctuation){
			ime.isChinesePunctuation=false;
			str="＞";
		}else{
			ime.isChinesePunctuation=true;
			str="》";
		}
		$(this).html(str);
	});	
});
