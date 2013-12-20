function autoResizeDiv(){
    var m_height = $("#main").height(); 
    if (m_height < window.innerHeight){
        $("#main").css("height", window.innerHeight + 'px');
        $("#main_body").css("height", (window.innerHeight - 270) + 'px');
    }
}

$(document).ready(function(){
	$("h4").wrapInner(function(){
		return "<a href='#" + $(this).attr('id') + "'></a>";
	});
	autoResizeDiv();
});

$(document).resize(function(){
	autoResizeDiv();
});