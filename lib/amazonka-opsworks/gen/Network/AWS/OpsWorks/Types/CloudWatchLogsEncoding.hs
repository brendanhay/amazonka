{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding where

import Network.AWS.Prelude

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
data CloudWatchLogsEncoding
  = Ascii
  | BIG5
  | Big5hkscs
  | CP037
  | CP1006
  | CP1026
  | CP1140
  | CP1250
  | CP1251
  | CP1252
  | CP1253
  | CP1254
  | CP1255
  | CP1256
  | CP1257
  | CP1258
  | CP424
  | CP437
  | CP500
  | CP720
  | CP737
  | CP775
  | CP850
  | CP852
  | CP855
  | CP856
  | CP857
  | CP858
  | CP860
  | CP861
  | CP862
  | CP863
  | CP864
  | CP865
  | CP866
  | CP869
  | CP874
  | CP875
  | CP932
  | CP949
  | CP950
  | EucJISX0213
  | EucJis2004
  | EucJp
  | EucKr
  | GB18030
  | GB2312
  | Gbk
  | HZ
  | ISO2022Jp
  | ISO2022Jp1
  | ISO2022Jp2
  | ISO2022Jp2004
  | ISO2022Jp3
  | ISO2022JpExt
  | ISO2022Kr
  | ISO885910
  | ISO885913
  | ISO885914
  | ISO885915
  | ISO885916
  | ISO88592
  | ISO88593
  | ISO88594
  | ISO88595
  | ISO88596
  | ISO88597
  | ISO88598
  | ISO88599
  | Johab
  | KOI8r
  | KOI8u
  | Latin1
  | MACCyrillic
  | MACGreek
  | MACIceland
  | MACLATIN2
  | MACRoman
  | MACTurkish
  | PTCP154
  | ShiftJISX0213
  | ShiftJis
  | ShiftJis2004
  | Utf16
  | Utf16Be
  | Utf16Le
  | Utf32
  | Utf32Be
  | Utf32Le
  | Utf7
  | Utf8
  | Utf8Sig
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CloudWatchLogsEncoding where
  parser =
    takeLowerText >>= \case
      "ascii" -> pure Ascii
      "big5" -> pure BIG5
      "big5hkscs" -> pure Big5hkscs
      "cp037" -> pure CP037
      "cp1006" -> pure CP1006
      "cp1026" -> pure CP1026
      "cp1140" -> pure CP1140
      "cp1250" -> pure CP1250
      "cp1251" -> pure CP1251
      "cp1252" -> pure CP1252
      "cp1253" -> pure CP1253
      "cp1254" -> pure CP1254
      "cp1255" -> pure CP1255
      "cp1256" -> pure CP1256
      "cp1257" -> pure CP1257
      "cp1258" -> pure CP1258
      "cp424" -> pure CP424
      "cp437" -> pure CP437
      "cp500" -> pure CP500
      "cp720" -> pure CP720
      "cp737" -> pure CP737
      "cp775" -> pure CP775
      "cp850" -> pure CP850
      "cp852" -> pure CP852
      "cp855" -> pure CP855
      "cp856" -> pure CP856
      "cp857" -> pure CP857
      "cp858" -> pure CP858
      "cp860" -> pure CP860
      "cp861" -> pure CP861
      "cp862" -> pure CP862
      "cp863" -> pure CP863
      "cp864" -> pure CP864
      "cp865" -> pure CP865
      "cp866" -> pure CP866
      "cp869" -> pure CP869
      "cp874" -> pure CP874
      "cp875" -> pure CP875
      "cp932" -> pure CP932
      "cp949" -> pure CP949
      "cp950" -> pure CP950
      "euc_jisx0213" -> pure EucJISX0213
      "euc_jis_2004" -> pure EucJis2004
      "euc_jp" -> pure EucJp
      "euc_kr" -> pure EucKr
      "gb18030" -> pure GB18030
      "gb2312" -> pure GB2312
      "gbk" -> pure Gbk
      "hz" -> pure HZ
      "iso2022_jp" -> pure ISO2022Jp
      "iso2022_jp_1" -> pure ISO2022Jp1
      "iso2022_jp_2" -> pure ISO2022Jp2
      "iso2022_jp_2004" -> pure ISO2022Jp2004
      "iso2022_jp_3" -> pure ISO2022Jp3
      "iso2022_jp_ext" -> pure ISO2022JpExt
      "iso2022_kr" -> pure ISO2022Kr
      "iso8859_10" -> pure ISO885910
      "iso8859_13" -> pure ISO885913
      "iso8859_14" -> pure ISO885914
      "iso8859_15" -> pure ISO885915
      "iso8859_16" -> pure ISO885916
      "iso8859_2" -> pure ISO88592
      "iso8859_3" -> pure ISO88593
      "iso8859_4" -> pure ISO88594
      "iso8859_5" -> pure ISO88595
      "iso8859_6" -> pure ISO88596
      "iso8859_7" -> pure ISO88597
      "iso8859_8" -> pure ISO88598
      "iso8859_9" -> pure ISO88599
      "johab" -> pure Johab
      "koi8_r" -> pure KOI8r
      "koi8_u" -> pure KOI8u
      "latin_1" -> pure Latin1
      "mac_cyrillic" -> pure MACCyrillic
      "mac_greek" -> pure MACGreek
      "mac_iceland" -> pure MACIceland
      "mac_latin2" -> pure MACLATIN2
      "mac_roman" -> pure MACRoman
      "mac_turkish" -> pure MACTurkish
      "ptcp154" -> pure PTCP154
      "shift_jisx0213" -> pure ShiftJISX0213
      "shift_jis" -> pure ShiftJis
      "shift_jis_2004" -> pure ShiftJis2004
      "utf_16" -> pure Utf16
      "utf_16_be" -> pure Utf16Be
      "utf_16_le" -> pure Utf16Le
      "utf_32" -> pure Utf32
      "utf_32_be" -> pure Utf32Be
      "utf_32_le" -> pure Utf32Le
      "utf_7" -> pure Utf7
      "utf_8" -> pure Utf8
      "utf_8_sig" -> pure Utf8Sig
      e ->
        fromTextError $
          "Failure parsing CloudWatchLogsEncoding from value: '" <> e
            <> "'. Accepted values: ascii, big5, big5hkscs, cp037, cp1006, cp1026, cp1140, cp1250, cp1251, cp1252, cp1253, cp1254, cp1255, cp1256, cp1257, cp1258, cp424, cp437, cp500, cp720, cp737, cp775, cp850, cp852, cp855, cp856, cp857, cp858, cp860, cp861, cp862, cp863, cp864, cp865, cp866, cp869, cp874, cp875, cp932, cp949, cp950, euc_jisx0213, euc_jis_2004, euc_jp, euc_kr, gb18030, gb2312, gbk, hz, iso2022_jp, iso2022_jp_1, iso2022_jp_2, iso2022_jp_2004, iso2022_jp_3, iso2022_jp_ext, iso2022_kr, iso8859_10, iso8859_13, iso8859_14, iso8859_15, iso8859_16, iso8859_2, iso8859_3, iso8859_4, iso8859_5, iso8859_6, iso8859_7, iso8859_8, iso8859_9, johab, koi8_r, koi8_u, latin_1, mac_cyrillic, mac_greek, mac_iceland, mac_latin2, mac_roman, mac_turkish, ptcp154, shift_jisx0213, shift_jis, shift_jis_2004, utf_16, utf_16_be, utf_16_le, utf_32, utf_32_be, utf_32_le, utf_7, utf_8, utf_8_sig"

instance ToText CloudWatchLogsEncoding where
  toText = \case
    Ascii -> "ascii"
    BIG5 -> "big5"
    Big5hkscs -> "big5hkscs"
    CP037 -> "cp037"
    CP1006 -> "cp1006"
    CP1026 -> "cp1026"
    CP1140 -> "cp1140"
    CP1250 -> "cp1250"
    CP1251 -> "cp1251"
    CP1252 -> "cp1252"
    CP1253 -> "cp1253"
    CP1254 -> "cp1254"
    CP1255 -> "cp1255"
    CP1256 -> "cp1256"
    CP1257 -> "cp1257"
    CP1258 -> "cp1258"
    CP424 -> "cp424"
    CP437 -> "cp437"
    CP500 -> "cp500"
    CP720 -> "cp720"
    CP737 -> "cp737"
    CP775 -> "cp775"
    CP850 -> "cp850"
    CP852 -> "cp852"
    CP855 -> "cp855"
    CP856 -> "cp856"
    CP857 -> "cp857"
    CP858 -> "cp858"
    CP860 -> "cp860"
    CP861 -> "cp861"
    CP862 -> "cp862"
    CP863 -> "cp863"
    CP864 -> "cp864"
    CP865 -> "cp865"
    CP866 -> "cp866"
    CP869 -> "cp869"
    CP874 -> "cp874"
    CP875 -> "cp875"
    CP932 -> "cp932"
    CP949 -> "cp949"
    CP950 -> "cp950"
    EucJISX0213 -> "euc_jisx0213"
    EucJis2004 -> "euc_jis_2004"
    EucJp -> "euc_jp"
    EucKr -> "euc_kr"
    GB18030 -> "gb18030"
    GB2312 -> "gb2312"
    Gbk -> "gbk"
    HZ -> "hz"
    ISO2022Jp -> "iso2022_jp"
    ISO2022Jp1 -> "iso2022_jp_1"
    ISO2022Jp2 -> "iso2022_jp_2"
    ISO2022Jp2004 -> "iso2022_jp_2004"
    ISO2022Jp3 -> "iso2022_jp_3"
    ISO2022JpExt -> "iso2022_jp_ext"
    ISO2022Kr -> "iso2022_kr"
    ISO885910 -> "iso8859_10"
    ISO885913 -> "iso8859_13"
    ISO885914 -> "iso8859_14"
    ISO885915 -> "iso8859_15"
    ISO885916 -> "iso8859_16"
    ISO88592 -> "iso8859_2"
    ISO88593 -> "iso8859_3"
    ISO88594 -> "iso8859_4"
    ISO88595 -> "iso8859_5"
    ISO88596 -> "iso8859_6"
    ISO88597 -> "iso8859_7"
    ISO88598 -> "iso8859_8"
    ISO88599 -> "iso8859_9"
    Johab -> "johab"
    KOI8r -> "koi8_r"
    KOI8u -> "koi8_u"
    Latin1 -> "latin_1"
    MACCyrillic -> "mac_cyrillic"
    MACGreek -> "mac_greek"
    MACIceland -> "mac_iceland"
    MACLATIN2 -> "mac_latin2"
    MACRoman -> "mac_roman"
    MACTurkish -> "mac_turkish"
    PTCP154 -> "ptcp154"
    ShiftJISX0213 -> "shift_jisx0213"
    ShiftJis -> "shift_jis"
    ShiftJis2004 -> "shift_jis_2004"
    Utf16 -> "utf_16"
    Utf16Be -> "utf_16_be"
    Utf16Le -> "utf_16_le"
    Utf32 -> "utf_32"
    Utf32Be -> "utf_32_be"
    Utf32Le -> "utf_32_le"
    Utf7 -> "utf_7"
    Utf8 -> "utf_8"
    Utf8Sig -> "utf_8_sig"

instance Hashable CloudWatchLogsEncoding

instance NFData CloudWatchLogsEncoding

instance ToByteString CloudWatchLogsEncoding

instance ToQuery CloudWatchLogsEncoding

instance ToHeader CloudWatchLogsEncoding

instance ToJSON CloudWatchLogsEncoding where
  toJSON = toJSONText

instance FromJSON CloudWatchLogsEncoding where
  parseJSON = parseJSONText "CloudWatchLogsEncoding"
