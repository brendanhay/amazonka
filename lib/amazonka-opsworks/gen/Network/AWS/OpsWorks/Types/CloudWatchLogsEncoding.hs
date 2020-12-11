-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
  ( CloudWatchLogsEncoding
      ( CloudWatchLogsEncoding',
        Ascii,
        BIG5,
        Big5hkscs,
        CP037,
        CP1006,
        CP1026,
        CP1140,
        CP1250,
        CP1251,
        CP1252,
        CP1253,
        CP1254,
        CP1255,
        CP1256,
        CP1257,
        CP1258,
        CP424,
        CP437,
        CP500,
        CP720,
        CP737,
        CP775,
        CP850,
        CP852,
        CP855,
        CP856,
        CP857,
        CP858,
        CP860,
        CP861,
        CP862,
        CP863,
        CP864,
        CP865,
        CP866,
        CP869,
        CP874,
        CP875,
        CP932,
        CP949,
        CP950,
        EucJISX0213,
        EucJis2004,
        EucJp,
        EucKr,
        GB18030,
        GB2312,
        Gbk,
        HZ,
        ISO2022Jp,
        ISO2022Jp1,
        ISO2022Jp2,
        ISO2022Jp2004,
        ISO2022Jp3,
        ISO2022JpExt,
        ISO2022Kr,
        ISO885910,
        ISO885913,
        ISO885914,
        ISO885915,
        ISO885916,
        ISO88592,
        ISO88593,
        ISO88594,
        ISO88595,
        ISO88596,
        ISO88597,
        ISO88598,
        ISO88599,
        Johab,
        KOI8r,
        KOI8u,
        Latin1,
        MACCyrillic,
        MACGreek,
        MACIceland,
        MACLATIN2,
        MACRoman,
        MACTurkish,
        PTCP154,
        ShiftJISX0213,
        ShiftJis,
        ShiftJis2004,
        Utf16,
        Utf16Be,
        Utf16Le,
        Utf32,
        Utf32Be,
        Utf32Le,
        Utf7,
        Utf8,
        Utf8Sig
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
newtype CloudWatchLogsEncoding = CloudWatchLogsEncoding' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Ascii :: CloudWatchLogsEncoding
pattern Ascii = CloudWatchLogsEncoding' "ascii"

pattern BIG5 :: CloudWatchLogsEncoding
pattern BIG5 = CloudWatchLogsEncoding' "big5"

pattern Big5hkscs :: CloudWatchLogsEncoding
pattern Big5hkscs = CloudWatchLogsEncoding' "big5hkscs"

pattern CP037 :: CloudWatchLogsEncoding
pattern CP037 = CloudWatchLogsEncoding' "cp037"

pattern CP1006 :: CloudWatchLogsEncoding
pattern CP1006 = CloudWatchLogsEncoding' "cp1006"

pattern CP1026 :: CloudWatchLogsEncoding
pattern CP1026 = CloudWatchLogsEncoding' "cp1026"

pattern CP1140 :: CloudWatchLogsEncoding
pattern CP1140 = CloudWatchLogsEncoding' "cp1140"

pattern CP1250 :: CloudWatchLogsEncoding
pattern CP1250 = CloudWatchLogsEncoding' "cp1250"

pattern CP1251 :: CloudWatchLogsEncoding
pattern CP1251 = CloudWatchLogsEncoding' "cp1251"

pattern CP1252 :: CloudWatchLogsEncoding
pattern CP1252 = CloudWatchLogsEncoding' "cp1252"

pattern CP1253 :: CloudWatchLogsEncoding
pattern CP1253 = CloudWatchLogsEncoding' "cp1253"

pattern CP1254 :: CloudWatchLogsEncoding
pattern CP1254 = CloudWatchLogsEncoding' "cp1254"

pattern CP1255 :: CloudWatchLogsEncoding
pattern CP1255 = CloudWatchLogsEncoding' "cp1255"

pattern CP1256 :: CloudWatchLogsEncoding
pattern CP1256 = CloudWatchLogsEncoding' "cp1256"

pattern CP1257 :: CloudWatchLogsEncoding
pattern CP1257 = CloudWatchLogsEncoding' "cp1257"

pattern CP1258 :: CloudWatchLogsEncoding
pattern CP1258 = CloudWatchLogsEncoding' "cp1258"

pattern CP424 :: CloudWatchLogsEncoding
pattern CP424 = CloudWatchLogsEncoding' "cp424"

pattern CP437 :: CloudWatchLogsEncoding
pattern CP437 = CloudWatchLogsEncoding' "cp437"

pattern CP500 :: CloudWatchLogsEncoding
pattern CP500 = CloudWatchLogsEncoding' "cp500"

pattern CP720 :: CloudWatchLogsEncoding
pattern CP720 = CloudWatchLogsEncoding' "cp720"

pattern CP737 :: CloudWatchLogsEncoding
pattern CP737 = CloudWatchLogsEncoding' "cp737"

pattern CP775 :: CloudWatchLogsEncoding
pattern CP775 = CloudWatchLogsEncoding' "cp775"

pattern CP850 :: CloudWatchLogsEncoding
pattern CP850 = CloudWatchLogsEncoding' "cp850"

pattern CP852 :: CloudWatchLogsEncoding
pattern CP852 = CloudWatchLogsEncoding' "cp852"

pattern CP855 :: CloudWatchLogsEncoding
pattern CP855 = CloudWatchLogsEncoding' "cp855"

pattern CP856 :: CloudWatchLogsEncoding
pattern CP856 = CloudWatchLogsEncoding' "cp856"

pattern CP857 :: CloudWatchLogsEncoding
pattern CP857 = CloudWatchLogsEncoding' "cp857"

pattern CP858 :: CloudWatchLogsEncoding
pattern CP858 = CloudWatchLogsEncoding' "cp858"

pattern CP860 :: CloudWatchLogsEncoding
pattern CP860 = CloudWatchLogsEncoding' "cp860"

pattern CP861 :: CloudWatchLogsEncoding
pattern CP861 = CloudWatchLogsEncoding' "cp861"

pattern CP862 :: CloudWatchLogsEncoding
pattern CP862 = CloudWatchLogsEncoding' "cp862"

pattern CP863 :: CloudWatchLogsEncoding
pattern CP863 = CloudWatchLogsEncoding' "cp863"

pattern CP864 :: CloudWatchLogsEncoding
pattern CP864 = CloudWatchLogsEncoding' "cp864"

pattern CP865 :: CloudWatchLogsEncoding
pattern CP865 = CloudWatchLogsEncoding' "cp865"

pattern CP866 :: CloudWatchLogsEncoding
pattern CP866 = CloudWatchLogsEncoding' "cp866"

pattern CP869 :: CloudWatchLogsEncoding
pattern CP869 = CloudWatchLogsEncoding' "cp869"

pattern CP874 :: CloudWatchLogsEncoding
pattern CP874 = CloudWatchLogsEncoding' "cp874"

pattern CP875 :: CloudWatchLogsEncoding
pattern CP875 = CloudWatchLogsEncoding' "cp875"

pattern CP932 :: CloudWatchLogsEncoding
pattern CP932 = CloudWatchLogsEncoding' "cp932"

pattern CP949 :: CloudWatchLogsEncoding
pattern CP949 = CloudWatchLogsEncoding' "cp949"

pattern CP950 :: CloudWatchLogsEncoding
pattern CP950 = CloudWatchLogsEncoding' "cp950"

pattern EucJISX0213 :: CloudWatchLogsEncoding
pattern EucJISX0213 = CloudWatchLogsEncoding' "euc_jisx0213"

pattern EucJis2004 :: CloudWatchLogsEncoding
pattern EucJis2004 = CloudWatchLogsEncoding' "euc_jis_2004"

pattern EucJp :: CloudWatchLogsEncoding
pattern EucJp = CloudWatchLogsEncoding' "euc_jp"

pattern EucKr :: CloudWatchLogsEncoding
pattern EucKr = CloudWatchLogsEncoding' "euc_kr"

pattern GB18030 :: CloudWatchLogsEncoding
pattern GB18030 = CloudWatchLogsEncoding' "gb18030"

pattern GB2312 :: CloudWatchLogsEncoding
pattern GB2312 = CloudWatchLogsEncoding' "gb2312"

pattern Gbk :: CloudWatchLogsEncoding
pattern Gbk = CloudWatchLogsEncoding' "gbk"

pattern HZ :: CloudWatchLogsEncoding
pattern HZ = CloudWatchLogsEncoding' "hz"

pattern ISO2022Jp :: CloudWatchLogsEncoding
pattern ISO2022Jp = CloudWatchLogsEncoding' "iso2022_jp"

pattern ISO2022Jp1 :: CloudWatchLogsEncoding
pattern ISO2022Jp1 = CloudWatchLogsEncoding' "iso2022_jp_1"

pattern ISO2022Jp2 :: CloudWatchLogsEncoding
pattern ISO2022Jp2 = CloudWatchLogsEncoding' "iso2022_jp_2"

pattern ISO2022Jp2004 :: CloudWatchLogsEncoding
pattern ISO2022Jp2004 = CloudWatchLogsEncoding' "iso2022_jp_2004"

pattern ISO2022Jp3 :: CloudWatchLogsEncoding
pattern ISO2022Jp3 = CloudWatchLogsEncoding' "iso2022_jp_3"

pattern ISO2022JpExt :: CloudWatchLogsEncoding
pattern ISO2022JpExt = CloudWatchLogsEncoding' "iso2022_jp_ext"

pattern ISO2022Kr :: CloudWatchLogsEncoding
pattern ISO2022Kr = CloudWatchLogsEncoding' "iso2022_kr"

pattern ISO885910 :: CloudWatchLogsEncoding
pattern ISO885910 = CloudWatchLogsEncoding' "iso8859_10"

pattern ISO885913 :: CloudWatchLogsEncoding
pattern ISO885913 = CloudWatchLogsEncoding' "iso8859_13"

pattern ISO885914 :: CloudWatchLogsEncoding
pattern ISO885914 = CloudWatchLogsEncoding' "iso8859_14"

pattern ISO885915 :: CloudWatchLogsEncoding
pattern ISO885915 = CloudWatchLogsEncoding' "iso8859_15"

pattern ISO885916 :: CloudWatchLogsEncoding
pattern ISO885916 = CloudWatchLogsEncoding' "iso8859_16"

pattern ISO88592 :: CloudWatchLogsEncoding
pattern ISO88592 = CloudWatchLogsEncoding' "iso8859_2"

pattern ISO88593 :: CloudWatchLogsEncoding
pattern ISO88593 = CloudWatchLogsEncoding' "iso8859_3"

pattern ISO88594 :: CloudWatchLogsEncoding
pattern ISO88594 = CloudWatchLogsEncoding' "iso8859_4"

pattern ISO88595 :: CloudWatchLogsEncoding
pattern ISO88595 = CloudWatchLogsEncoding' "iso8859_5"

pattern ISO88596 :: CloudWatchLogsEncoding
pattern ISO88596 = CloudWatchLogsEncoding' "iso8859_6"

pattern ISO88597 :: CloudWatchLogsEncoding
pattern ISO88597 = CloudWatchLogsEncoding' "iso8859_7"

pattern ISO88598 :: CloudWatchLogsEncoding
pattern ISO88598 = CloudWatchLogsEncoding' "iso8859_8"

pattern ISO88599 :: CloudWatchLogsEncoding
pattern ISO88599 = CloudWatchLogsEncoding' "iso8859_9"

pattern Johab :: CloudWatchLogsEncoding
pattern Johab = CloudWatchLogsEncoding' "johab"

pattern KOI8r :: CloudWatchLogsEncoding
pattern KOI8r = CloudWatchLogsEncoding' "koi8_r"

pattern KOI8u :: CloudWatchLogsEncoding
pattern KOI8u = CloudWatchLogsEncoding' "koi8_u"

pattern Latin1 :: CloudWatchLogsEncoding
pattern Latin1 = CloudWatchLogsEncoding' "latin_1"

pattern MACCyrillic :: CloudWatchLogsEncoding
pattern MACCyrillic = CloudWatchLogsEncoding' "mac_cyrillic"

pattern MACGreek :: CloudWatchLogsEncoding
pattern MACGreek = CloudWatchLogsEncoding' "mac_greek"

pattern MACIceland :: CloudWatchLogsEncoding
pattern MACIceland = CloudWatchLogsEncoding' "mac_iceland"

pattern MACLATIN2 :: CloudWatchLogsEncoding
pattern MACLATIN2 = CloudWatchLogsEncoding' "mac_latin2"

pattern MACRoman :: CloudWatchLogsEncoding
pattern MACRoman = CloudWatchLogsEncoding' "mac_roman"

pattern MACTurkish :: CloudWatchLogsEncoding
pattern MACTurkish = CloudWatchLogsEncoding' "mac_turkish"

pattern PTCP154 :: CloudWatchLogsEncoding
pattern PTCP154 = CloudWatchLogsEncoding' "ptcp154"

pattern ShiftJISX0213 :: CloudWatchLogsEncoding
pattern ShiftJISX0213 = CloudWatchLogsEncoding' "shift_jisx0213"

pattern ShiftJis :: CloudWatchLogsEncoding
pattern ShiftJis = CloudWatchLogsEncoding' "shift_jis"

pattern ShiftJis2004 :: CloudWatchLogsEncoding
pattern ShiftJis2004 = CloudWatchLogsEncoding' "shift_jis_2004"

pattern Utf16 :: CloudWatchLogsEncoding
pattern Utf16 = CloudWatchLogsEncoding' "utf_16"

pattern Utf16Be :: CloudWatchLogsEncoding
pattern Utf16Be = CloudWatchLogsEncoding' "utf_16_be"

pattern Utf16Le :: CloudWatchLogsEncoding
pattern Utf16Le = CloudWatchLogsEncoding' "utf_16_le"

pattern Utf32 :: CloudWatchLogsEncoding
pattern Utf32 = CloudWatchLogsEncoding' "utf_32"

pattern Utf32Be :: CloudWatchLogsEncoding
pattern Utf32Be = CloudWatchLogsEncoding' "utf_32_be"

pattern Utf32Le :: CloudWatchLogsEncoding
pattern Utf32Le = CloudWatchLogsEncoding' "utf_32_le"

pattern Utf7 :: CloudWatchLogsEncoding
pattern Utf7 = CloudWatchLogsEncoding' "utf_7"

pattern Utf8 :: CloudWatchLogsEncoding
pattern Utf8 = CloudWatchLogsEncoding' "utf_8"

pattern Utf8Sig :: CloudWatchLogsEncoding
pattern Utf8Sig = CloudWatchLogsEncoding' "utf_8_sig"

{-# COMPLETE
  Ascii,
  BIG5,
  Big5hkscs,
  CP037,
  CP1006,
  CP1026,
  CP1140,
  CP1250,
  CP1251,
  CP1252,
  CP1253,
  CP1254,
  CP1255,
  CP1256,
  CP1257,
  CP1258,
  CP424,
  CP437,
  CP500,
  CP720,
  CP737,
  CP775,
  CP850,
  CP852,
  CP855,
  CP856,
  CP857,
  CP858,
  CP860,
  CP861,
  CP862,
  CP863,
  CP864,
  CP865,
  CP866,
  CP869,
  CP874,
  CP875,
  CP932,
  CP949,
  CP950,
  EucJISX0213,
  EucJis2004,
  EucJp,
  EucKr,
  GB18030,
  GB2312,
  Gbk,
  HZ,
  ISO2022Jp,
  ISO2022Jp1,
  ISO2022Jp2,
  ISO2022Jp2004,
  ISO2022Jp3,
  ISO2022JpExt,
  ISO2022Kr,
  ISO885910,
  ISO885913,
  ISO885914,
  ISO885915,
  ISO885916,
  ISO88592,
  ISO88593,
  ISO88594,
  ISO88595,
  ISO88596,
  ISO88597,
  ISO88598,
  ISO88599,
  Johab,
  KOI8r,
  KOI8u,
  Latin1,
  MACCyrillic,
  MACGreek,
  MACIceland,
  MACLATIN2,
  MACRoman,
  MACTurkish,
  PTCP154,
  ShiftJISX0213,
  ShiftJis,
  ShiftJis2004,
  Utf16,
  Utf16Be,
  Utf16Le,
  Utf32,
  Utf32Be,
  Utf32Le,
  Utf7,
  Utf8,
  Utf8Sig,
  CloudWatchLogsEncoding'
  #-}
