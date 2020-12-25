{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CloudWatchLogsEncodingAscii,
        CloudWatchLogsEncodingBIG5,
        CloudWatchLogsEncodingBig5hkscs,
        CloudWatchLogsEncodingCP037,
        CloudWatchLogsEncodingCP424,
        CloudWatchLogsEncodingCP437,
        CloudWatchLogsEncodingCP500,
        CloudWatchLogsEncodingCP720,
        CloudWatchLogsEncodingCP737,
        CloudWatchLogsEncodingCP775,
        CloudWatchLogsEncodingCP850,
        CloudWatchLogsEncodingCP852,
        CloudWatchLogsEncodingCP855,
        CloudWatchLogsEncodingCP856,
        CloudWatchLogsEncodingCP857,
        CloudWatchLogsEncodingCP858,
        CloudWatchLogsEncodingCP860,
        CloudWatchLogsEncodingCP861,
        CloudWatchLogsEncodingCP862,
        CloudWatchLogsEncodingCP863,
        CloudWatchLogsEncodingCP864,
        CloudWatchLogsEncodingCP865,
        CloudWatchLogsEncodingCP866,
        CloudWatchLogsEncodingCP869,
        CloudWatchLogsEncodingCP874,
        CloudWatchLogsEncodingCP875,
        CloudWatchLogsEncodingCP932,
        CloudWatchLogsEncodingCP949,
        CloudWatchLogsEncodingCP950,
        CloudWatchLogsEncodingCP1006,
        CloudWatchLogsEncodingCP1026,
        CloudWatchLogsEncodingCP1140,
        CloudWatchLogsEncodingCP1250,
        CloudWatchLogsEncodingCP1251,
        CloudWatchLogsEncodingCP1252,
        CloudWatchLogsEncodingCP1253,
        CloudWatchLogsEncodingCP1254,
        CloudWatchLogsEncodingCP1255,
        CloudWatchLogsEncodingCP1256,
        CloudWatchLogsEncodingCP1257,
        CloudWatchLogsEncodingCP1258,
        CloudWatchLogsEncodingEucJp,
        CloudWatchLogsEncodingEucJis2004,
        CloudWatchLogsEncodingEucJISX0213,
        CloudWatchLogsEncodingEucKr,
        CloudWatchLogsEncodingGB2312,
        CloudWatchLogsEncodingGbk,
        CloudWatchLogsEncodingGB18030,
        CloudWatchLogsEncodingHZ,
        CloudWatchLogsEncodingISO2022Jp,
        CloudWatchLogsEncodingISO2022Jp1,
        CloudWatchLogsEncodingISO2022Jp2,
        CloudWatchLogsEncodingISO2022Jp2004,
        CloudWatchLogsEncodingISO2022Jp3,
        CloudWatchLogsEncodingISO2022JpExt,
        CloudWatchLogsEncodingISO2022Kr,
        CloudWatchLogsEncodingLatin1,
        CloudWatchLogsEncodingISO88592,
        CloudWatchLogsEncodingISO88593,
        CloudWatchLogsEncodingISO88594,
        CloudWatchLogsEncodingISO88595,
        CloudWatchLogsEncodingISO88596,
        CloudWatchLogsEncodingISO88597,
        CloudWatchLogsEncodingISO88598,
        CloudWatchLogsEncodingISO88599,
        CloudWatchLogsEncodingISO885910,
        CloudWatchLogsEncodingISO885913,
        CloudWatchLogsEncodingISO885914,
        CloudWatchLogsEncodingISO885915,
        CloudWatchLogsEncodingISO885916,
        CloudWatchLogsEncodingJohab,
        CloudWatchLogsEncodingKOI8r,
        CloudWatchLogsEncodingKOI8u,
        CloudWatchLogsEncodingMacCyrillic,
        CloudWatchLogsEncodingMacGreek,
        CloudWatchLogsEncodingMacIceland,
        CloudWatchLogsEncodingMacLATIN2,
        CloudWatchLogsEncodingMacRoman,
        CloudWatchLogsEncodingMacTurkish,
        CloudWatchLogsEncodingPTCP154,
        CloudWatchLogsEncodingShiftJis,
        CloudWatchLogsEncodingShiftJis2004,
        CloudWatchLogsEncodingShiftJISX0213,
        CloudWatchLogsEncodingUtf32,
        CloudWatchLogsEncodingUtf32Be,
        CloudWatchLogsEncodingUtf32Le,
        CloudWatchLogsEncodingUtf16,
        CloudWatchLogsEncodingUtf16Be,
        CloudWatchLogsEncodingUtf16Le,
        CloudWatchLogsEncodingUtf7,
        CloudWatchLogsEncodingUtf8,
        CloudWatchLogsEncodingUtf8Sig,
        fromCloudWatchLogsEncoding
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
newtype CloudWatchLogsEncoding = CloudWatchLogsEncoding'
  { fromCloudWatchLogsEncoding ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CloudWatchLogsEncodingAscii :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingAscii = CloudWatchLogsEncoding' "ascii"

pattern CloudWatchLogsEncodingBIG5 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingBIG5 = CloudWatchLogsEncoding' "big5"

pattern CloudWatchLogsEncodingBig5hkscs :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingBig5hkscs = CloudWatchLogsEncoding' "big5hkscs"

pattern CloudWatchLogsEncodingCP037 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP037 = CloudWatchLogsEncoding' "cp037"

pattern CloudWatchLogsEncodingCP424 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP424 = CloudWatchLogsEncoding' "cp424"

pattern CloudWatchLogsEncodingCP437 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP437 = CloudWatchLogsEncoding' "cp437"

pattern CloudWatchLogsEncodingCP500 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP500 = CloudWatchLogsEncoding' "cp500"

pattern CloudWatchLogsEncodingCP720 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP720 = CloudWatchLogsEncoding' "cp720"

pattern CloudWatchLogsEncodingCP737 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP737 = CloudWatchLogsEncoding' "cp737"

pattern CloudWatchLogsEncodingCP775 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP775 = CloudWatchLogsEncoding' "cp775"

pattern CloudWatchLogsEncodingCP850 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP850 = CloudWatchLogsEncoding' "cp850"

pattern CloudWatchLogsEncodingCP852 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP852 = CloudWatchLogsEncoding' "cp852"

pattern CloudWatchLogsEncodingCP855 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP855 = CloudWatchLogsEncoding' "cp855"

pattern CloudWatchLogsEncodingCP856 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP856 = CloudWatchLogsEncoding' "cp856"

pattern CloudWatchLogsEncodingCP857 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP857 = CloudWatchLogsEncoding' "cp857"

pattern CloudWatchLogsEncodingCP858 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP858 = CloudWatchLogsEncoding' "cp858"

pattern CloudWatchLogsEncodingCP860 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP860 = CloudWatchLogsEncoding' "cp860"

pattern CloudWatchLogsEncodingCP861 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP861 = CloudWatchLogsEncoding' "cp861"

pattern CloudWatchLogsEncodingCP862 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP862 = CloudWatchLogsEncoding' "cp862"

pattern CloudWatchLogsEncodingCP863 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP863 = CloudWatchLogsEncoding' "cp863"

pattern CloudWatchLogsEncodingCP864 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP864 = CloudWatchLogsEncoding' "cp864"

pattern CloudWatchLogsEncodingCP865 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP865 = CloudWatchLogsEncoding' "cp865"

pattern CloudWatchLogsEncodingCP866 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP866 = CloudWatchLogsEncoding' "cp866"

pattern CloudWatchLogsEncodingCP869 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP869 = CloudWatchLogsEncoding' "cp869"

pattern CloudWatchLogsEncodingCP874 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP874 = CloudWatchLogsEncoding' "cp874"

pattern CloudWatchLogsEncodingCP875 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP875 = CloudWatchLogsEncoding' "cp875"

pattern CloudWatchLogsEncodingCP932 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP932 = CloudWatchLogsEncoding' "cp932"

pattern CloudWatchLogsEncodingCP949 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP949 = CloudWatchLogsEncoding' "cp949"

pattern CloudWatchLogsEncodingCP950 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP950 = CloudWatchLogsEncoding' "cp950"

pattern CloudWatchLogsEncodingCP1006 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1006 = CloudWatchLogsEncoding' "cp1006"

pattern CloudWatchLogsEncodingCP1026 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1026 = CloudWatchLogsEncoding' "cp1026"

pattern CloudWatchLogsEncodingCP1140 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1140 = CloudWatchLogsEncoding' "cp1140"

pattern CloudWatchLogsEncodingCP1250 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1250 = CloudWatchLogsEncoding' "cp1250"

pattern CloudWatchLogsEncodingCP1251 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1251 = CloudWatchLogsEncoding' "cp1251"

pattern CloudWatchLogsEncodingCP1252 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1252 = CloudWatchLogsEncoding' "cp1252"

pattern CloudWatchLogsEncodingCP1253 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1253 = CloudWatchLogsEncoding' "cp1253"

pattern CloudWatchLogsEncodingCP1254 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1254 = CloudWatchLogsEncoding' "cp1254"

pattern CloudWatchLogsEncodingCP1255 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1255 = CloudWatchLogsEncoding' "cp1255"

pattern CloudWatchLogsEncodingCP1256 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1256 = CloudWatchLogsEncoding' "cp1256"

pattern CloudWatchLogsEncodingCP1257 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1257 = CloudWatchLogsEncoding' "cp1257"

pattern CloudWatchLogsEncodingCP1258 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingCP1258 = CloudWatchLogsEncoding' "cp1258"

pattern CloudWatchLogsEncodingEucJp :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingEucJp = CloudWatchLogsEncoding' "euc_jp"

pattern CloudWatchLogsEncodingEucJis2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingEucJis2004 = CloudWatchLogsEncoding' "euc_jis_2004"

pattern CloudWatchLogsEncodingEucJISX0213 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingEucJISX0213 = CloudWatchLogsEncoding' "euc_jisx0213"

pattern CloudWatchLogsEncodingEucKr :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingEucKr = CloudWatchLogsEncoding' "euc_kr"

pattern CloudWatchLogsEncodingGB2312 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingGB2312 = CloudWatchLogsEncoding' "gb2312"

pattern CloudWatchLogsEncodingGbk :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingGbk = CloudWatchLogsEncoding' "gbk"

pattern CloudWatchLogsEncodingGB18030 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingGB18030 = CloudWatchLogsEncoding' "gb18030"

pattern CloudWatchLogsEncodingHZ :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingHZ = CloudWatchLogsEncoding' "hz"

pattern CloudWatchLogsEncodingISO2022Jp :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Jp = CloudWatchLogsEncoding' "iso2022_jp"

pattern CloudWatchLogsEncodingISO2022Jp1 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Jp1 = CloudWatchLogsEncoding' "iso2022_jp_1"

pattern CloudWatchLogsEncodingISO2022Jp2 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Jp2 = CloudWatchLogsEncoding' "iso2022_jp_2"

pattern CloudWatchLogsEncodingISO2022Jp2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Jp2004 = CloudWatchLogsEncoding' "iso2022_jp_2004"

pattern CloudWatchLogsEncodingISO2022Jp3 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Jp3 = CloudWatchLogsEncoding' "iso2022_jp_3"

pattern CloudWatchLogsEncodingISO2022JpExt :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022JpExt = CloudWatchLogsEncoding' "iso2022_jp_ext"

pattern CloudWatchLogsEncodingISO2022Kr :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO2022Kr = CloudWatchLogsEncoding' "iso2022_kr"

pattern CloudWatchLogsEncodingLatin1 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingLatin1 = CloudWatchLogsEncoding' "latin_1"

pattern CloudWatchLogsEncodingISO88592 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88592 = CloudWatchLogsEncoding' "iso8859_2"

pattern CloudWatchLogsEncodingISO88593 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88593 = CloudWatchLogsEncoding' "iso8859_3"

pattern CloudWatchLogsEncodingISO88594 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88594 = CloudWatchLogsEncoding' "iso8859_4"

pattern CloudWatchLogsEncodingISO88595 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88595 = CloudWatchLogsEncoding' "iso8859_5"

pattern CloudWatchLogsEncodingISO88596 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88596 = CloudWatchLogsEncoding' "iso8859_6"

pattern CloudWatchLogsEncodingISO88597 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88597 = CloudWatchLogsEncoding' "iso8859_7"

pattern CloudWatchLogsEncodingISO88598 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88598 = CloudWatchLogsEncoding' "iso8859_8"

pattern CloudWatchLogsEncodingISO88599 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO88599 = CloudWatchLogsEncoding' "iso8859_9"

pattern CloudWatchLogsEncodingISO885910 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO885910 = CloudWatchLogsEncoding' "iso8859_10"

pattern CloudWatchLogsEncodingISO885913 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO885913 = CloudWatchLogsEncoding' "iso8859_13"

pattern CloudWatchLogsEncodingISO885914 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO885914 = CloudWatchLogsEncoding' "iso8859_14"

pattern CloudWatchLogsEncodingISO885915 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO885915 = CloudWatchLogsEncoding' "iso8859_15"

pattern CloudWatchLogsEncodingISO885916 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingISO885916 = CloudWatchLogsEncoding' "iso8859_16"

pattern CloudWatchLogsEncodingJohab :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingJohab = CloudWatchLogsEncoding' "johab"

pattern CloudWatchLogsEncodingKOI8r :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingKOI8r = CloudWatchLogsEncoding' "koi8_r"

pattern CloudWatchLogsEncodingKOI8u :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingKOI8u = CloudWatchLogsEncoding' "koi8_u"

pattern CloudWatchLogsEncodingMacCyrillic :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacCyrillic = CloudWatchLogsEncoding' "mac_cyrillic"

pattern CloudWatchLogsEncodingMacGreek :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacGreek = CloudWatchLogsEncoding' "mac_greek"

pattern CloudWatchLogsEncodingMacIceland :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacIceland = CloudWatchLogsEncoding' "mac_iceland"

pattern CloudWatchLogsEncodingMacLATIN2 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacLATIN2 = CloudWatchLogsEncoding' "mac_latin2"

pattern CloudWatchLogsEncodingMacRoman :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacRoman = CloudWatchLogsEncoding' "mac_roman"

pattern CloudWatchLogsEncodingMacTurkish :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingMacTurkish = CloudWatchLogsEncoding' "mac_turkish"

pattern CloudWatchLogsEncodingPTCP154 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingPTCP154 = CloudWatchLogsEncoding' "ptcp154"

pattern CloudWatchLogsEncodingShiftJis :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingShiftJis = CloudWatchLogsEncoding' "shift_jis"

pattern CloudWatchLogsEncodingShiftJis2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingShiftJis2004 = CloudWatchLogsEncoding' "shift_jis_2004"

pattern CloudWatchLogsEncodingShiftJISX0213 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingShiftJISX0213 = CloudWatchLogsEncoding' "shift_jisx0213"

pattern CloudWatchLogsEncodingUtf32 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf32 = CloudWatchLogsEncoding' "utf_32"

pattern CloudWatchLogsEncodingUtf32Be :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf32Be = CloudWatchLogsEncoding' "utf_32_be"

pattern CloudWatchLogsEncodingUtf32Le :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf32Le = CloudWatchLogsEncoding' "utf_32_le"

pattern CloudWatchLogsEncodingUtf16 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf16 = CloudWatchLogsEncoding' "utf_16"

pattern CloudWatchLogsEncodingUtf16Be :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf16Be = CloudWatchLogsEncoding' "utf_16_be"

pattern CloudWatchLogsEncodingUtf16Le :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf16Le = CloudWatchLogsEncoding' "utf_16_le"

pattern CloudWatchLogsEncodingUtf7 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf7 = CloudWatchLogsEncoding' "utf_7"

pattern CloudWatchLogsEncodingUtf8 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf8 = CloudWatchLogsEncoding' "utf_8"

pattern CloudWatchLogsEncodingUtf8Sig :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncodingUtf8Sig = CloudWatchLogsEncoding' "utf_8_sig"

{-# COMPLETE
  CloudWatchLogsEncodingAscii,
  CloudWatchLogsEncodingBIG5,
  CloudWatchLogsEncodingBig5hkscs,
  CloudWatchLogsEncodingCP037,
  CloudWatchLogsEncodingCP424,
  CloudWatchLogsEncodingCP437,
  CloudWatchLogsEncodingCP500,
  CloudWatchLogsEncodingCP720,
  CloudWatchLogsEncodingCP737,
  CloudWatchLogsEncodingCP775,
  CloudWatchLogsEncodingCP850,
  CloudWatchLogsEncodingCP852,
  CloudWatchLogsEncodingCP855,
  CloudWatchLogsEncodingCP856,
  CloudWatchLogsEncodingCP857,
  CloudWatchLogsEncodingCP858,
  CloudWatchLogsEncodingCP860,
  CloudWatchLogsEncodingCP861,
  CloudWatchLogsEncodingCP862,
  CloudWatchLogsEncodingCP863,
  CloudWatchLogsEncodingCP864,
  CloudWatchLogsEncodingCP865,
  CloudWatchLogsEncodingCP866,
  CloudWatchLogsEncodingCP869,
  CloudWatchLogsEncodingCP874,
  CloudWatchLogsEncodingCP875,
  CloudWatchLogsEncodingCP932,
  CloudWatchLogsEncodingCP949,
  CloudWatchLogsEncodingCP950,
  CloudWatchLogsEncodingCP1006,
  CloudWatchLogsEncodingCP1026,
  CloudWatchLogsEncodingCP1140,
  CloudWatchLogsEncodingCP1250,
  CloudWatchLogsEncodingCP1251,
  CloudWatchLogsEncodingCP1252,
  CloudWatchLogsEncodingCP1253,
  CloudWatchLogsEncodingCP1254,
  CloudWatchLogsEncodingCP1255,
  CloudWatchLogsEncodingCP1256,
  CloudWatchLogsEncodingCP1257,
  CloudWatchLogsEncodingCP1258,
  CloudWatchLogsEncodingEucJp,
  CloudWatchLogsEncodingEucJis2004,
  CloudWatchLogsEncodingEucJISX0213,
  CloudWatchLogsEncodingEucKr,
  CloudWatchLogsEncodingGB2312,
  CloudWatchLogsEncodingGbk,
  CloudWatchLogsEncodingGB18030,
  CloudWatchLogsEncodingHZ,
  CloudWatchLogsEncodingISO2022Jp,
  CloudWatchLogsEncodingISO2022Jp1,
  CloudWatchLogsEncodingISO2022Jp2,
  CloudWatchLogsEncodingISO2022Jp2004,
  CloudWatchLogsEncodingISO2022Jp3,
  CloudWatchLogsEncodingISO2022JpExt,
  CloudWatchLogsEncodingISO2022Kr,
  CloudWatchLogsEncodingLatin1,
  CloudWatchLogsEncodingISO88592,
  CloudWatchLogsEncodingISO88593,
  CloudWatchLogsEncodingISO88594,
  CloudWatchLogsEncodingISO88595,
  CloudWatchLogsEncodingISO88596,
  CloudWatchLogsEncodingISO88597,
  CloudWatchLogsEncodingISO88598,
  CloudWatchLogsEncodingISO88599,
  CloudWatchLogsEncodingISO885910,
  CloudWatchLogsEncodingISO885913,
  CloudWatchLogsEncodingISO885914,
  CloudWatchLogsEncodingISO885915,
  CloudWatchLogsEncodingISO885916,
  CloudWatchLogsEncodingJohab,
  CloudWatchLogsEncodingKOI8r,
  CloudWatchLogsEncodingKOI8u,
  CloudWatchLogsEncodingMacCyrillic,
  CloudWatchLogsEncodingMacGreek,
  CloudWatchLogsEncodingMacIceland,
  CloudWatchLogsEncodingMacLATIN2,
  CloudWatchLogsEncodingMacRoman,
  CloudWatchLogsEncodingMacTurkish,
  CloudWatchLogsEncodingPTCP154,
  CloudWatchLogsEncodingShiftJis,
  CloudWatchLogsEncodingShiftJis2004,
  CloudWatchLogsEncodingShiftJISX0213,
  CloudWatchLogsEncodingUtf32,
  CloudWatchLogsEncodingUtf32Be,
  CloudWatchLogsEncodingUtf32Le,
  CloudWatchLogsEncodingUtf16,
  CloudWatchLogsEncodingUtf16Be,
  CloudWatchLogsEncodingUtf16Le,
  CloudWatchLogsEncodingUtf7,
  CloudWatchLogsEncodingUtf8,
  CloudWatchLogsEncodingUtf8Sig,
  CloudWatchLogsEncoding'
  #-}
