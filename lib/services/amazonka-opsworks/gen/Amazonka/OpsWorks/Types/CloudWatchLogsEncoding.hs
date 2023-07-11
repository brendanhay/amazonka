{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Types.CloudWatchLogsEncoding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsEncoding
  ( CloudWatchLogsEncoding
      ( ..,
        CloudWatchLogsEncoding_Ascii,
        CloudWatchLogsEncoding_Big5,
        CloudWatchLogsEncoding_Big5hkscs,
        CloudWatchLogsEncoding_Cp037,
        CloudWatchLogsEncoding_Cp1006,
        CloudWatchLogsEncoding_Cp1026,
        CloudWatchLogsEncoding_Cp1140,
        CloudWatchLogsEncoding_Cp1250,
        CloudWatchLogsEncoding_Cp1251,
        CloudWatchLogsEncoding_Cp1252,
        CloudWatchLogsEncoding_Cp1253,
        CloudWatchLogsEncoding_Cp1254,
        CloudWatchLogsEncoding_Cp1255,
        CloudWatchLogsEncoding_Cp1256,
        CloudWatchLogsEncoding_Cp1257,
        CloudWatchLogsEncoding_Cp1258,
        CloudWatchLogsEncoding_Cp424,
        CloudWatchLogsEncoding_Cp437,
        CloudWatchLogsEncoding_Cp500,
        CloudWatchLogsEncoding_Cp720,
        CloudWatchLogsEncoding_Cp737,
        CloudWatchLogsEncoding_Cp775,
        CloudWatchLogsEncoding_Cp850,
        CloudWatchLogsEncoding_Cp852,
        CloudWatchLogsEncoding_Cp855,
        CloudWatchLogsEncoding_Cp856,
        CloudWatchLogsEncoding_Cp857,
        CloudWatchLogsEncoding_Cp858,
        CloudWatchLogsEncoding_Cp860,
        CloudWatchLogsEncoding_Cp861,
        CloudWatchLogsEncoding_Cp862,
        CloudWatchLogsEncoding_Cp863,
        CloudWatchLogsEncoding_Cp864,
        CloudWatchLogsEncoding_Cp865,
        CloudWatchLogsEncoding_Cp866,
        CloudWatchLogsEncoding_Cp869,
        CloudWatchLogsEncoding_Cp874,
        CloudWatchLogsEncoding_Cp875,
        CloudWatchLogsEncoding_Cp932,
        CloudWatchLogsEncoding_Cp949,
        CloudWatchLogsEncoding_Cp950,
        CloudWatchLogsEncoding_Euc_jis_2004,
        CloudWatchLogsEncoding_Euc_jisx0213,
        CloudWatchLogsEncoding_Euc_jp,
        CloudWatchLogsEncoding_Euc_kr,
        CloudWatchLogsEncoding_Gb18030,
        CloudWatchLogsEncoding_Gb2312,
        CloudWatchLogsEncoding_Gbk,
        CloudWatchLogsEncoding_Hz,
        CloudWatchLogsEncoding_Iso2022_jp,
        CloudWatchLogsEncoding_Iso2022_jp_1,
        CloudWatchLogsEncoding_Iso2022_jp_2,
        CloudWatchLogsEncoding_Iso2022_jp_2004,
        CloudWatchLogsEncoding_Iso2022_jp_3,
        CloudWatchLogsEncoding_Iso2022_jp_ext,
        CloudWatchLogsEncoding_Iso2022_kr,
        CloudWatchLogsEncoding_Iso8859_10,
        CloudWatchLogsEncoding_Iso8859_13,
        CloudWatchLogsEncoding_Iso8859_14,
        CloudWatchLogsEncoding_Iso8859_15,
        CloudWatchLogsEncoding_Iso8859_16,
        CloudWatchLogsEncoding_Iso8859_2,
        CloudWatchLogsEncoding_Iso8859_3,
        CloudWatchLogsEncoding_Iso8859_4,
        CloudWatchLogsEncoding_Iso8859_5,
        CloudWatchLogsEncoding_Iso8859_6,
        CloudWatchLogsEncoding_Iso8859_7,
        CloudWatchLogsEncoding_Iso8859_8,
        CloudWatchLogsEncoding_Iso8859_9,
        CloudWatchLogsEncoding_Johab,
        CloudWatchLogsEncoding_Koi8_r,
        CloudWatchLogsEncoding_Koi8_u,
        CloudWatchLogsEncoding_Latin_1,
        CloudWatchLogsEncoding_Mac_cyrillic,
        CloudWatchLogsEncoding_Mac_greek,
        CloudWatchLogsEncoding_Mac_iceland,
        CloudWatchLogsEncoding_Mac_latin2,
        CloudWatchLogsEncoding_Mac_roman,
        CloudWatchLogsEncoding_Mac_turkish,
        CloudWatchLogsEncoding_Ptcp154,
        CloudWatchLogsEncoding_Shift_jis,
        CloudWatchLogsEncoding_Shift_jis_2004,
        CloudWatchLogsEncoding_Shift_jisx0213,
        CloudWatchLogsEncoding_Utf_16,
        CloudWatchLogsEncoding_Utf_16_be,
        CloudWatchLogsEncoding_Utf_16_le,
        CloudWatchLogsEncoding_Utf_32,
        CloudWatchLogsEncoding_Utf_32_be,
        CloudWatchLogsEncoding_Utf_32_le,
        CloudWatchLogsEncoding_Utf_7,
        CloudWatchLogsEncoding_Utf_8,
        CloudWatchLogsEncoding_Utf_8_sig
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the encoding of the log file so that the file can be read
-- correctly. The default is @utf_8@. Encodings supported by Python
-- @codecs.decode()@ can be used here.
newtype CloudWatchLogsEncoding = CloudWatchLogsEncoding'
  { fromCloudWatchLogsEncoding ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CloudWatchLogsEncoding_Ascii :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Ascii = CloudWatchLogsEncoding' "ascii"

pattern CloudWatchLogsEncoding_Big5 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Big5 = CloudWatchLogsEncoding' "big5"

pattern CloudWatchLogsEncoding_Big5hkscs :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Big5hkscs = CloudWatchLogsEncoding' "big5hkscs"

pattern CloudWatchLogsEncoding_Cp037 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp037 = CloudWatchLogsEncoding' "cp037"

pattern CloudWatchLogsEncoding_Cp1006 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1006 = CloudWatchLogsEncoding' "cp1006"

pattern CloudWatchLogsEncoding_Cp1026 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1026 = CloudWatchLogsEncoding' "cp1026"

pattern CloudWatchLogsEncoding_Cp1140 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1140 = CloudWatchLogsEncoding' "cp1140"

pattern CloudWatchLogsEncoding_Cp1250 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1250 = CloudWatchLogsEncoding' "cp1250"

pattern CloudWatchLogsEncoding_Cp1251 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1251 = CloudWatchLogsEncoding' "cp1251"

pattern CloudWatchLogsEncoding_Cp1252 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1252 = CloudWatchLogsEncoding' "cp1252"

pattern CloudWatchLogsEncoding_Cp1253 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1253 = CloudWatchLogsEncoding' "cp1253"

pattern CloudWatchLogsEncoding_Cp1254 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1254 = CloudWatchLogsEncoding' "cp1254"

pattern CloudWatchLogsEncoding_Cp1255 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1255 = CloudWatchLogsEncoding' "cp1255"

pattern CloudWatchLogsEncoding_Cp1256 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1256 = CloudWatchLogsEncoding' "cp1256"

pattern CloudWatchLogsEncoding_Cp1257 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1257 = CloudWatchLogsEncoding' "cp1257"

pattern CloudWatchLogsEncoding_Cp1258 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp1258 = CloudWatchLogsEncoding' "cp1258"

pattern CloudWatchLogsEncoding_Cp424 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp424 = CloudWatchLogsEncoding' "cp424"

pattern CloudWatchLogsEncoding_Cp437 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp437 = CloudWatchLogsEncoding' "cp437"

pattern CloudWatchLogsEncoding_Cp500 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp500 = CloudWatchLogsEncoding' "cp500"

pattern CloudWatchLogsEncoding_Cp720 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp720 = CloudWatchLogsEncoding' "cp720"

pattern CloudWatchLogsEncoding_Cp737 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp737 = CloudWatchLogsEncoding' "cp737"

pattern CloudWatchLogsEncoding_Cp775 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp775 = CloudWatchLogsEncoding' "cp775"

pattern CloudWatchLogsEncoding_Cp850 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp850 = CloudWatchLogsEncoding' "cp850"

pattern CloudWatchLogsEncoding_Cp852 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp852 = CloudWatchLogsEncoding' "cp852"

pattern CloudWatchLogsEncoding_Cp855 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp855 = CloudWatchLogsEncoding' "cp855"

pattern CloudWatchLogsEncoding_Cp856 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp856 = CloudWatchLogsEncoding' "cp856"

pattern CloudWatchLogsEncoding_Cp857 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp857 = CloudWatchLogsEncoding' "cp857"

pattern CloudWatchLogsEncoding_Cp858 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp858 = CloudWatchLogsEncoding' "cp858"

pattern CloudWatchLogsEncoding_Cp860 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp860 = CloudWatchLogsEncoding' "cp860"

pattern CloudWatchLogsEncoding_Cp861 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp861 = CloudWatchLogsEncoding' "cp861"

pattern CloudWatchLogsEncoding_Cp862 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp862 = CloudWatchLogsEncoding' "cp862"

pattern CloudWatchLogsEncoding_Cp863 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp863 = CloudWatchLogsEncoding' "cp863"

pattern CloudWatchLogsEncoding_Cp864 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp864 = CloudWatchLogsEncoding' "cp864"

pattern CloudWatchLogsEncoding_Cp865 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp865 = CloudWatchLogsEncoding' "cp865"

pattern CloudWatchLogsEncoding_Cp866 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp866 = CloudWatchLogsEncoding' "cp866"

pattern CloudWatchLogsEncoding_Cp869 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp869 = CloudWatchLogsEncoding' "cp869"

pattern CloudWatchLogsEncoding_Cp874 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp874 = CloudWatchLogsEncoding' "cp874"

pattern CloudWatchLogsEncoding_Cp875 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp875 = CloudWatchLogsEncoding' "cp875"

pattern CloudWatchLogsEncoding_Cp932 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp932 = CloudWatchLogsEncoding' "cp932"

pattern CloudWatchLogsEncoding_Cp949 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp949 = CloudWatchLogsEncoding' "cp949"

pattern CloudWatchLogsEncoding_Cp950 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Cp950 = CloudWatchLogsEncoding' "cp950"

pattern CloudWatchLogsEncoding_Euc_jis_2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Euc_jis_2004 = CloudWatchLogsEncoding' "euc_jis_2004"

pattern CloudWatchLogsEncoding_Euc_jisx0213 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Euc_jisx0213 = CloudWatchLogsEncoding' "euc_jisx0213"

pattern CloudWatchLogsEncoding_Euc_jp :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Euc_jp = CloudWatchLogsEncoding' "euc_jp"

pattern CloudWatchLogsEncoding_Euc_kr :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Euc_kr = CloudWatchLogsEncoding' "euc_kr"

pattern CloudWatchLogsEncoding_Gb18030 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Gb18030 = CloudWatchLogsEncoding' "gb18030"

pattern CloudWatchLogsEncoding_Gb2312 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Gb2312 = CloudWatchLogsEncoding' "gb2312"

pattern CloudWatchLogsEncoding_Gbk :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Gbk = CloudWatchLogsEncoding' "gbk"

pattern CloudWatchLogsEncoding_Hz :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Hz = CloudWatchLogsEncoding' "hz"

pattern CloudWatchLogsEncoding_Iso2022_jp :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp = CloudWatchLogsEncoding' "iso2022_jp"

pattern CloudWatchLogsEncoding_Iso2022_jp_1 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp_1 = CloudWatchLogsEncoding' "iso2022_jp_1"

pattern CloudWatchLogsEncoding_Iso2022_jp_2 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp_2 = CloudWatchLogsEncoding' "iso2022_jp_2"

pattern CloudWatchLogsEncoding_Iso2022_jp_2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp_2004 = CloudWatchLogsEncoding' "iso2022_jp_2004"

pattern CloudWatchLogsEncoding_Iso2022_jp_3 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp_3 = CloudWatchLogsEncoding' "iso2022_jp_3"

pattern CloudWatchLogsEncoding_Iso2022_jp_ext :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_jp_ext = CloudWatchLogsEncoding' "iso2022_jp_ext"

pattern CloudWatchLogsEncoding_Iso2022_kr :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso2022_kr = CloudWatchLogsEncoding' "iso2022_kr"

pattern CloudWatchLogsEncoding_Iso8859_10 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_10 = CloudWatchLogsEncoding' "iso8859_10"

pattern CloudWatchLogsEncoding_Iso8859_13 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_13 = CloudWatchLogsEncoding' "iso8859_13"

pattern CloudWatchLogsEncoding_Iso8859_14 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_14 = CloudWatchLogsEncoding' "iso8859_14"

pattern CloudWatchLogsEncoding_Iso8859_15 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_15 = CloudWatchLogsEncoding' "iso8859_15"

pattern CloudWatchLogsEncoding_Iso8859_16 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_16 = CloudWatchLogsEncoding' "iso8859_16"

pattern CloudWatchLogsEncoding_Iso8859_2 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_2 = CloudWatchLogsEncoding' "iso8859_2"

pattern CloudWatchLogsEncoding_Iso8859_3 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_3 = CloudWatchLogsEncoding' "iso8859_3"

pattern CloudWatchLogsEncoding_Iso8859_4 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_4 = CloudWatchLogsEncoding' "iso8859_4"

pattern CloudWatchLogsEncoding_Iso8859_5 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_5 = CloudWatchLogsEncoding' "iso8859_5"

pattern CloudWatchLogsEncoding_Iso8859_6 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_6 = CloudWatchLogsEncoding' "iso8859_6"

pattern CloudWatchLogsEncoding_Iso8859_7 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_7 = CloudWatchLogsEncoding' "iso8859_7"

pattern CloudWatchLogsEncoding_Iso8859_8 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_8 = CloudWatchLogsEncoding' "iso8859_8"

pattern CloudWatchLogsEncoding_Iso8859_9 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Iso8859_9 = CloudWatchLogsEncoding' "iso8859_9"

pattern CloudWatchLogsEncoding_Johab :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Johab = CloudWatchLogsEncoding' "johab"

pattern CloudWatchLogsEncoding_Koi8_r :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Koi8_r = CloudWatchLogsEncoding' "koi8_r"

pattern CloudWatchLogsEncoding_Koi8_u :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Koi8_u = CloudWatchLogsEncoding' "koi8_u"

pattern CloudWatchLogsEncoding_Latin_1 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Latin_1 = CloudWatchLogsEncoding' "latin_1"

pattern CloudWatchLogsEncoding_Mac_cyrillic :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_cyrillic = CloudWatchLogsEncoding' "mac_cyrillic"

pattern CloudWatchLogsEncoding_Mac_greek :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_greek = CloudWatchLogsEncoding' "mac_greek"

pattern CloudWatchLogsEncoding_Mac_iceland :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_iceland = CloudWatchLogsEncoding' "mac_iceland"

pattern CloudWatchLogsEncoding_Mac_latin2 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_latin2 = CloudWatchLogsEncoding' "mac_latin2"

pattern CloudWatchLogsEncoding_Mac_roman :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_roman = CloudWatchLogsEncoding' "mac_roman"

pattern CloudWatchLogsEncoding_Mac_turkish :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Mac_turkish = CloudWatchLogsEncoding' "mac_turkish"

pattern CloudWatchLogsEncoding_Ptcp154 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Ptcp154 = CloudWatchLogsEncoding' "ptcp154"

pattern CloudWatchLogsEncoding_Shift_jis :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Shift_jis = CloudWatchLogsEncoding' "shift_jis"

pattern CloudWatchLogsEncoding_Shift_jis_2004 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Shift_jis_2004 = CloudWatchLogsEncoding' "shift_jis_2004"

pattern CloudWatchLogsEncoding_Shift_jisx0213 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Shift_jisx0213 = CloudWatchLogsEncoding' "shift_jisx0213"

pattern CloudWatchLogsEncoding_Utf_16 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_16 = CloudWatchLogsEncoding' "utf_16"

pattern CloudWatchLogsEncoding_Utf_16_be :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_16_be = CloudWatchLogsEncoding' "utf_16_be"

pattern CloudWatchLogsEncoding_Utf_16_le :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_16_le = CloudWatchLogsEncoding' "utf_16_le"

pattern CloudWatchLogsEncoding_Utf_32 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_32 = CloudWatchLogsEncoding' "utf_32"

pattern CloudWatchLogsEncoding_Utf_32_be :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_32_be = CloudWatchLogsEncoding' "utf_32_be"

pattern CloudWatchLogsEncoding_Utf_32_le :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_32_le = CloudWatchLogsEncoding' "utf_32_le"

pattern CloudWatchLogsEncoding_Utf_7 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_7 = CloudWatchLogsEncoding' "utf_7"

pattern CloudWatchLogsEncoding_Utf_8 :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_8 = CloudWatchLogsEncoding' "utf_8"

pattern CloudWatchLogsEncoding_Utf_8_sig :: CloudWatchLogsEncoding
pattern CloudWatchLogsEncoding_Utf_8_sig = CloudWatchLogsEncoding' "utf_8_sig"

{-# COMPLETE
  CloudWatchLogsEncoding_Ascii,
  CloudWatchLogsEncoding_Big5,
  CloudWatchLogsEncoding_Big5hkscs,
  CloudWatchLogsEncoding_Cp037,
  CloudWatchLogsEncoding_Cp1006,
  CloudWatchLogsEncoding_Cp1026,
  CloudWatchLogsEncoding_Cp1140,
  CloudWatchLogsEncoding_Cp1250,
  CloudWatchLogsEncoding_Cp1251,
  CloudWatchLogsEncoding_Cp1252,
  CloudWatchLogsEncoding_Cp1253,
  CloudWatchLogsEncoding_Cp1254,
  CloudWatchLogsEncoding_Cp1255,
  CloudWatchLogsEncoding_Cp1256,
  CloudWatchLogsEncoding_Cp1257,
  CloudWatchLogsEncoding_Cp1258,
  CloudWatchLogsEncoding_Cp424,
  CloudWatchLogsEncoding_Cp437,
  CloudWatchLogsEncoding_Cp500,
  CloudWatchLogsEncoding_Cp720,
  CloudWatchLogsEncoding_Cp737,
  CloudWatchLogsEncoding_Cp775,
  CloudWatchLogsEncoding_Cp850,
  CloudWatchLogsEncoding_Cp852,
  CloudWatchLogsEncoding_Cp855,
  CloudWatchLogsEncoding_Cp856,
  CloudWatchLogsEncoding_Cp857,
  CloudWatchLogsEncoding_Cp858,
  CloudWatchLogsEncoding_Cp860,
  CloudWatchLogsEncoding_Cp861,
  CloudWatchLogsEncoding_Cp862,
  CloudWatchLogsEncoding_Cp863,
  CloudWatchLogsEncoding_Cp864,
  CloudWatchLogsEncoding_Cp865,
  CloudWatchLogsEncoding_Cp866,
  CloudWatchLogsEncoding_Cp869,
  CloudWatchLogsEncoding_Cp874,
  CloudWatchLogsEncoding_Cp875,
  CloudWatchLogsEncoding_Cp932,
  CloudWatchLogsEncoding_Cp949,
  CloudWatchLogsEncoding_Cp950,
  CloudWatchLogsEncoding_Euc_jis_2004,
  CloudWatchLogsEncoding_Euc_jisx0213,
  CloudWatchLogsEncoding_Euc_jp,
  CloudWatchLogsEncoding_Euc_kr,
  CloudWatchLogsEncoding_Gb18030,
  CloudWatchLogsEncoding_Gb2312,
  CloudWatchLogsEncoding_Gbk,
  CloudWatchLogsEncoding_Hz,
  CloudWatchLogsEncoding_Iso2022_jp,
  CloudWatchLogsEncoding_Iso2022_jp_1,
  CloudWatchLogsEncoding_Iso2022_jp_2,
  CloudWatchLogsEncoding_Iso2022_jp_2004,
  CloudWatchLogsEncoding_Iso2022_jp_3,
  CloudWatchLogsEncoding_Iso2022_jp_ext,
  CloudWatchLogsEncoding_Iso2022_kr,
  CloudWatchLogsEncoding_Iso8859_10,
  CloudWatchLogsEncoding_Iso8859_13,
  CloudWatchLogsEncoding_Iso8859_14,
  CloudWatchLogsEncoding_Iso8859_15,
  CloudWatchLogsEncoding_Iso8859_16,
  CloudWatchLogsEncoding_Iso8859_2,
  CloudWatchLogsEncoding_Iso8859_3,
  CloudWatchLogsEncoding_Iso8859_4,
  CloudWatchLogsEncoding_Iso8859_5,
  CloudWatchLogsEncoding_Iso8859_6,
  CloudWatchLogsEncoding_Iso8859_7,
  CloudWatchLogsEncoding_Iso8859_8,
  CloudWatchLogsEncoding_Iso8859_9,
  CloudWatchLogsEncoding_Johab,
  CloudWatchLogsEncoding_Koi8_r,
  CloudWatchLogsEncoding_Koi8_u,
  CloudWatchLogsEncoding_Latin_1,
  CloudWatchLogsEncoding_Mac_cyrillic,
  CloudWatchLogsEncoding_Mac_greek,
  CloudWatchLogsEncoding_Mac_iceland,
  CloudWatchLogsEncoding_Mac_latin2,
  CloudWatchLogsEncoding_Mac_roman,
  CloudWatchLogsEncoding_Mac_turkish,
  CloudWatchLogsEncoding_Ptcp154,
  CloudWatchLogsEncoding_Shift_jis,
  CloudWatchLogsEncoding_Shift_jis_2004,
  CloudWatchLogsEncoding_Shift_jisx0213,
  CloudWatchLogsEncoding_Utf_16,
  CloudWatchLogsEncoding_Utf_16_be,
  CloudWatchLogsEncoding_Utf_16_le,
  CloudWatchLogsEncoding_Utf_32,
  CloudWatchLogsEncoding_Utf_32_be,
  CloudWatchLogsEncoding_Utf_32_le,
  CloudWatchLogsEncoding_Utf_7,
  CloudWatchLogsEncoding_Utf_8,
  CloudWatchLogsEncoding_Utf_8_sig,
  CloudWatchLogsEncoding'
  #-}
