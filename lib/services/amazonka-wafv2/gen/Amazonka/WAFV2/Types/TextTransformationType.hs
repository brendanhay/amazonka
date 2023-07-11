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
-- Module      : Amazonka.WAFV2.Types.TextTransformationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.TextTransformationType
  ( TextTransformationType
      ( ..,
        TextTransformationType_BASE64_DECODE,
        TextTransformationType_BASE64_DECODE_EXT,
        TextTransformationType_CMD_LINE,
        TextTransformationType_COMPRESS_WHITE_SPACE,
        TextTransformationType_CSS_DECODE,
        TextTransformationType_ESCAPE_SEQ_DECODE,
        TextTransformationType_HEX_DECODE,
        TextTransformationType_HTML_ENTITY_DECODE,
        TextTransformationType_JS_DECODE,
        TextTransformationType_LOWERCASE,
        TextTransformationType_MD5,
        TextTransformationType_NONE,
        TextTransformationType_NORMALIZE_PATH,
        TextTransformationType_NORMALIZE_PATH_WIN,
        TextTransformationType_REMOVE_NULLS,
        TextTransformationType_REPLACE_COMMENTS,
        TextTransformationType_REPLACE_NULLS,
        TextTransformationType_SQL_HEX_DECODE,
        TextTransformationType_URL_DECODE,
        TextTransformationType_URL_DECODE_UNI,
        TextTransformationType_UTF8_TO_UNICODE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TextTransformationType = TextTransformationType'
  { fromTextTransformationType ::
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

pattern TextTransformationType_BASE64_DECODE :: TextTransformationType
pattern TextTransformationType_BASE64_DECODE = TextTransformationType' "BASE64_DECODE"

pattern TextTransformationType_BASE64_DECODE_EXT :: TextTransformationType
pattern TextTransformationType_BASE64_DECODE_EXT = TextTransformationType' "BASE64_DECODE_EXT"

pattern TextTransformationType_CMD_LINE :: TextTransformationType
pattern TextTransformationType_CMD_LINE = TextTransformationType' "CMD_LINE"

pattern TextTransformationType_COMPRESS_WHITE_SPACE :: TextTransformationType
pattern TextTransformationType_COMPRESS_WHITE_SPACE = TextTransformationType' "COMPRESS_WHITE_SPACE"

pattern TextTransformationType_CSS_DECODE :: TextTransformationType
pattern TextTransformationType_CSS_DECODE = TextTransformationType' "CSS_DECODE"

pattern TextTransformationType_ESCAPE_SEQ_DECODE :: TextTransformationType
pattern TextTransformationType_ESCAPE_SEQ_DECODE = TextTransformationType' "ESCAPE_SEQ_DECODE"

pattern TextTransformationType_HEX_DECODE :: TextTransformationType
pattern TextTransformationType_HEX_DECODE = TextTransformationType' "HEX_DECODE"

pattern TextTransformationType_HTML_ENTITY_DECODE :: TextTransformationType
pattern TextTransformationType_HTML_ENTITY_DECODE = TextTransformationType' "HTML_ENTITY_DECODE"

pattern TextTransformationType_JS_DECODE :: TextTransformationType
pattern TextTransformationType_JS_DECODE = TextTransformationType' "JS_DECODE"

pattern TextTransformationType_LOWERCASE :: TextTransformationType
pattern TextTransformationType_LOWERCASE = TextTransformationType' "LOWERCASE"

pattern TextTransformationType_MD5 :: TextTransformationType
pattern TextTransformationType_MD5 = TextTransformationType' "MD5"

pattern TextTransformationType_NONE :: TextTransformationType
pattern TextTransformationType_NONE = TextTransformationType' "NONE"

pattern TextTransformationType_NORMALIZE_PATH :: TextTransformationType
pattern TextTransformationType_NORMALIZE_PATH = TextTransformationType' "NORMALIZE_PATH"

pattern TextTransformationType_NORMALIZE_PATH_WIN :: TextTransformationType
pattern TextTransformationType_NORMALIZE_PATH_WIN = TextTransformationType' "NORMALIZE_PATH_WIN"

pattern TextTransformationType_REMOVE_NULLS :: TextTransformationType
pattern TextTransformationType_REMOVE_NULLS = TextTransformationType' "REMOVE_NULLS"

pattern TextTransformationType_REPLACE_COMMENTS :: TextTransformationType
pattern TextTransformationType_REPLACE_COMMENTS = TextTransformationType' "REPLACE_COMMENTS"

pattern TextTransformationType_REPLACE_NULLS :: TextTransformationType
pattern TextTransformationType_REPLACE_NULLS = TextTransformationType' "REPLACE_NULLS"

pattern TextTransformationType_SQL_HEX_DECODE :: TextTransformationType
pattern TextTransformationType_SQL_HEX_DECODE = TextTransformationType' "SQL_HEX_DECODE"

pattern TextTransformationType_URL_DECODE :: TextTransformationType
pattern TextTransformationType_URL_DECODE = TextTransformationType' "URL_DECODE"

pattern TextTransformationType_URL_DECODE_UNI :: TextTransformationType
pattern TextTransformationType_URL_DECODE_UNI = TextTransformationType' "URL_DECODE_UNI"

pattern TextTransformationType_UTF8_TO_UNICODE :: TextTransformationType
pattern TextTransformationType_UTF8_TO_UNICODE = TextTransformationType' "UTF8_TO_UNICODE"

{-# COMPLETE
  TextTransformationType_BASE64_DECODE,
  TextTransformationType_BASE64_DECODE_EXT,
  TextTransformationType_CMD_LINE,
  TextTransformationType_COMPRESS_WHITE_SPACE,
  TextTransformationType_CSS_DECODE,
  TextTransformationType_ESCAPE_SEQ_DECODE,
  TextTransformationType_HEX_DECODE,
  TextTransformationType_HTML_ENTITY_DECODE,
  TextTransformationType_JS_DECODE,
  TextTransformationType_LOWERCASE,
  TextTransformationType_MD5,
  TextTransformationType_NONE,
  TextTransformationType_NORMALIZE_PATH,
  TextTransformationType_NORMALIZE_PATH_WIN,
  TextTransformationType_REMOVE_NULLS,
  TextTransformationType_REPLACE_COMMENTS,
  TextTransformationType_REPLACE_NULLS,
  TextTransformationType_SQL_HEX_DECODE,
  TextTransformationType_URL_DECODE,
  TextTransformationType_URL_DECODE_UNI,
  TextTransformationType_UTF8_TO_UNICODE,
  TextTransformationType'
  #-}
