{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TextTransformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.TextTransformation
  ( TextTransformation
      ( ..,
        TextTransformation_CMD_LINE,
        TextTransformation_COMPRESS_WHITE_SPACE,
        TextTransformation_HTML_ENTITY_DECODE,
        TextTransformation_LOWERCASE,
        TextTransformation_NONE,
        TextTransformation_URL_DECODE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TextTransformation = TextTransformation'
  { fromTextTransformation ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TextTransformation_CMD_LINE :: TextTransformation
pattern TextTransformation_CMD_LINE = TextTransformation' "CMD_LINE"

pattern TextTransformation_COMPRESS_WHITE_SPACE :: TextTransformation
pattern TextTransformation_COMPRESS_WHITE_SPACE = TextTransformation' "COMPRESS_WHITE_SPACE"

pattern TextTransformation_HTML_ENTITY_DECODE :: TextTransformation
pattern TextTransformation_HTML_ENTITY_DECODE = TextTransformation' "HTML_ENTITY_DECODE"

pattern TextTransformation_LOWERCASE :: TextTransformation
pattern TextTransformation_LOWERCASE = TextTransformation' "LOWERCASE"

pattern TextTransformation_NONE :: TextTransformation
pattern TextTransformation_NONE = TextTransformation' "NONE"

pattern TextTransformation_URL_DECODE :: TextTransformation
pattern TextTransformation_URL_DECODE = TextTransformation' "URL_DECODE"

{-# COMPLETE
  TextTransformation_CMD_LINE,
  TextTransformation_COMPRESS_WHITE_SPACE,
  TextTransformation_HTML_ENTITY_DECODE,
  TextTransformation_LOWERCASE,
  TextTransformation_NONE,
  TextTransformation_URL_DECODE,
  TextTransformation'
  #-}
