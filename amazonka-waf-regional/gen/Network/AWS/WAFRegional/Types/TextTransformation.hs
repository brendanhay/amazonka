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
-- Module      : Network.AWS.WAFRegional.Types.TextTransformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.TextTransformation
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

import qualified Network.AWS.Core as Core

newtype TextTransformation = TextTransformation'
  { fromTextTransformation ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
