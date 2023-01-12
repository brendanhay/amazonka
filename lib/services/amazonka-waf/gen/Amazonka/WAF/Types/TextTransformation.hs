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
-- Module      : Amazonka.WAF.Types.TextTransformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.TextTransformation
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TextTransformation = TextTransformation'
  { fromTextTransformation ::
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
