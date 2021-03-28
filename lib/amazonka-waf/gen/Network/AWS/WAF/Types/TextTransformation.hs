{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TextTransformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.TextTransformation
  ( TextTransformation
    ( TextTransformation'
    , TextTransformationNone
    , TextTransformationCompressWhiteSpace
    , TextTransformationHtmlEntityDecode
    , TextTransformationLowercase
    , TextTransformationCmdLine
    , TextTransformationUrlDecode
    , fromTextTransformation
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TextTransformation = TextTransformation'{fromTextTransformation
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern TextTransformationNone :: TextTransformation
pattern TextTransformationNone = TextTransformation' "NONE"

pattern TextTransformationCompressWhiteSpace :: TextTransformation
pattern TextTransformationCompressWhiteSpace = TextTransformation' "COMPRESS_WHITE_SPACE"

pattern TextTransformationHtmlEntityDecode :: TextTransformation
pattern TextTransformationHtmlEntityDecode = TextTransformation' "HTML_ENTITY_DECODE"

pattern TextTransformationLowercase :: TextTransformation
pattern TextTransformationLowercase = TextTransformation' "LOWERCASE"

pattern TextTransformationCmdLine :: TextTransformation
pattern TextTransformationCmdLine = TextTransformation' "CMD_LINE"

pattern TextTransformationUrlDecode :: TextTransformation
pattern TextTransformationUrlDecode = TextTransformation' "URL_DECODE"

{-# COMPLETE 
  TextTransformationNone,

  TextTransformationCompressWhiteSpace,

  TextTransformationHtmlEntityDecode,

  TextTransformationLowercase,

  TextTransformationCmdLine,

  TextTransformationUrlDecode,
  TextTransformation'
  #-}
