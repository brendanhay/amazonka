{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.ContentHandlingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.ContentHandlingStrategy
  ( ContentHandlingStrategy
    ( ContentHandlingStrategy'
    , ContentHandlingStrategyConvertToBinary
    , ContentHandlingStrategyConvertToText
    , fromContentHandlingStrategy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContentHandlingStrategy = ContentHandlingStrategy'{fromContentHandlingStrategy
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern ContentHandlingStrategyConvertToBinary :: ContentHandlingStrategy
pattern ContentHandlingStrategyConvertToBinary = ContentHandlingStrategy' "CONVERT_TO_BINARY"

pattern ContentHandlingStrategyConvertToText :: ContentHandlingStrategy
pattern ContentHandlingStrategyConvertToText = ContentHandlingStrategy' "CONVERT_TO_TEXT"

{-# COMPLETE 
  ContentHandlingStrategyConvertToBinary,

  ContentHandlingStrategyConvertToText,
  ContentHandlingStrategy'
  #-}
