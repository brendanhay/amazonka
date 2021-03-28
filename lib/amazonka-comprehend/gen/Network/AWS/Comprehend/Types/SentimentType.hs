{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SentimentType
  ( SentimentType
    ( SentimentType'
    , SentimentTypePositive
    , SentimentTypeNegative
    , SentimentTypeNeutral
    , SentimentTypeMixed
    , fromSentimentType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SentimentType = SentimentType'{fromSentimentType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern SentimentTypePositive :: SentimentType
pattern SentimentTypePositive = SentimentType' "POSITIVE"

pattern SentimentTypeNegative :: SentimentType
pattern SentimentTypeNegative = SentimentType' "NEGATIVE"

pattern SentimentTypeNeutral :: SentimentType
pattern SentimentTypeNeutral = SentimentType' "NEUTRAL"

pattern SentimentTypeMixed :: SentimentType
pattern SentimentTypeMixed = SentimentType' "MIXED"

{-# COMPLETE 
  SentimentTypePositive,

  SentimentTypeNegative,

  SentimentTypeNeutral,

  SentimentTypeMixed,
  SentimentType'
  #-}
