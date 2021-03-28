{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
  ( SuggesterFuzzyMatching
    ( SuggesterFuzzyMatching'
    , SuggesterFuzzyMatchingNone
    , SuggesterFuzzyMatchingLow
    , SuggesterFuzzyMatchingHigh
    , fromSuggesterFuzzyMatching
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SuggesterFuzzyMatching = SuggesterFuzzyMatching'{fromSuggesterFuzzyMatching
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern SuggesterFuzzyMatchingNone :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatchingNone = SuggesterFuzzyMatching' "none"

pattern SuggesterFuzzyMatchingLow :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatchingLow = SuggesterFuzzyMatching' "low"

pattern SuggesterFuzzyMatchingHigh :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatchingHigh = SuggesterFuzzyMatching' "high"

{-# COMPLETE 
  SuggesterFuzzyMatchingNone,

  SuggesterFuzzyMatchingLow,

  SuggesterFuzzyMatchingHigh,
  SuggesterFuzzyMatching'
  #-}
