-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
  ( SuggesterFuzzyMatching
      ( SuggesterFuzzyMatching',
        High,
        Low,
        None
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SuggesterFuzzyMatching = SuggesterFuzzyMatching' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern High :: SuggesterFuzzyMatching
pattern High = SuggesterFuzzyMatching' "high"

pattern Low :: SuggesterFuzzyMatching
pattern Low = SuggesterFuzzyMatching' "low"

pattern None :: SuggesterFuzzyMatching
pattern None = SuggesterFuzzyMatching' "none"

{-# COMPLETE
  High,
  Low,
  None,
  SuggesterFuzzyMatching'
  #-}
