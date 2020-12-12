{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentType
  ( SentimentType
      ( SentimentType',
        Mixed,
        Negative,
        Neutral,
        Positive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SentimentType = SentimentType' Lude.Text
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

pattern Mixed :: SentimentType
pattern Mixed = SentimentType' "MIXED"

pattern Negative :: SentimentType
pattern Negative = SentimentType' "NEGATIVE"

pattern Neutral :: SentimentType
pattern Neutral = SentimentType' "NEUTRAL"

pattern Positive :: SentimentType
pattern Positive = SentimentType' "POSITIVE"

{-# COMPLETE
  Mixed,
  Negative,
  Neutral,
  Positive,
  SentimentType'
  #-}
