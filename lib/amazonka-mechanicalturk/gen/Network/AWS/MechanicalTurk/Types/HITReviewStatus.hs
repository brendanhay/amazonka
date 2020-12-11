-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITReviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITReviewStatus
  ( HITReviewStatus
      ( HITReviewStatus',
        MarkedForReview,
        NotReviewed,
        ReviewedAppropriate,
        ReviewedInappropriate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HITReviewStatus = HITReviewStatus' Lude.Text
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

pattern MarkedForReview :: HITReviewStatus
pattern MarkedForReview = HITReviewStatus' "MarkedForReview"

pattern NotReviewed :: HITReviewStatus
pattern NotReviewed = HITReviewStatus' "NotReviewed"

pattern ReviewedAppropriate :: HITReviewStatus
pattern ReviewedAppropriate = HITReviewStatus' "ReviewedAppropriate"

pattern ReviewedInappropriate :: HITReviewStatus
pattern ReviewedInappropriate = HITReviewStatus' "ReviewedInappropriate"

{-# COMPLETE
  MarkedForReview,
  NotReviewed,
  ReviewedAppropriate,
  ReviewedInappropriate,
  HITReviewStatus'
  #-}
