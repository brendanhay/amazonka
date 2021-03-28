{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITReviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.HITReviewStatus
  ( HITReviewStatus
    ( HITReviewStatus'
    , HITReviewStatusNotReviewed
    , HITReviewStatusMarkedForReview
    , HITReviewStatusReviewedAppropriate
    , HITReviewStatusReviewedInappropriate
    , fromHITReviewStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HITReviewStatus = HITReviewStatus'{fromHITReviewStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern HITReviewStatusNotReviewed :: HITReviewStatus
pattern HITReviewStatusNotReviewed = HITReviewStatus' "NotReviewed"

pattern HITReviewStatusMarkedForReview :: HITReviewStatus
pattern HITReviewStatusMarkedForReview = HITReviewStatus' "MarkedForReview"

pattern HITReviewStatusReviewedAppropriate :: HITReviewStatus
pattern HITReviewStatusReviewedAppropriate = HITReviewStatus' "ReviewedAppropriate"

pattern HITReviewStatusReviewedInappropriate :: HITReviewStatus
pattern HITReviewStatusReviewedInappropriate = HITReviewStatus' "ReviewedInappropriate"

{-# COMPLETE 
  HITReviewStatusNotReviewed,

  HITReviewStatusMarkedForReview,

  HITReviewStatusReviewedAppropriate,

  HITReviewStatusReviewedInappropriate,
  HITReviewStatus'
  #-}
