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
-- Module      : Network.AWS.MechanicalTurk.Types.HITReviewStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITReviewStatus
  ( HITReviewStatus
      ( ..,
        HITReviewStatus_MarkedForReview,
        HITReviewStatus_NotReviewed,
        HITReviewStatus_ReviewedAppropriate,
        HITReviewStatus_ReviewedInappropriate
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HITReviewStatus = HITReviewStatus'
  { fromHITReviewStatus ::
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

pattern HITReviewStatus_MarkedForReview :: HITReviewStatus
pattern HITReviewStatus_MarkedForReview = HITReviewStatus' "MarkedForReview"

pattern HITReviewStatus_NotReviewed :: HITReviewStatus
pattern HITReviewStatus_NotReviewed = HITReviewStatus' "NotReviewed"

pattern HITReviewStatus_ReviewedAppropriate :: HITReviewStatus
pattern HITReviewStatus_ReviewedAppropriate = HITReviewStatus' "ReviewedAppropriate"

pattern HITReviewStatus_ReviewedInappropriate :: HITReviewStatus
pattern HITReviewStatus_ReviewedInappropriate = HITReviewStatus' "ReviewedInappropriate"

{-# COMPLETE
  HITReviewStatus_MarkedForReview,
  HITReviewStatus_NotReviewed,
  HITReviewStatus_ReviewedAppropriate,
  HITReviewStatus_ReviewedInappropriate,
  HITReviewStatus'
  #-}
