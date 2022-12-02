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
-- Module      : Amazonka.MechanicalTurk.Types.HITReviewStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.HITReviewStatus
  ( HITReviewStatus
      ( ..,
        HITReviewStatus_MarkedForReview,
        HITReviewStatus_NotReviewed,
        HITReviewStatus_ReviewedAppropriate,
        HITReviewStatus_ReviewedInappropriate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HITReviewStatus = HITReviewStatus'
  { fromHITReviewStatus ::
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
