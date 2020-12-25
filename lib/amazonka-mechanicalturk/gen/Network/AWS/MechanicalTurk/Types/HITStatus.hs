{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITStatus
  ( HITStatus
      ( HITStatus',
        HITStatusAssignable,
        HITStatusUnassignable,
        HITStatusReviewable,
        HITStatusReviewing,
        HITStatusDisposed,
        fromHITStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HITStatus = HITStatus' {fromHITStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HITStatusAssignable :: HITStatus
pattern HITStatusAssignable = HITStatus' "Assignable"

pattern HITStatusUnassignable :: HITStatus
pattern HITStatusUnassignable = HITStatus' "Unassignable"

pattern HITStatusReviewable :: HITStatus
pattern HITStatusReviewable = HITStatus' "Reviewable"

pattern HITStatusReviewing :: HITStatus
pattern HITStatusReviewing = HITStatus' "Reviewing"

pattern HITStatusDisposed :: HITStatus
pattern HITStatusDisposed = HITStatus' "Disposed"

{-# COMPLETE
  HITStatusAssignable,
  HITStatusUnassignable,
  HITStatusReviewable,
  HITStatusReviewing,
  HITStatusDisposed,
  HITStatus'
  #-}
