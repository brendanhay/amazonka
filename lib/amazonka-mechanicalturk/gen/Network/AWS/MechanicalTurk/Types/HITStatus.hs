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
        HITSAssignable,
        HITSDisposed,
        HITSReviewable,
        HITSReviewing,
        HITSUnassignable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HITStatus = HITStatus' Lude.Text
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

pattern HITSAssignable :: HITStatus
pattern HITSAssignable = HITStatus' "Assignable"

pattern HITSDisposed :: HITStatus
pattern HITSDisposed = HITStatus' "Disposed"

pattern HITSReviewable :: HITStatus
pattern HITSReviewable = HITStatus' "Reviewable"

pattern HITSReviewing :: HITStatus
pattern HITSReviewing = HITStatus' "Reviewing"

pattern HITSUnassignable :: HITStatus
pattern HITSUnassignable = HITStatus' "Unassignable"

{-# COMPLETE
  HITSAssignable,
  HITSDisposed,
  HITSReviewable,
  HITSReviewing,
  HITSUnassignable,
  HITStatus'
  #-}
