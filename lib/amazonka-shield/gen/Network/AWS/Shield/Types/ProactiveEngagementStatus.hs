-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProactiveEngagementStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProactiveEngagementStatus
  ( ProactiveEngagementStatus
      ( ProactiveEngagementStatus',
        PESDisabled,
        PESEnabled,
        PESPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProactiveEngagementStatus = ProactiveEngagementStatus' Lude.Text
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

pattern PESDisabled :: ProactiveEngagementStatus
pattern PESDisabled = ProactiveEngagementStatus' "DISABLED"

pattern PESEnabled :: ProactiveEngagementStatus
pattern PESEnabled = ProactiveEngagementStatus' "ENABLED"

pattern PESPending :: ProactiveEngagementStatus
pattern PESPending = ProactiveEngagementStatus' "PENDING"

{-# COMPLETE
  PESDisabled,
  PESEnabled,
  PESPending,
  ProactiveEngagementStatus'
  #-}
