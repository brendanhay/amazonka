{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllocationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllocationState
  ( AllocationState
      ( AllocationState',
        ASAvailable,
        ASUnderAssessment,
        ASPermanentFailure,
        ASReleased,
        ASReleasedPermanentFailure,
        ASPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AllocationState = AllocationState' Lude.Text
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

pattern ASAvailable :: AllocationState
pattern ASAvailable = AllocationState' "available"

pattern ASUnderAssessment :: AllocationState
pattern ASUnderAssessment = AllocationState' "under-assessment"

pattern ASPermanentFailure :: AllocationState
pattern ASPermanentFailure = AllocationState' "permanent-failure"

pattern ASReleased :: AllocationState
pattern ASReleased = AllocationState' "released"

pattern ASReleasedPermanentFailure :: AllocationState
pattern ASReleasedPermanentFailure = AllocationState' "released-permanent-failure"

pattern ASPending :: AllocationState
pattern ASPending = AllocationState' "pending"

{-# COMPLETE
  ASAvailable,
  ASUnderAssessment,
  ASPermanentFailure,
  ASReleased,
  ASReleasedPermanentFailure,
  ASPending,
  AllocationState'
  #-}
