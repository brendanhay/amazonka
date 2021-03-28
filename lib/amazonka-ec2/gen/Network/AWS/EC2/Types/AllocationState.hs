{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllocationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AllocationState
  ( AllocationState
    ( AllocationState'
    , AllocationStateAvailable
    , AllocationStateUnderAssessment
    , AllocationStatePermanentFailure
    , AllocationStateReleased
    , AllocationStateReleasedPermanentFailure
    , AllocationStatePending
    , fromAllocationState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AllocationState = AllocationState'{fromAllocationState ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern AllocationStateAvailable :: AllocationState
pattern AllocationStateAvailable = AllocationState' "available"

pattern AllocationStateUnderAssessment :: AllocationState
pattern AllocationStateUnderAssessment = AllocationState' "under-assessment"

pattern AllocationStatePermanentFailure :: AllocationState
pattern AllocationStatePermanentFailure = AllocationState' "permanent-failure"

pattern AllocationStateReleased :: AllocationState
pattern AllocationStateReleased = AllocationState' "released"

pattern AllocationStateReleasedPermanentFailure :: AllocationState
pattern AllocationStateReleasedPermanentFailure = AllocationState' "released-permanent-failure"

pattern AllocationStatePending :: AllocationState
pattern AllocationStatePending = AllocationState' "pending"

{-# COMPLETE 
  AllocationStateAvailable,

  AllocationStateUnderAssessment,

  AllocationStatePermanentFailure,

  AllocationStateReleased,

  AllocationStateReleasedPermanentFailure,

  AllocationStatePending,
  AllocationState'
  #-}
