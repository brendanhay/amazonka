{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthState
  ( InstanceHealthState
      ( InstanceHealthState',
        InstanceHealthStateInitial,
        InstanceHealthStateHealthy,
        InstanceHealthStateUnhealthy,
        InstanceHealthStateUnused,
        InstanceHealthStateDraining,
        InstanceHealthStateUnavailable,
        fromInstanceHealthState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceHealthState = InstanceHealthState'
  { fromInstanceHealthState ::
      Core.Text
  }
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

pattern InstanceHealthStateInitial :: InstanceHealthState
pattern InstanceHealthStateInitial = InstanceHealthState' "initial"

pattern InstanceHealthStateHealthy :: InstanceHealthState
pattern InstanceHealthStateHealthy = InstanceHealthState' "healthy"

pattern InstanceHealthStateUnhealthy :: InstanceHealthState
pattern InstanceHealthStateUnhealthy = InstanceHealthState' "unhealthy"

pattern InstanceHealthStateUnused :: InstanceHealthState
pattern InstanceHealthStateUnused = InstanceHealthState' "unused"

pattern InstanceHealthStateDraining :: InstanceHealthState
pattern InstanceHealthStateDraining = InstanceHealthState' "draining"

pattern InstanceHealthStateUnavailable :: InstanceHealthState
pattern InstanceHealthStateUnavailable = InstanceHealthState' "unavailable"

{-# COMPLETE
  InstanceHealthStateInitial,
  InstanceHealthStateHealthy,
  InstanceHealthStateUnhealthy,
  InstanceHealthStateUnused,
  InstanceHealthStateDraining,
  InstanceHealthStateUnavailable,
  InstanceHealthState'
  #-}
