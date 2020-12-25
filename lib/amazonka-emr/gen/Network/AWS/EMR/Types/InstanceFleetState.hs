{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetState
  ( InstanceFleetState
      ( InstanceFleetState',
        InstanceFleetStateProvisioning,
        InstanceFleetStateBootstrapping,
        InstanceFleetStateRunning,
        InstanceFleetStateResizing,
        InstanceFleetStateSuspended,
        InstanceFleetStateTerminating,
        InstanceFleetStateTerminated,
        fromInstanceFleetState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceFleetState = InstanceFleetState'
  { fromInstanceFleetState ::
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

pattern InstanceFleetStateProvisioning :: InstanceFleetState
pattern InstanceFleetStateProvisioning = InstanceFleetState' "PROVISIONING"

pattern InstanceFleetStateBootstrapping :: InstanceFleetState
pattern InstanceFleetStateBootstrapping = InstanceFleetState' "BOOTSTRAPPING"

pattern InstanceFleetStateRunning :: InstanceFleetState
pattern InstanceFleetStateRunning = InstanceFleetState' "RUNNING"

pattern InstanceFleetStateResizing :: InstanceFleetState
pattern InstanceFleetStateResizing = InstanceFleetState' "RESIZING"

pattern InstanceFleetStateSuspended :: InstanceFleetState
pattern InstanceFleetStateSuspended = InstanceFleetState' "SUSPENDED"

pattern InstanceFleetStateTerminating :: InstanceFleetState
pattern InstanceFleetStateTerminating = InstanceFleetState' "TERMINATING"

pattern InstanceFleetStateTerminated :: InstanceFleetState
pattern InstanceFleetStateTerminated = InstanceFleetState' "TERMINATED"

{-# COMPLETE
  InstanceFleetStateProvisioning,
  InstanceFleetStateBootstrapping,
  InstanceFleetStateRunning,
  InstanceFleetStateResizing,
  InstanceFleetStateSuspended,
  InstanceFleetStateTerminating,
  InstanceFleetStateTerminated,
  InstanceFleetState'
  #-}
