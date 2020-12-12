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
        IFSBootstrapping,
        IFSProvisioning,
        IFSResizing,
        IFSRunning,
        IFSSuspended,
        IFSTerminated,
        IFSTerminating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceFleetState = InstanceFleetState' Lude.Text
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

pattern IFSBootstrapping :: InstanceFleetState
pattern IFSBootstrapping = InstanceFleetState' "BOOTSTRAPPING"

pattern IFSProvisioning :: InstanceFleetState
pattern IFSProvisioning = InstanceFleetState' "PROVISIONING"

pattern IFSResizing :: InstanceFleetState
pattern IFSResizing = InstanceFleetState' "RESIZING"

pattern IFSRunning :: InstanceFleetState
pattern IFSRunning = InstanceFleetState' "RUNNING"

pattern IFSSuspended :: InstanceFleetState
pattern IFSSuspended = InstanceFleetState' "SUSPENDED"

pattern IFSTerminated :: InstanceFleetState
pattern IFSTerminated = InstanceFleetState' "TERMINATED"

pattern IFSTerminating :: InstanceFleetState
pattern IFSTerminating = InstanceFleetState' "TERMINATING"

{-# COMPLETE
  IFSBootstrapping,
  IFSProvisioning,
  IFSResizing,
  IFSRunning,
  IFSSuspended,
  IFSTerminated,
  IFSTerminating,
  InstanceFleetState'
  #-}
