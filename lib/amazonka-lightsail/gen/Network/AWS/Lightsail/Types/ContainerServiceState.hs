{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceState
  ( ContainerServiceState
    ( ContainerServiceState'
    , ContainerServiceStatePending
    , ContainerServiceStateReady
    , ContainerServiceStateRunning
    , ContainerServiceStateUpdating
    , ContainerServiceStateDeleting
    , ContainerServiceStateDisabled
    , fromContainerServiceState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContainerServiceState = ContainerServiceState'{fromContainerServiceState
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ContainerServiceStatePending :: ContainerServiceState
pattern ContainerServiceStatePending = ContainerServiceState' "PENDING"

pattern ContainerServiceStateReady :: ContainerServiceState
pattern ContainerServiceStateReady = ContainerServiceState' "READY"

pattern ContainerServiceStateRunning :: ContainerServiceState
pattern ContainerServiceStateRunning = ContainerServiceState' "RUNNING"

pattern ContainerServiceStateUpdating :: ContainerServiceState
pattern ContainerServiceStateUpdating = ContainerServiceState' "UPDATING"

pattern ContainerServiceStateDeleting :: ContainerServiceState
pattern ContainerServiceStateDeleting = ContainerServiceState' "DELETING"

pattern ContainerServiceStateDisabled :: ContainerServiceState
pattern ContainerServiceStateDisabled = ContainerServiceState' "DISABLED"

{-# COMPLETE 
  ContainerServiceStatePending,

  ContainerServiceStateReady,

  ContainerServiceStateRunning,

  ContainerServiceStateUpdating,

  ContainerServiceStateDeleting,

  ContainerServiceStateDisabled,
  ContainerServiceState'
  #-}
