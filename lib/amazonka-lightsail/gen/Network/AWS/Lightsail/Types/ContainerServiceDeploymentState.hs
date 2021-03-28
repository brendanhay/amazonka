{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
  ( ContainerServiceDeploymentState
    ( ContainerServiceDeploymentState'
    , ContainerServiceDeploymentStateActivating
    , ContainerServiceDeploymentStateActive
    , ContainerServiceDeploymentStateInactive
    , ContainerServiceDeploymentStateFailed
    , fromContainerServiceDeploymentState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContainerServiceDeploymentState = ContainerServiceDeploymentState'{fromContainerServiceDeploymentState
                                                                           :: Core.Text}
                                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                            Core.Generic)
                                            deriving newtype (Core.IsString, Core.Hashable,
                                                              Core.NFData, Core.ToJSONKey,
                                                              Core.FromJSONKey, Core.ToJSON,
                                                              Core.FromJSON, Core.ToXML,
                                                              Core.FromXML, Core.ToText,
                                                              Core.FromText, Core.ToByteString,
                                                              Core.ToQuery, Core.ToHeader)

pattern ContainerServiceDeploymentStateActivating :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentStateActivating = ContainerServiceDeploymentState' "ACTIVATING"

pattern ContainerServiceDeploymentStateActive :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentStateActive = ContainerServiceDeploymentState' "ACTIVE"

pattern ContainerServiceDeploymentStateInactive :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentStateInactive = ContainerServiceDeploymentState' "INACTIVE"

pattern ContainerServiceDeploymentStateFailed :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentStateFailed = ContainerServiceDeploymentState' "FAILED"

{-# COMPLETE 
  ContainerServiceDeploymentStateActivating,

  ContainerServiceDeploymentStateActive,

  ContainerServiceDeploymentStateInactive,

  ContainerServiceDeploymentStateFailed,
  ContainerServiceDeploymentState'
  #-}
