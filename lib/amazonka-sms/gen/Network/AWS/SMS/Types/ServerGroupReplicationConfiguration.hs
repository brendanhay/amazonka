{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
  ( ServerGroupReplicationConfiguration (..)
  -- * Smart constructor
  , mkServerGroupReplicationConfiguration
  -- * Lenses
  , sgrcServerGroupId
  , sgrcServerReplicationConfigurations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ServerGroupId as Types
import qualified Network.AWS.SMS.Types.ServerReplicationConfiguration as Types

-- | Replication configuration for a server group.
--
-- /See:/ 'mkServerGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { serverGroupId :: Core.Maybe Types.ServerGroupId
    -- ^ The ID of the server group with which this replication configuration is associated.
  , serverReplicationConfigurations :: Core.Maybe [Types.ServerReplicationConfiguration]
    -- ^ The replication configuration for servers in the server group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServerGroupReplicationConfiguration' value with any optional fields omitted.
mkServerGroupReplicationConfiguration
    :: ServerGroupReplicationConfiguration
mkServerGroupReplicationConfiguration
  = ServerGroupReplicationConfiguration'{serverGroupId =
                                           Core.Nothing,
                                         serverReplicationConfigurations = Core.Nothing}

-- | The ID of the server group with which this replication configuration is associated.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrcServerGroupId :: Lens.Lens' ServerGroupReplicationConfiguration (Core.Maybe Types.ServerGroupId)
sgrcServerGroupId = Lens.field @"serverGroupId"
{-# INLINEABLE sgrcServerGroupId #-}
{-# DEPRECATED serverGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead"  #-}

-- | The replication configuration for servers in the server group.
--
-- /Note:/ Consider using 'serverReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrcServerReplicationConfigurations :: Lens.Lens' ServerGroupReplicationConfiguration (Core.Maybe [Types.ServerReplicationConfiguration])
sgrcServerReplicationConfigurations = Lens.field @"serverReplicationConfigurations"
{-# INLINEABLE sgrcServerReplicationConfigurations #-}
{-# DEPRECATED serverReplicationConfigurations "Use generic-lens or generic-optics with 'serverReplicationConfigurations' instead"  #-}

instance Core.FromJSON ServerGroupReplicationConfiguration where
        toJSON ServerGroupReplicationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("serverGroupId" Core..=) Core.<$> serverGroupId,
                  ("serverReplicationConfigurations" Core..=) Core.<$>
                    serverReplicationConfigurations])

instance Core.FromJSON ServerGroupReplicationConfiguration where
        parseJSON
          = Core.withObject "ServerGroupReplicationConfiguration" Core.$
              \ x ->
                ServerGroupReplicationConfiguration' Core.<$>
                  (x Core..:? "serverGroupId") Core.<*>
                    x Core..:? "serverReplicationConfigurations"
