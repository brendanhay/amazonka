{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RebootReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a replication instance. Rebooting results in a momentary outage, until the replication instance becomes available again.
module Network.AWS.DMS.RebootReplicationInstance
  ( -- * Creating a request
    RebootReplicationInstance (..),
    mkRebootReplicationInstance,

    -- ** Request lenses
    rriReplicationInstanceArn,
    rriForceFailover,

    -- * Destructuring the response
    RebootReplicationInstanceResponse (..),
    mkRebootReplicationInstanceResponse,

    -- ** Response lenses
    rrirrsReplicationInstance,
    rrirrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootReplicationInstance' smart constructor.
data RebootReplicationInstance = RebootReplicationInstance'
  { -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Types.String,
    -- | If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
    forceFailover :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootReplicationInstance' value with any optional fields omitted.
mkRebootReplicationInstance ::
  -- | 'replicationInstanceArn'
  Types.String ->
  RebootReplicationInstance
mkRebootReplicationInstance replicationInstanceArn =
  RebootReplicationInstance'
    { replicationInstanceArn,
      forceFailover = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rriReplicationInstanceArn :: Lens.Lens' RebootReplicationInstance Types.String
rriReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED rriReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
--
-- /Note:/ Consider using 'forceFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rriForceFailover :: Lens.Lens' RebootReplicationInstance (Core.Maybe Core.Bool)
rriForceFailover = Lens.field @"forceFailover"
{-# DEPRECATED rriForceFailover "Use generic-lens or generic-optics with 'forceFailover' instead." #-}

instance Core.FromJSON RebootReplicationInstance where
  toJSON RebootReplicationInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn),
            ("ForceFailover" Core..=) Core.<$> forceFailover
          ]
      )

instance Core.AWSRequest RebootReplicationInstance where
  type
    Rs RebootReplicationInstance =
      RebootReplicationInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.RebootReplicationInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootReplicationInstanceResponse'
            Core.<$> (x Core..:? "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebootReplicationInstanceResponse' smart constructor.
data RebootReplicationInstanceResponse = RebootReplicationInstanceResponse'
  { -- | The replication instance that is being rebooted.
    replicationInstance :: Core.Maybe Types.ReplicationInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RebootReplicationInstanceResponse' value with any optional fields omitted.
mkRebootReplicationInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebootReplicationInstanceResponse
mkRebootReplicationInstanceResponse responseStatus =
  RebootReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      responseStatus
    }

-- | The replication instance that is being rebooted.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrirrsReplicationInstance :: Lens.Lens' RebootReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
rrirrsReplicationInstance = Lens.field @"replicationInstance"
{-# DEPRECATED rrirrsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrirrsResponseStatus :: Lens.Lens' RebootReplicationInstanceResponse Core.Int
rrirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
