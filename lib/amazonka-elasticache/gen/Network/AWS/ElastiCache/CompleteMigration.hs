{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CompleteMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Complete the migration of data.
module Network.AWS.ElastiCache.CompleteMigration
  ( -- * Creating a request
    CompleteMigration (..),
    mkCompleteMigration,

    -- ** Request lenses
    cmReplicationGroupId,
    cmForce,

    -- * Destructuring the response
    CompleteMigrationResponse (..),
    mkCompleteMigrationResponse,

    -- ** Response lenses
    cmrrsReplicationGroup,
    cmrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCompleteMigration' smart constructor.
data CompleteMigration = CompleteMigration'
  { -- | The ID of the replication group to which data is being migrated.
    replicationGroupId :: Types.String,
    -- | Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteMigration' value with any optional fields omitted.
mkCompleteMigration ::
  -- | 'replicationGroupId'
  Types.String ->
  CompleteMigration
mkCompleteMigration replicationGroupId =
  CompleteMigration' {replicationGroupId, force = Core.Nothing}

-- | The ID of the replication group to which data is being migrated.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmReplicationGroupId :: Lens.Lens' CompleteMigration Types.String
cmReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED cmReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmForce :: Lens.Lens' CompleteMigration (Core.Maybe Core.Bool)
cmForce = Lens.field @"force"
{-# DEPRECATED cmForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.AWSRequest CompleteMigration where
  type Rs CompleteMigration = CompleteMigrationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CompleteMigration")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CompleteMigrationResult"
      ( \s h x ->
          CompleteMigrationResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCompleteMigrationResponse' smart constructor.
data CompleteMigrationResponse = CompleteMigrationResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CompleteMigrationResponse' value with any optional fields omitted.
mkCompleteMigrationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CompleteMigrationResponse
mkCompleteMigrationResponse responseStatus =
  CompleteMigrationResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsReplicationGroup :: Lens.Lens' CompleteMigrationResponse (Core.Maybe Types.ReplicationGroup)
cmrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED cmrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CompleteMigrationResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
