{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CompleteMigration (..)
    , mkCompleteMigration
    -- ** Request lenses
    , cmReplicationGroupId
    , cmForce

    -- * Destructuring the response
    , CompleteMigrationResponse (..)
    , mkCompleteMigrationResponse
    -- ** Response lenses
    , cmrrsReplicationGroup
    , cmrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCompleteMigration' smart constructor.
data CompleteMigration = CompleteMigration'
  { replicationGroupId :: Core.Text
    -- ^ The ID of the replication group to which data is being migrated.
  , force :: Core.Maybe Core.Bool
    -- ^ Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteMigration' value with any optional fields omitted.
mkCompleteMigration
    :: Core.Text -- ^ 'replicationGroupId'
    -> CompleteMigration
mkCompleteMigration replicationGroupId
  = CompleteMigration'{replicationGroupId, force = Core.Nothing}

-- | The ID of the replication group to which data is being migrated.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmReplicationGroupId :: Lens.Lens' CompleteMigration Core.Text
cmReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE cmReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmForce :: Lens.Lens' CompleteMigration (Core.Maybe Core.Bool)
cmForce = Lens.field @"force"
{-# INLINEABLE cmForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery CompleteMigration where
        toQuery CompleteMigration{..}
          = Core.toQueryPair "Action" ("CompleteMigration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ReplicationGroupId" replicationGroupId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Force") force

instance Core.ToHeaders CompleteMigration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CompleteMigration where
        type Rs CompleteMigration = CompleteMigrationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CompleteMigrationResult"
              (\ s h x ->
                 CompleteMigrationResponse' Core.<$>
                   (x Core..@? "ReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCompleteMigrationResponse' smart constructor.
data CompleteMigrationResponse = CompleteMigrationResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CompleteMigrationResponse' value with any optional fields omitted.
mkCompleteMigrationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CompleteMigrationResponse
mkCompleteMigrationResponse responseStatus
  = CompleteMigrationResponse'{replicationGroup = Core.Nothing,
                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsReplicationGroup :: Lens.Lens' CompleteMigrationResponse (Core.Maybe Types.ReplicationGroup)
cmrrsReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE cmrrsReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CompleteMigrationResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
