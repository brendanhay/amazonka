{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.StartMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the migration of data.
module Network.AWS.ElastiCache.StartMigration
    (
    -- * Creating a request
      StartMigration (..)
    , mkStartMigration
    -- ** Request lenses
    , smReplicationGroupId
    , smCustomerNodeEndpointList

    -- * Destructuring the response
    , StartMigrationResponse (..)
    , mkStartMigrationResponse
    -- ** Response lenses
    , smrrsReplicationGroup
    , smrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartMigration' smart constructor.
data StartMigration = StartMigration'
  { replicationGroupId :: Core.Text
    -- ^ The ID of the replication group to which data should be migrated.
  , customerNodeEndpointList :: [Types.CustomerNodeEndpoint]
    -- ^ List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMigration' value with any optional fields omitted.
mkStartMigration
    :: Core.Text -- ^ 'replicationGroupId'
    -> StartMigration
mkStartMigration replicationGroupId
  = StartMigration'{replicationGroupId,
                    customerNodeEndpointList = Core.mempty}

-- | The ID of the replication group to which data should be migrated.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smReplicationGroupId :: Lens.Lens' StartMigration Core.Text
smReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE smReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
--
-- /Note:/ Consider using 'customerNodeEndpointList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smCustomerNodeEndpointList :: Lens.Lens' StartMigration [Types.CustomerNodeEndpoint]
smCustomerNodeEndpointList = Lens.field @"customerNodeEndpointList"
{-# INLINEABLE smCustomerNodeEndpointList #-}
{-# DEPRECATED customerNodeEndpointList "Use generic-lens or generic-optics with 'customerNodeEndpointList' instead"  #-}

instance Core.ToQuery StartMigration where
        toQuery StartMigration{..}
          = Core.toQueryPair "Action" ("StartMigration" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ReplicationGroupId" replicationGroupId
              Core.<>
              Core.toQueryPair "CustomerNodeEndpointList"
                (Core.toQueryList "member" customerNodeEndpointList)

instance Core.ToHeaders StartMigration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest StartMigration where
        type Rs StartMigration = StartMigrationResponse
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
          = Response.receiveXMLWrapper "StartMigrationResult"
              (\ s h x ->
                 StartMigrationResponse' Core.<$>
                   (x Core..@? "ReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartMigrationResponse' smart constructor.
data StartMigrationResponse = StartMigrationResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartMigrationResponse' value with any optional fields omitted.
mkStartMigrationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMigrationResponse
mkStartMigrationResponse responseStatus
  = StartMigrationResponse'{replicationGroup = Core.Nothing,
                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsReplicationGroup :: Lens.Lens' StartMigrationResponse (Core.Maybe Types.ReplicationGroup)
smrrsReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE smrrsReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' StartMigrationResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
