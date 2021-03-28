{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Region-specific settings for a global table.
module Network.AWS.DynamoDB.DescribeGlobalTableSettings
    (
    -- * Creating a request
      DescribeGlobalTableSettings (..)
    , mkDescribeGlobalTableSettings
    -- ** Request lenses
    , dgtsGlobalTableName

    -- * Destructuring the response
    , DescribeGlobalTableSettingsResponse (..)
    , mkDescribeGlobalTableSettingsResponse
    -- ** Response lenses
    , dgtsrrsGlobalTableName
    , dgtsrrsReplicaSettings
    , dgtsrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalTableSettings' smart constructor.
newtype DescribeGlobalTableSettings = DescribeGlobalTableSettings'
  { globalTableName :: Types.GlobalTableName
    -- ^ The name of the global table to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalTableSettings' value with any optional fields omitted.
mkDescribeGlobalTableSettings
    :: Types.GlobalTableName -- ^ 'globalTableName'
    -> DescribeGlobalTableSettings
mkDescribeGlobalTableSettings globalTableName
  = DescribeGlobalTableSettings'{globalTableName}

-- | The name of the global table to describe.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsGlobalTableName :: Lens.Lens' DescribeGlobalTableSettings Types.GlobalTableName
dgtsGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE dgtsGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

instance Core.ToQuery DescribeGlobalTableSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeGlobalTableSettings where
        toHeaders DescribeGlobalTableSettings{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.DescribeGlobalTableSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeGlobalTableSettings where
        toJSON DescribeGlobalTableSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GlobalTableName" Core..= globalTableName)])

instance Core.AWSRequest DescribeGlobalTableSettings where
        type Rs DescribeGlobalTableSettings =
             DescribeGlobalTableSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGlobalTableSettingsResponse' Core.<$>
                   (x Core..:? "GlobalTableName") Core.<*>
                     x Core..:? "ReplicaSettings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeGlobalTableSettingsResponse' smart constructor.
data DescribeGlobalTableSettingsResponse = DescribeGlobalTableSettingsResponse'
  { globalTableName :: Core.Maybe Types.GlobalTableName
    -- ^ The name of the global table.
  , replicaSettings :: Core.Maybe [Types.ReplicaSettingsDescription]
    -- ^ The Region-specific settings for the global table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeGlobalTableSettingsResponse' value with any optional fields omitted.
mkDescribeGlobalTableSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGlobalTableSettingsResponse
mkDescribeGlobalTableSettingsResponse responseStatus
  = DescribeGlobalTableSettingsResponse'{globalTableName =
                                           Core.Nothing,
                                         replicaSettings = Core.Nothing, responseStatus}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrrsGlobalTableName :: Lens.Lens' DescribeGlobalTableSettingsResponse (Core.Maybe Types.GlobalTableName)
dgtsrrsGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE dgtsrrsGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

-- | The Region-specific settings for the global table.
--
-- /Note:/ Consider using 'replicaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrrsReplicaSettings :: Lens.Lens' DescribeGlobalTableSettingsResponse (Core.Maybe [Types.ReplicaSettingsDescription])
dgtsrrsReplicaSettings = Lens.field @"replicaSettings"
{-# INLINEABLE dgtsrrsReplicaSettings #-}
{-# DEPRECATED replicaSettings "Use generic-lens or generic-optics with 'replicaSettings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtsrrsResponseStatus :: Lens.Lens' DescribeGlobalTableSettingsResponse Core.Int
dgtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
