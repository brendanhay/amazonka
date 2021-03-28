{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache subnet group.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    (
    -- * Creating a request
      CreateCacheSubnetGroup (..)
    , mkCreateCacheSubnetGroup
    -- ** Request lenses
    , ccsgCacheSubnetGroupName
    , ccsgCacheSubnetGroupDescription
    , ccsgSubnetIds

    -- * Destructuring the response
    , CreateCacheSubnetGroupResponse (..)
    , mkCreateCacheSubnetGroupResponse
    -- ** Response lenses
    , crsCacheSubnetGroup
    , crsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateCacheSubnetGroup@ operation.
--
-- /See:/ 'mkCreateCacheSubnetGroup' smart constructor.
data CreateCacheSubnetGroup = CreateCacheSubnetGroup'
  { cacheSubnetGroupName :: Core.Text
    -- ^ A name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@ 
  , cacheSubnetGroupDescription :: Core.Text
    -- ^ A description for the cache subnet group.
  , subnetIds :: [Core.Text]
    -- ^ A list of VPC subnet IDs for the cache subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheSubnetGroup' value with any optional fields omitted.
mkCreateCacheSubnetGroup
    :: Core.Text -- ^ 'cacheSubnetGroupName'
    -> Core.Text -- ^ 'cacheSubnetGroupDescription'
    -> CreateCacheSubnetGroup
mkCreateCacheSubnetGroup cacheSubnetGroupName
  cacheSubnetGroupDescription
  = CreateCacheSubnetGroup'{cacheSubnetGroupName,
                            cacheSubnetGroupDescription, subnetIds = Core.mempty}

-- | A name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@ 
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSubnetGroupName :: Lens.Lens' CreateCacheSubnetGroup Core.Text
ccsgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE ccsgCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

-- | A description for the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSubnetGroupDescription :: Lens.Lens' CreateCacheSubnetGroup Core.Text
ccsgCacheSubnetGroupDescription = Lens.field @"cacheSubnetGroupDescription"
{-# INLINEABLE ccsgCacheSubnetGroupDescription #-}
{-# DEPRECATED cacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead"  #-}

-- | A list of VPC subnet IDs for the cache subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgSubnetIds :: Lens.Lens' CreateCacheSubnetGroup [Core.Text]
ccsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ccsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.ToQuery CreateCacheSubnetGroup where
        toQuery CreateCacheSubnetGroup{..}
          = Core.toQueryPair "Action" ("CreateCacheSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSubnetGroupName" cacheSubnetGroupName
              Core.<>
              Core.toQueryPair "CacheSubnetGroupDescription"
                cacheSubnetGroupDescription
              Core.<>
              Core.toQueryPair "SubnetIds"
                (Core.toQueryList "SubnetIdentifier" subnetIds)

instance Core.ToHeaders CreateCacheSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCacheSubnetGroup where
        type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse
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
          = Response.receiveXMLWrapper "CreateCacheSubnetGroupResult"
              (\ s h x ->
                 CreateCacheSubnetGroupResponse' Core.<$>
                   (x Core..@? "CacheSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCacheSubnetGroupResponse' smart constructor.
data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse'
  { cacheSubnetGroup :: Core.Maybe Types.CacheSubnetGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheSubnetGroupResponse' value with any optional fields omitted.
mkCreateCacheSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCacheSubnetGroupResponse
mkCreateCacheSubnetGroupResponse responseStatus
  = CreateCacheSubnetGroupResponse'{cacheSubnetGroup = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCacheSubnetGroup :: Lens.Lens' CreateCacheSubnetGroupResponse (Core.Maybe Types.CacheSubnetGroup)
crsCacheSubnetGroup = Lens.field @"cacheSubnetGroup"
{-# INLINEABLE crsCacheSubnetGroup #-}
{-# DEPRECATED cacheSubnetGroup "Use generic-lens or generic-optics with 'cacheSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateCacheSubnetGroupResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
