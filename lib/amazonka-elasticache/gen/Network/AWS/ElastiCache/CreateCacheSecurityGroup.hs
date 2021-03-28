{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache security group. Use a cache security group to control access to one or more clusters.
--
-- Cache security groups are only used when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are creating a cluster inside of a VPC, use a cache subnet group instead. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html CreateCacheSubnetGroup> .
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
    (
    -- * Creating a request
      CreateCacheSecurityGroup (..)
    , mkCreateCacheSecurityGroup
    -- ** Request lenses
    , ccsgCacheSecurityGroupName
    , ccsgDescription

    -- * Destructuring the response
    , CreateCacheSecurityGroupResponse (..)
    , mkCreateCacheSecurityGroupResponse
    -- ** Response lenses
    , ccsgrrsCacheSecurityGroup
    , ccsgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateCacheSecurityGroup@ operation.
--
-- /See:/ 'mkCreateCacheSecurityGroup' smart constructor.
data CreateCacheSecurityGroup = CreateCacheSecurityGroup'
  { cacheSecurityGroupName :: Core.Text
    -- ^ A name for the cache security group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Cannot be the word "Default".
-- Example: @mysecuritygroup@ 
  , description :: Core.Text
    -- ^ A description for the cache security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheSecurityGroup' value with any optional fields omitted.
mkCreateCacheSecurityGroup
    :: Core.Text -- ^ 'cacheSecurityGroupName'
    -> Core.Text -- ^ 'description'
    -> CreateCacheSecurityGroup
mkCreateCacheSecurityGroup cacheSecurityGroupName description
  = CreateCacheSecurityGroup'{cacheSecurityGroupName, description}

-- | A name for the cache security group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Cannot be the word "Default".
-- Example: @mysecuritygroup@ 
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSecurityGroupName :: Lens.Lens' CreateCacheSecurityGroup Core.Text
ccsgCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# INLINEABLE ccsgCacheSecurityGroupName #-}
{-# DEPRECATED cacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead"  #-}

-- | A description for the cache security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgDescription :: Lens.Lens' CreateCacheSecurityGroup Core.Text
ccsgDescription = Lens.field @"description"
{-# INLINEABLE ccsgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateCacheSecurityGroup where
        toQuery CreateCacheSecurityGroup{..}
          = Core.toQueryPair "Action"
              ("CreateCacheSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSecurityGroupName" cacheSecurityGroupName
              Core.<> Core.toQueryPair "Description" description

instance Core.ToHeaders CreateCacheSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCacheSecurityGroup where
        type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse
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
          = Response.receiveXMLWrapper "CreateCacheSecurityGroupResult"
              (\ s h x ->
                 CreateCacheSecurityGroupResponse' Core.<$>
                   (x Core..@? "CacheSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCacheSecurityGroupResponse' smart constructor.
data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse'
  { cacheSecurityGroup :: Core.Maybe Types.CacheSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheSecurityGroupResponse' value with any optional fields omitted.
mkCreateCacheSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCacheSecurityGroupResponse
mkCreateCacheSecurityGroupResponse responseStatus
  = CreateCacheSecurityGroupResponse'{cacheSecurityGroup =
                                        Core.Nothing,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsCacheSecurityGroup :: Lens.Lens' CreateCacheSecurityGroupResponse (Core.Maybe Types.CacheSecurityGroup)
ccsgrrsCacheSecurityGroup = Lens.field @"cacheSecurityGroup"
{-# INLINEABLE ccsgrrsCacheSecurityGroup #-}
{-# DEPRECATED cacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsResponseStatus :: Lens.Lens' CreateCacheSecurityGroupResponse Core.Int
ccsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
