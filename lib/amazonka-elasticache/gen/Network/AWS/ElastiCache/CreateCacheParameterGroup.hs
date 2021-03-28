{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ElastiCache cache parameter group. An ElastiCache cache parameter group is a collection of parameters and their values that are applied to all of the nodes in any cluster or replication group using the CacheParameterGroup.
--
-- A newly created CacheParameterGroup is an exact duplicate of the default parameter group for the CacheParameterGroupFamily. To customize the newly created CacheParameterGroup you can change the values of specific parameters. For more information, see:
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html ModifyCacheParameterGroup> in the ElastiCache API Reference.
--
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ParameterGroups.html Parameters and Parameter Groups> in the ElastiCache User Guide.
--
--
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Creating a request
      CreateCacheParameterGroup (..)
    , mkCreateCacheParameterGroup
    -- ** Request lenses
    , ccpgCacheParameterGroupName
    , ccpgCacheParameterGroupFamily
    , ccpgDescription

    -- * Destructuring the response
    , CreateCacheParameterGroupResponse (..)
    , mkCreateCacheParameterGroupResponse
    -- ** Response lenses
    , ccpgrrsCacheParameterGroup
    , ccpgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateCacheParameterGroup@ operation.
--
-- /See:/ 'mkCreateCacheParameterGroup' smart constructor.
data CreateCacheParameterGroup = CreateCacheParameterGroup'
  { cacheParameterGroupName :: Core.Text
    -- ^ A user-specified name for the cache parameter group.
  , cacheParameterGroupFamily :: Core.Text
    -- ^ The name of the cache parameter group family that the cache parameter group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
  , description :: Core.Text
    -- ^ A user-specified description for the cache parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheParameterGroup' value with any optional fields omitted.
mkCreateCacheParameterGroup
    :: Core.Text -- ^ 'cacheParameterGroupName'
    -> Core.Text -- ^ 'cacheParameterGroupFamily'
    -> Core.Text -- ^ 'description'
    -> CreateCacheParameterGroup
mkCreateCacheParameterGroup cacheParameterGroupName
  cacheParameterGroupFamily description
  = CreateCacheParameterGroup'{cacheParameterGroupName,
                               cacheParameterGroupFamily, description}

-- | A user-specified name for the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgCacheParameterGroupName :: Lens.Lens' CreateCacheParameterGroup Core.Text
ccpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE ccpgCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | The name of the cache parameter group family that the cache parameter group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgCacheParameterGroupFamily :: Lens.Lens' CreateCacheParameterGroup Core.Text
ccpgCacheParameterGroupFamily = Lens.field @"cacheParameterGroupFamily"
{-# INLINEABLE ccpgCacheParameterGroupFamily #-}
{-# DEPRECATED cacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead"  #-}

-- | A user-specified description for the cache parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgDescription :: Lens.Lens' CreateCacheParameterGroup Core.Text
ccpgDescription = Lens.field @"description"
{-# INLINEABLE ccpgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateCacheParameterGroup where
        toQuery CreateCacheParameterGroup{..}
          = Core.toQueryPair "Action"
              ("CreateCacheParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheParameterGroupName" cacheParameterGroupName
              Core.<>
              Core.toQueryPair "CacheParameterGroupFamily"
                cacheParameterGroupFamily
              Core.<> Core.toQueryPair "Description" description

instance Core.ToHeaders CreateCacheParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCacheParameterGroup where
        type Rs CreateCacheParameterGroup =
             CreateCacheParameterGroupResponse
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
          = Response.receiveXMLWrapper "CreateCacheParameterGroupResult"
              (\ s h x ->
                 CreateCacheParameterGroupResponse' Core.<$>
                   (x Core..@? "CacheParameterGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCacheParameterGroupResponse' smart constructor.
data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse'
  { cacheParameterGroup :: Core.Maybe Types.CacheParameterGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheParameterGroupResponse' value with any optional fields omitted.
mkCreateCacheParameterGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCacheParameterGroupResponse
mkCreateCacheParameterGroupResponse responseStatus
  = CreateCacheParameterGroupResponse'{cacheParameterGroup =
                                         Core.Nothing,
                                       responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrrsCacheParameterGroup :: Lens.Lens' CreateCacheParameterGroupResponse (Core.Maybe Types.CacheParameterGroup)
ccpgrrsCacheParameterGroup = Lens.field @"cacheParameterGroup"
{-# INLINEABLE ccpgrrsCacheParameterGroup #-}
{-# DEPRECATED cacheParameterGroup "Use generic-lens or generic-optics with 'cacheParameterGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrrsResponseStatus :: Lens.Lens' CreateCacheParameterGroupResponse Core.Int
ccpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
