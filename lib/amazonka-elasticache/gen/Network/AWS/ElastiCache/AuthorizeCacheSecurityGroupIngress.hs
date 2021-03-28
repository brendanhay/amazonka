{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows network ingress to a cache security group. Applications using ElastiCache must be running on Amazon EC2, and Amazon EC2 security groups are used as the authorization mechanism.
module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
    (
    -- * Creating a request
      AuthorizeCacheSecurityGroupIngress (..)
    , mkAuthorizeCacheSecurityGroupIngress
    -- ** Request lenses
    , acsgiCacheSecurityGroupName
    , acsgiEC2SecurityGroupName
    , acsgiEC2SecurityGroupOwnerId

    -- * Destructuring the response
    , AuthorizeCacheSecurityGroupIngressResponse (..)
    , mkAuthorizeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirrsCacheSecurityGroup
    , acsgirrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AuthorizeCacheSecurityGroupIngress operation.
--
-- /See:/ 'mkAuthorizeCacheSecurityGroupIngress' smart constructor.
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'
  { cacheSecurityGroupName :: Core.Text
    -- ^ The cache security group that allows network ingress.
  , eC2SecurityGroupName :: Core.Text
    -- ^ The Amazon EC2 security group to be authorized for ingress to the cache security group.
  , eC2SecurityGroupOwnerId :: Core.Text
    -- ^ The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeCacheSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeCacheSecurityGroupIngress
    :: Core.Text -- ^ 'cacheSecurityGroupName'
    -> Core.Text -- ^ 'eC2SecurityGroupName'
    -> Core.Text -- ^ 'eC2SecurityGroupOwnerId'
    -> AuthorizeCacheSecurityGroupIngress
mkAuthorizeCacheSecurityGroupIngress cacheSecurityGroupName
  eC2SecurityGroupName eC2SecurityGroupOwnerId
  = AuthorizeCacheSecurityGroupIngress'{cacheSecurityGroupName,
                                        eC2SecurityGroupName, eC2SecurityGroupOwnerId}

-- | The cache security group that allows network ingress.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiCacheSecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Core.Text
acsgiCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# INLINEABLE acsgiCacheSecurityGroupName #-}
{-# DEPRECATED cacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead"  #-}

-- | The Amazon EC2 security group to be authorized for ingress to the cache security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Core.Text
acsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE acsgiEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Core.Text
acsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE acsgiEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

instance Core.ToQuery AuthorizeCacheSecurityGroupIngress where
        toQuery AuthorizeCacheSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("AuthorizeCacheSecurityGroupIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSecurityGroupName" cacheSecurityGroupName
              Core.<>
              Core.toQueryPair "EC2SecurityGroupName" eC2SecurityGroupName
              Core.<>
              Core.toQueryPair "EC2SecurityGroupOwnerId" eC2SecurityGroupOwnerId

instance Core.ToHeaders AuthorizeCacheSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AuthorizeCacheSecurityGroupIngress where
        type Rs AuthorizeCacheSecurityGroupIngress =
             AuthorizeCacheSecurityGroupIngressResponse
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
          = Response.receiveXMLWrapper
              "AuthorizeCacheSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeCacheSecurityGroupIngressResponse' Core.<$>
                   (x Core..@? "CacheSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeCacheSecurityGroupIngressResponse' smart constructor.
data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Core.Maybe Types.CacheSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeCacheSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeCacheSecurityGroupIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AuthorizeCacheSecurityGroupIngressResponse
mkAuthorizeCacheSecurityGroupIngressResponse responseStatus
  = AuthorizeCacheSecurityGroupIngressResponse'{cacheSecurityGroup =
                                                  Core.Nothing,
                                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsCacheSecurityGroup :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse (Core.Maybe Types.CacheSecurityGroup)
acsgirrsCacheSecurityGroup = Lens.field @"cacheSecurityGroup"
{-# INLINEABLE acsgirrsCacheSecurityGroup #-}
{-# DEPRECATED cacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsResponseStatus :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse Core.Int
acsgirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acsgirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
