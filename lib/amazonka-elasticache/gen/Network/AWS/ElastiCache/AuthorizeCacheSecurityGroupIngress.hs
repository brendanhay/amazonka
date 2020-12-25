{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AuthorizeCacheSecurityGroupIngress (..),
    mkAuthorizeCacheSecurityGroupIngress,

    -- ** Request lenses
    acsgiCacheSecurityGroupName,
    acsgiEC2SecurityGroupName,
    acsgiEC2SecurityGroupOwnerId,

    -- * Destructuring the response
    AuthorizeCacheSecurityGroupIngressResponse (..),
    mkAuthorizeCacheSecurityGroupIngressResponse,

    -- ** Response lenses
    acsgirrsCacheSecurityGroup,
    acsgirrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AuthorizeCacheSecurityGroupIngress operation.
--
-- /See:/ 'mkAuthorizeCacheSecurityGroupIngress' smart constructor.
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'
  { -- | The cache security group that allows network ingress.
    cacheSecurityGroupName :: Types.CacheSecurityGroupName,
    -- | The Amazon EC2 security group to be authorized for ingress to the cache security group.
    eC2SecurityGroupName :: Types.EC2SecurityGroupName,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
    eC2SecurityGroupOwnerId :: Types.EC2SecurityGroupOwnerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeCacheSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Types.CacheSecurityGroupName ->
  -- | 'eC2SecurityGroupName'
  Types.EC2SecurityGroupName ->
  -- | 'eC2SecurityGroupOwnerId'
  Types.EC2SecurityGroupOwnerId ->
  AuthorizeCacheSecurityGroupIngress
mkAuthorizeCacheSecurityGroupIngress
  cacheSecurityGroupName
  eC2SecurityGroupName
  eC2SecurityGroupOwnerId =
    AuthorizeCacheSecurityGroupIngress'
      { cacheSecurityGroupName,
        eC2SecurityGroupName,
        eC2SecurityGroupOwnerId
      }

-- | The cache security group that allows network ingress.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiCacheSecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Types.CacheSecurityGroupName
acsgiCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# DEPRECATED acsgiCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The Amazon EC2 security group to be authorized for ingress to the cache security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Types.EC2SecurityGroupName
acsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED acsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Types.EC2SecurityGroupOwnerId
acsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED acsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

instance Core.AWSRequest AuthorizeCacheSecurityGroupIngress where
  type
    Rs AuthorizeCacheSecurityGroupIngress =
      AuthorizeCacheSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "AuthorizeCacheSecurityGroupIngress")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheSecurityGroupName" cacheSecurityGroupName)
                Core.<> (Core.toQueryValue "EC2SecurityGroupName" eC2SecurityGroupName)
                Core.<> ( Core.toQueryValue
                            "EC2SecurityGroupOwnerId"
                            eC2SecurityGroupOwnerId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "AuthorizeCacheSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeCacheSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "CacheSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAuthorizeCacheSecurityGroupIngressResponse' smart constructor.
data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Core.Maybe Types.CacheSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeCacheSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeCacheSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AuthorizeCacheSecurityGroupIngressResponse
mkAuthorizeCacheSecurityGroupIngressResponse responseStatus =
  AuthorizeCacheSecurityGroupIngressResponse'
    { cacheSecurityGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsCacheSecurityGroup :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse (Core.Maybe Types.CacheSecurityGroup)
acsgirrsCacheSecurityGroup = Lens.field @"cacheSecurityGroup"
{-# DEPRECATED acsgirrsCacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsResponseStatus :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse Core.Int
acsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
