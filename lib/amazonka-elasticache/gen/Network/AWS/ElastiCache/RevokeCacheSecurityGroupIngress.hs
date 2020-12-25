{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a cache security group. Use this operation to disallow access from an Amazon EC2 security group that had been previously authorized.
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
  ( -- * Creating a request
    RevokeCacheSecurityGroupIngress (..),
    mkRevokeCacheSecurityGroupIngress,

    -- ** Request lenses
    rcsgiCacheSecurityGroupName,
    rcsgiEC2SecurityGroupName,
    rcsgiEC2SecurityGroupOwnerId,

    -- * Destructuring the response
    RevokeCacheSecurityGroupIngressResponse (..),
    mkRevokeCacheSecurityGroupIngressResponse,

    -- ** Response lenses
    rcsgirrsCacheSecurityGroup,
    rcsgirrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RevokeCacheSecurityGroupIngress@ operation.
--
-- /See:/ 'mkRevokeCacheSecurityGroupIngress' smart constructor.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
  { -- | The name of the cache security group to revoke ingress from.
    cacheSecurityGroupName :: Types.String,
    -- | The name of the Amazon EC2 security group to revoke access from.
    eC2SecurityGroupName :: Types.String,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
    eC2SecurityGroupOwnerId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeCacheSecurityGroupIngress' value with any optional fields omitted.
mkRevokeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Types.String ->
  -- | 'eC2SecurityGroupName'
  Types.String ->
  -- | 'eC2SecurityGroupOwnerId'
  Types.String ->
  RevokeCacheSecurityGroupIngress
mkRevokeCacheSecurityGroupIngress
  cacheSecurityGroupName
  eC2SecurityGroupName
  eC2SecurityGroupOwnerId =
    RevokeCacheSecurityGroupIngress'
      { cacheSecurityGroupName,
        eC2SecurityGroupName,
        eC2SecurityGroupOwnerId
      }

-- | The name of the cache security group to revoke ingress from.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiCacheSecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Types.String
rcsgiCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# DEPRECATED rcsgiCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The name of the Amazon EC2 security group to revoke access from.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Types.String
rcsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED rcsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeCacheSecurityGroupIngress Types.String
rcsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED rcsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

instance Core.AWSRequest RevokeCacheSecurityGroupIngress where
  type
    Rs RevokeCacheSecurityGroupIngress =
      RevokeCacheSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "RevokeCacheSecurityGroupIngress")
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
      "RevokeCacheSecurityGroupIngressResult"
      ( \s h x ->
          RevokeCacheSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "CacheSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRevokeCacheSecurityGroupIngressResponse' smart constructor.
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Core.Maybe Types.CacheSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeCacheSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeCacheSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RevokeCacheSecurityGroupIngressResponse
mkRevokeCacheSecurityGroupIngressResponse responseStatus =
  RevokeCacheSecurityGroupIngressResponse'
    { cacheSecurityGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsCacheSecurityGroup :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse (Core.Maybe Types.CacheSecurityGroup)
rcsgirrsCacheSecurityGroup = Lens.field @"cacheSecurityGroup"
{-# DEPRECATED rcsgirrsCacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsResponseStatus :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse Core.Int
rcsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
