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
    acsgiEC2SecurityGroupOwnerId,
    acsgiEC2SecurityGroupName,

    -- * Destructuring the response
    AuthorizeCacheSecurityGroupIngressResponse (..),
    mkAuthorizeCacheSecurityGroupIngressResponse,

    -- ** Response lenses
    acsgirsCacheSecurityGroup,
    acsgirsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an AuthorizeCacheSecurityGroupIngress operation.
--
-- /See:/ 'mkAuthorizeCacheSecurityGroupIngress' smart constructor.
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'
  { -- | The cache security group that allows network ingress.
    cacheSecurityGroupName :: Lude.Text,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
    ec2SecurityGroupOwnerId :: Lude.Text,
    -- | The Amazon EC2 security group to be authorized for ingress to the cache security group.
    ec2SecurityGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeCacheSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroupName' - The cache security group that allows network ingress.
-- * 'ec2SecurityGroupOwnerId' - The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
-- * 'ec2SecurityGroupName' - The Amazon EC2 security group to be authorized for ingress to the cache security group.
mkAuthorizeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Lude.Text ->
  -- | 'ec2SecurityGroupOwnerId'
  Lude.Text ->
  -- | 'ec2SecurityGroupName'
  Lude.Text ->
  AuthorizeCacheSecurityGroupIngress
mkAuthorizeCacheSecurityGroupIngress
  pCacheSecurityGroupName_
  pEC2SecurityGroupOwnerId_
  pEC2SecurityGroupName_ =
    AuthorizeCacheSecurityGroupIngress'
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        ec2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId_,
        ec2SecurityGroupName = pEC2SecurityGroupName_
      }

-- | The cache security group that allows network ingress.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiCacheSecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Lude.Text
acsgiCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: AuthorizeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: AuthorizeCacheSecurityGroupIngress)
{-# DEPRECATED acsgiCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Lude.Text
acsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: AuthorizeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: AuthorizeCacheSecurityGroupIngress)
{-# DEPRECATED acsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The Amazon EC2 security group to be authorized for ingress to the cache security group.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Lude.Text
acsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: AuthorizeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: AuthorizeCacheSecurityGroupIngress)
{-# DEPRECATED acsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

instance Lude.AWSRequest AuthorizeCacheSecurityGroupIngress where
  type
    Rs AuthorizeCacheSecurityGroupIngress =
      AuthorizeCacheSecurityGroupIngressResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "AuthorizeCacheSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeCacheSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "CacheSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeCacheSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeCacheSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeCacheSecurityGroupIngress where
  toQuery AuthorizeCacheSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AuthorizeCacheSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSecurityGroupName" Lude.=: cacheSecurityGroupName,
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName
      ]

-- | /See:/ 'mkAuthorizeCacheSecurityGroupIngressResponse' smart constructor.
data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Lude.Maybe CacheSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeCacheSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkAuthorizeCacheSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeCacheSecurityGroupIngressResponse
mkAuthorizeCacheSecurityGroupIngressResponse pResponseStatus_ =
  AuthorizeCacheSecurityGroupIngressResponse'
    { cacheSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirsCacheSecurityGroup :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse (Lude.Maybe CacheSecurityGroup)
acsgirsCacheSecurityGroup = Lens.lens (cacheSecurityGroup :: AuthorizeCacheSecurityGroupIngressResponse -> Lude.Maybe CacheSecurityGroup) (\s a -> s {cacheSecurityGroup = a} :: AuthorizeCacheSecurityGroupIngressResponse)
{-# DEPRECATED acsgirsCacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirsResponseStatus :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse Lude.Int
acsgirsResponseStatus = Lens.lens (responseStatus :: AuthorizeCacheSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeCacheSecurityGroupIngressResponse)
{-# DEPRECATED acsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
