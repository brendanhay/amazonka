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
    rcsgiEC2SecurityGroupOwnerId,
    rcsgiEC2SecurityGroupName,

    -- * Destructuring the response
    RevokeCacheSecurityGroupIngressResponse (..),
    mkRevokeCacheSecurityGroupIngressResponse,

    -- ** Response lenses
    rcsgirsCacheSecurityGroup,
    rcsgirsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @RevokeCacheSecurityGroupIngress@ operation.
--
-- /See:/ 'mkRevokeCacheSecurityGroupIngress' smart constructor.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
  { -- | The name of the cache security group to revoke ingress from.
    cacheSecurityGroupName :: Lude.Text,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
    ec2SecurityGroupOwnerId :: Lude.Text,
    -- | The name of the Amazon EC2 security group to revoke access from.
    ec2SecurityGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeCacheSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroupName' - The name of the cache security group to revoke ingress from.
-- * 'ec2SecurityGroupOwnerId' - The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
-- * 'ec2SecurityGroupName' - The name of the Amazon EC2 security group to revoke access from.
mkRevokeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Lude.Text ->
  -- | 'ec2SecurityGroupOwnerId'
  Lude.Text ->
  -- | 'ec2SecurityGroupName'
  Lude.Text ->
  RevokeCacheSecurityGroupIngress
mkRevokeCacheSecurityGroupIngress
  pCacheSecurityGroupName_
  pEC2SecurityGroupOwnerId_
  pEC2SecurityGroupName_ =
    RevokeCacheSecurityGroupIngress'
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        ec2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId_,
        ec2SecurityGroupName = pEC2SecurityGroupName_
      }

-- | The name of the cache security group to revoke ingress from.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiCacheSecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Lude.Text
rcsgiCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: RevokeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)
{-# DEPRECATED rcsgiCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeCacheSecurityGroupIngress Lude.Text
rcsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: RevokeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: RevokeCacheSecurityGroupIngress)
{-# DEPRECATED rcsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The name of the Amazon EC2 security group to revoke access from.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Lude.Text
rcsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: RevokeCacheSecurityGroupIngress -> Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)
{-# DEPRECATED rcsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

instance Lude.AWSRequest RevokeCacheSecurityGroupIngress where
  type
    Rs RevokeCacheSecurityGroupIngress =
      RevokeCacheSecurityGroupIngressResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "RevokeCacheSecurityGroupIngressResult"
      ( \s h x ->
          RevokeCacheSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "CacheSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeCacheSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeCacheSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeCacheSecurityGroupIngress where
  toQuery RevokeCacheSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RevokeCacheSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSecurityGroupName" Lude.=: cacheSecurityGroupName,
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName
      ]

-- | /See:/ 'mkRevokeCacheSecurityGroupIngressResponse' smart constructor.
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Lude.Maybe CacheSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeCacheSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkRevokeCacheSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeCacheSecurityGroupIngressResponse
mkRevokeCacheSecurityGroupIngressResponse pResponseStatus_ =
  RevokeCacheSecurityGroupIngressResponse'
    { cacheSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirsCacheSecurityGroup :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse (Lude.Maybe CacheSecurityGroup)
rcsgirsCacheSecurityGroup = Lens.lens (cacheSecurityGroup :: RevokeCacheSecurityGroupIngressResponse -> Lude.Maybe CacheSecurityGroup) (\s a -> s {cacheSecurityGroup = a} :: RevokeCacheSecurityGroupIngressResponse)
{-# DEPRECATED rcsgirsCacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirsResponseStatus :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse Lude.Int
rcsgirsResponseStatus = Lens.lens (responseStatus :: RevokeCacheSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeCacheSecurityGroupIngressResponse)
{-# DEPRECATED rcsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
