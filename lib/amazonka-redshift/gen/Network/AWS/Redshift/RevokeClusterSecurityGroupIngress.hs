{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an ingress rule in an Amazon Redshift security group for a previously authorized IP range or Amazon EC2 security group. To add an ingress rule, see 'AuthorizeClusterSecurityGroupIngress' . For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
  ( -- * Creating a request
    RevokeClusterSecurityGroupIngress (..),
    mkRevokeClusterSecurityGroupIngress,

    -- ** Request lenses
    rcsgiEC2SecurityGroupOwnerId,
    rcsgiClusterSecurityGroupName,
    rcsgiEC2SecurityGroupName,
    rcsgiCIdRIP,

    -- * Destructuring the response
    RevokeClusterSecurityGroupIngressResponse (..),
    mkRevokeClusterSecurityGroupIngressResponse,

    -- ** Response lenses
    rcsgirsClusterSecurityGroup,
    rcsgirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRevokeClusterSecurityGroupIngress' smart constructor.
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress'
  { -- | The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided.
    --
    -- Example: @111122223333@
    ec2SecurityGroupOwnerId :: Lude.Maybe Lude.Text,
    -- | The name of the security Group from which to revoke the ingress rule.
    clusterSecurityGroupName :: Lude.Text,
    -- | The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided.
    ec2SecurityGroupName :: Lude.Maybe Lude.Text,
    -- | The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided.
    cIdRIP :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeClusterSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'ec2SecurityGroupOwnerId' - The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided.
--
-- Example: @111122223333@
-- * 'clusterSecurityGroupName' - The name of the security Group from which to revoke the ingress rule.
-- * 'ec2SecurityGroupName' - The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided.
-- * 'cIdRIP' - The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided.
mkRevokeClusterSecurityGroupIngress ::
  -- | 'clusterSecurityGroupName'
  Lude.Text ->
  RevokeClusterSecurityGroupIngress
mkRevokeClusterSecurityGroupIngress pClusterSecurityGroupName_ =
  RevokeClusterSecurityGroupIngress'
    { ec2SecurityGroupOwnerId =
        Lude.Nothing,
      clusterSecurityGroupName = pClusterSecurityGroupName_,
      ec2SecurityGroupName = Lude.Nothing,
      cIdRIP = Lude.Nothing
    }

-- | The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided.
--
-- Example: @111122223333@
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
rcsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: RevokeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: RevokeClusterSecurityGroupIngress)
{-# DEPRECATED rcsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The name of the security Group from which to revoke the ingress rule.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiClusterSecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress Lude.Text
rcsgiClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: RevokeClusterSecurityGroupIngress -> Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: RevokeClusterSecurityGroupIngress)
{-# DEPRECATED rcsgiClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
rcsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: RevokeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: RevokeClusterSecurityGroupIngress)
{-# DEPRECATED rcsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

-- | The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiCIdRIP :: Lens.Lens' RevokeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
rcsgiCIdRIP = Lens.lens (cIdRIP :: RevokeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: RevokeClusterSecurityGroupIngress)
{-# DEPRECATED rcsgiCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

instance Lude.AWSRequest RevokeClusterSecurityGroupIngress where
  type
    Rs RevokeClusterSecurityGroupIngress =
      RevokeClusterSecurityGroupIngressResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RevokeClusterSecurityGroupIngressResult"
      ( \s h x ->
          RevokeClusterSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "ClusterSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeClusterSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeClusterSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeClusterSecurityGroupIngress where
  toQuery RevokeClusterSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RevokeClusterSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "ClusterSecurityGroupName" Lude.=: clusterSecurityGroupName,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName,
        "CIDRIP" Lude.=: cIdRIP
      ]

-- | /See:/ 'mkRevokeClusterSecurityGroupIngressResponse' smart constructor.
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Lude.Maybe ClusterSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeClusterSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkRevokeClusterSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeClusterSecurityGroupIngressResponse
mkRevokeClusterSecurityGroupIngressResponse pResponseStatus_ =
  RevokeClusterSecurityGroupIngressResponse'
    { clusterSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirsClusterSecurityGroup :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse (Lude.Maybe ClusterSecurityGroup)
rcsgirsClusterSecurityGroup = Lens.lens (clusterSecurityGroup :: RevokeClusterSecurityGroupIngressResponse -> Lude.Maybe ClusterSecurityGroup) (\s a -> s {clusterSecurityGroup = a} :: RevokeClusterSecurityGroupIngressResponse)
{-# DEPRECATED rcsgirsClusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirsResponseStatus :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse Lude.Int
rcsgirsResponseStatus = Lens.lens (responseStatus :: RevokeClusterSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeClusterSecurityGroupIngressResponse)
{-# DEPRECATED rcsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
