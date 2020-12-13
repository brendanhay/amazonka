{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an inbound (ingress) rule to an Amazon Redshift security group. Depending on whether the application accessing your cluster is running on the Internet or an Amazon EC2 instance, you can authorize inbound access to either a Classless Interdomain Routing (CIDR)/Internet Protocol (IP) range or to an Amazon EC2 security group. You can add as many as 20 ingress rules to an Amazon Redshift security group.
--
-- If you authorize access to an Amazon EC2 security group, specify /EC2SecurityGroupName/ and /EC2SecurityGroupOwnerId/ . The Amazon EC2 security group and Amazon Redshift cluster must be in the same AWS Region.
-- If you authorize access to a CIDR/IP address range, specify /CIDRIP/ . For an overview of CIDR blocks, see the Wikipedia article on <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
-- You must also associate the security group with a cluster so that clients running on these IP addresses or the EC2 instance are authorized to connect to the cluster. For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Working with Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
  ( -- * Creating a request
    AuthorizeClusterSecurityGroupIngress (..),
    mkAuthorizeClusterSecurityGroupIngress,

    -- ** Request lenses
    acsgiEC2SecurityGroupOwnerId,
    acsgiClusterSecurityGroupName,
    acsgiEC2SecurityGroupName,
    acsgiCIdRIP,

    -- * Destructuring the response
    AuthorizeClusterSecurityGroupIngressResponse (..),
    mkAuthorizeClusterSecurityGroupIngressResponse,

    -- ** Response lenses
    acsgirsClusterSecurityGroup,
    acsgirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAuthorizeClusterSecurityGroupIngress' smart constructor.
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
  { -- | The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.
    --
    -- Example: @111122223333@
    ec2SecurityGroupOwnerId :: Lude.Maybe Lude.Text,
    -- | The name of the security group to which the ingress rule is added.
    clusterSecurityGroupName :: Lude.Text,
    -- | The EC2 security group to be added the Amazon Redshift security group.
    ec2SecurityGroupName :: Lude.Maybe Lude.Text,
    -- | The IP range to be added the Amazon Redshift security group.
    cIdRIP :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeClusterSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'ec2SecurityGroupOwnerId' - The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.
--
-- Example: @111122223333@
-- * 'clusterSecurityGroupName' - The name of the security group to which the ingress rule is added.
-- * 'ec2SecurityGroupName' - The EC2 security group to be added the Amazon Redshift security group.
-- * 'cIdRIP' - The IP range to be added the Amazon Redshift security group.
mkAuthorizeClusterSecurityGroupIngress ::
  -- | 'clusterSecurityGroupName'
  Lude.Text ->
  AuthorizeClusterSecurityGroupIngress
mkAuthorizeClusterSecurityGroupIngress pClusterSecurityGroupName_ =
  AuthorizeClusterSecurityGroupIngress'
    { ec2SecurityGroupOwnerId =
        Lude.Nothing,
      clusterSecurityGroupName = pClusterSecurityGroupName_,
      ec2SecurityGroupName = Lude.Nothing,
      cIdRIP = Lude.Nothing
    }

-- | The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.
--
-- Example: @111122223333@
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
acsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: AuthorizeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: AuthorizeClusterSecurityGroupIngress)
{-# DEPRECATED acsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The name of the security group to which the ingress rule is added.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiClusterSecurityGroupName :: Lens.Lens' AuthorizeClusterSecurityGroupIngress Lude.Text
acsgiClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: AuthorizeClusterSecurityGroupIngress -> Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: AuthorizeClusterSecurityGroupIngress)
{-# DEPRECATED acsgiClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | The EC2 security group to be added the Amazon Redshift security group.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
acsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: AuthorizeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: AuthorizeClusterSecurityGroupIngress)
{-# DEPRECATED acsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

-- | The IP range to be added the Amazon Redshift security group.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiCIdRIP :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Lude.Maybe Lude.Text)
acsgiCIdRIP = Lens.lens (cIdRIP :: AuthorizeClusterSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: AuthorizeClusterSecurityGroupIngress)
{-# DEPRECATED acsgiCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

instance Lude.AWSRequest AuthorizeClusterSecurityGroupIngress where
  type
    Rs AuthorizeClusterSecurityGroupIngress =
      AuthorizeClusterSecurityGroupIngressResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "AuthorizeClusterSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeClusterSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "ClusterSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeClusterSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeClusterSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeClusterSecurityGroupIngress where
  toQuery AuthorizeClusterSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AuthorizeClusterSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "ClusterSecurityGroupName" Lude.=: clusterSecurityGroupName,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName,
        "CIDRIP" Lude.=: cIdRIP
      ]

-- | /See:/ 'mkAuthorizeClusterSecurityGroupIngressResponse' smart constructor.
data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Lude.Maybe ClusterSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeClusterSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkAuthorizeClusterSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeClusterSecurityGroupIngressResponse
mkAuthorizeClusterSecurityGroupIngressResponse pResponseStatus_ =
  AuthorizeClusterSecurityGroupIngressResponse'
    { clusterSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirsClusterSecurityGroup :: Lens.Lens' AuthorizeClusterSecurityGroupIngressResponse (Lude.Maybe ClusterSecurityGroup)
acsgirsClusterSecurityGroup = Lens.lens (clusterSecurityGroup :: AuthorizeClusterSecurityGroupIngressResponse -> Lude.Maybe ClusterSecurityGroup) (\s a -> s {clusterSecurityGroup = a} :: AuthorizeClusterSecurityGroupIngressResponse)
{-# DEPRECATED acsgirsClusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirsResponseStatus :: Lens.Lens' AuthorizeClusterSecurityGroupIngressResponse Lude.Int
acsgirsResponseStatus = Lens.lens (responseStatus :: AuthorizeClusterSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeClusterSecurityGroupIngressResponse)
{-# DEPRECATED acsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
