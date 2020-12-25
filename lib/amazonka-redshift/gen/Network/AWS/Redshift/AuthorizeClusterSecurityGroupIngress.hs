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
    acsgiClusterSecurityGroupName,
    acsgiCIDRIP,
    acsgiEC2SecurityGroupName,
    acsgiEC2SecurityGroupOwnerId,

    -- * Destructuring the response
    AuthorizeClusterSecurityGroupIngressResponse (..),
    mkAuthorizeClusterSecurityGroupIngressResponse,

    -- ** Response lenses
    acsgirrsClusterSecurityGroup,
    acsgirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkAuthorizeClusterSecurityGroupIngress' smart constructor.
data AuthorizeClusterSecurityGroupIngress = AuthorizeClusterSecurityGroupIngress'
  { -- | The name of the security group to which the ingress rule is added.
    clusterSecurityGroupName :: Types.String,
    -- | The IP range to be added the Amazon Redshift security group.
    cidrip :: Core.Maybe Types.String,
    -- | The EC2 security group to be added the Amazon Redshift security group.
    eC2SecurityGroupName :: Core.Maybe Types.String,
    -- | The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.
    --
    -- Example: @111122223333@
    eC2SecurityGroupOwnerId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeClusterSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeClusterSecurityGroupIngress ::
  -- | 'clusterSecurityGroupName'
  Types.String ->
  AuthorizeClusterSecurityGroupIngress
mkAuthorizeClusterSecurityGroupIngress clusterSecurityGroupName =
  AuthorizeClusterSecurityGroupIngress'
    { clusterSecurityGroupName,
      cidrip = Core.Nothing,
      eC2SecurityGroupName = Core.Nothing,
      eC2SecurityGroupOwnerId = Core.Nothing
    }

-- | The name of the security group to which the ingress rule is added.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiClusterSecurityGroupName :: Lens.Lens' AuthorizeClusterSecurityGroupIngress Types.String
acsgiClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# DEPRECATED acsgiClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | The IP range to be added the Amazon Redshift security group.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiCIDRIP :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Core.Maybe Types.String)
acsgiCIDRIP = Lens.field @"cidrip"
{-# DEPRECATED acsgiCIDRIP "Use generic-lens or generic-optics with 'cidrip' instead." #-}

-- | The EC2 security group to be added the Amazon Redshift security group.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Core.Maybe Types.String)
acsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED acsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | The AWS account number of the owner of the security group specified by the /EC2SecurityGroupName/ parameter. The AWS Access Key ID is not an acceptable value.
--
-- Example: @111122223333@
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeClusterSecurityGroupIngress (Core.Maybe Types.String)
acsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED acsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

instance Core.AWSRequest AuthorizeClusterSecurityGroupIngress where
  type
    Rs AuthorizeClusterSecurityGroupIngress =
      AuthorizeClusterSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "AuthorizeClusterSecurityGroupIngress")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue
                            "ClusterSecurityGroupName"
                            clusterSecurityGroupName
                        )
                Core.<> (Core.toQueryValue "CIDRIP" Core.<$> cidrip)
                Core.<> ( Core.toQueryValue "EC2SecurityGroupName"
                            Core.<$> eC2SecurityGroupName
                        )
                Core.<> ( Core.toQueryValue "EC2SecurityGroupOwnerId"
                            Core.<$> eC2SecurityGroupOwnerId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "AuthorizeClusterSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeClusterSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "ClusterSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAuthorizeClusterSecurityGroupIngressResponse' smart constructor.
data AuthorizeClusterSecurityGroupIngressResponse = AuthorizeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Core.Maybe Types.ClusterSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeClusterSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeClusterSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AuthorizeClusterSecurityGroupIngressResponse
mkAuthorizeClusterSecurityGroupIngressResponse responseStatus =
  AuthorizeClusterSecurityGroupIngressResponse'
    { clusterSecurityGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsClusterSecurityGroup :: Lens.Lens' AuthorizeClusterSecurityGroupIngressResponse (Core.Maybe Types.ClusterSecurityGroup)
acsgirrsClusterSecurityGroup = Lens.field @"clusterSecurityGroup"
{-# DEPRECATED acsgirrsClusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsgirrsResponseStatus :: Lens.Lens' AuthorizeClusterSecurityGroupIngressResponse Core.Int
acsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
