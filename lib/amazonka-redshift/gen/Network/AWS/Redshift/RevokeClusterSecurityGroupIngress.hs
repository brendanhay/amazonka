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
    rcsgiClusterSecurityGroupName,
    rcsgiCIDRIP,
    rcsgiEC2SecurityGroupName,
    rcsgiEC2SecurityGroupOwnerId,

    -- * Destructuring the response
    RevokeClusterSecurityGroupIngressResponse (..),
    mkRevokeClusterSecurityGroupIngressResponse,

    -- ** Response lenses
    rcsgirrsClusterSecurityGroup,
    rcsgirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRevokeClusterSecurityGroupIngress' smart constructor.
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress'
  { -- | The name of the security Group from which to revoke the ingress rule.
    clusterSecurityGroupName :: Types.ClusterSecurityGroupName,
    -- | The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided.
    cidrip :: Core.Maybe Types.CIDRIP,
    -- | The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided.
    eC2SecurityGroupName :: Core.Maybe Types.EC2SecurityGroupName,
    -- | The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided.
    --
    -- Example: @111122223333@
    eC2SecurityGroupOwnerId :: Core.Maybe Types.EC2SecurityGroupOwnerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClusterSecurityGroupIngress' value with any optional fields omitted.
mkRevokeClusterSecurityGroupIngress ::
  -- | 'clusterSecurityGroupName'
  Types.ClusterSecurityGroupName ->
  RevokeClusterSecurityGroupIngress
mkRevokeClusterSecurityGroupIngress clusterSecurityGroupName =
  RevokeClusterSecurityGroupIngress'
    { clusterSecurityGroupName,
      cidrip = Core.Nothing,
      eC2SecurityGroupName = Core.Nothing,
      eC2SecurityGroupOwnerId = Core.Nothing
    }

-- | The name of the security Group from which to revoke the ingress rule.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiClusterSecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress Types.ClusterSecurityGroupName
rcsgiClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# DEPRECATED rcsgiClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiCIDRIP :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Types.CIDRIP)
rcsgiCIDRIP = Lens.field @"cidrip"
{-# DEPRECATED rcsgiCIDRIP "Use generic-lens or generic-optics with 'cidrip' instead." #-}

-- | The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Types.EC2SecurityGroupName)
rcsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED rcsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided.
--
-- Example: @111122223333@
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Types.EC2SecurityGroupOwnerId)
rcsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED rcsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

instance Core.AWSRequest RevokeClusterSecurityGroupIngress where
  type
    Rs RevokeClusterSecurityGroupIngress =
      RevokeClusterSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "RevokeClusterSecurityGroupIngress")
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
      "RevokeClusterSecurityGroupIngressResult"
      ( \s h x ->
          RevokeClusterSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "ClusterSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRevokeClusterSecurityGroupIngressResponse' smart constructor.
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Core.Maybe Types.ClusterSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClusterSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeClusterSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RevokeClusterSecurityGroupIngressResponse
mkRevokeClusterSecurityGroupIngressResponse responseStatus =
  RevokeClusterSecurityGroupIngressResponse'
    { clusterSecurityGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsClusterSecurityGroup :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse (Core.Maybe Types.ClusterSecurityGroup)
rcsgirrsClusterSecurityGroup = Lens.field @"clusterSecurityGroup"
{-# DEPRECATED rcsgirrsClusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsResponseStatus :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse Core.Int
rcsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
