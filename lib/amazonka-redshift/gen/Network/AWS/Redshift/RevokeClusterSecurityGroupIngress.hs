{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RevokeClusterSecurityGroupIngress (..)
    , mkRevokeClusterSecurityGroupIngress
    -- ** Request lenses
    , rcsgiClusterSecurityGroupName
    , rcsgiCIDRIP
    , rcsgiEC2SecurityGroupName
    , rcsgiEC2SecurityGroupOwnerId

    -- * Destructuring the response
    , RevokeClusterSecurityGroupIngressResponse (..)
    , mkRevokeClusterSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirrsClusterSecurityGroup
    , rcsgirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRevokeClusterSecurityGroupIngress' smart constructor.
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress'
  { clusterSecurityGroupName :: Core.Text
    -- ^ The name of the security Group from which to revoke the ingress rule.
  , cidrip :: Core.Maybe Core.Text
    -- ^ The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided. 
  , eC2SecurityGroupName :: Core.Maybe Core.Text
    -- ^ The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided. 
  , eC2SecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided. 
--
-- Example: @111122223333@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClusterSecurityGroupIngress' value with any optional fields omitted.
mkRevokeClusterSecurityGroupIngress
    :: Core.Text -- ^ 'clusterSecurityGroupName'
    -> RevokeClusterSecurityGroupIngress
mkRevokeClusterSecurityGroupIngress clusterSecurityGroupName
  = RevokeClusterSecurityGroupIngress'{clusterSecurityGroupName,
                                       cidrip = Core.Nothing, eC2SecurityGroupName = Core.Nothing,
                                       eC2SecurityGroupOwnerId = Core.Nothing}

-- | The name of the security Group from which to revoke the ingress rule.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiClusterSecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress Core.Text
rcsgiClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# INLINEABLE rcsgiClusterSecurityGroupName #-}
{-# DEPRECATED clusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead"  #-}

-- | The IP range for which to revoke access. This range must be a valid Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@ is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@ cannot be provided. 
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiCIDRIP :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
rcsgiCIDRIP = Lens.field @"cidrip"
{-# INLINEABLE rcsgiCIDRIP #-}
{-# DEPRECATED cidrip "Use generic-lens or generic-optics with 'cidrip' instead"  #-}

-- | The name of the EC2 Security Group whose access is to be revoked. If @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also be provided and @CIDRIP@ cannot be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
rcsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE rcsgiEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | The AWS account number of the owner of the security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an acceptable value. If @EC2SecurityGroupOwnerId@ is specified, @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be provided. 
--
-- Example: @111122223333@ 
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
rcsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE rcsgiEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

instance Core.ToQuery RevokeClusterSecurityGroupIngress where
        toQuery RevokeClusterSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("RevokeClusterSecurityGroupIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSecurityGroupName"
                clusterSecurityGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "CIDRIP") cidrip
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EC2SecurityGroupName")
                eC2SecurityGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EC2SecurityGroupOwnerId")
                eC2SecurityGroupOwnerId

instance Core.ToHeaders RevokeClusterSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RevokeClusterSecurityGroupIngress where
        type Rs RevokeClusterSecurityGroupIngress =
             RevokeClusterSecurityGroupIngressResponse
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
              "RevokeClusterSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeClusterSecurityGroupIngressResponse' Core.<$>
                   (x Core..@? "ClusterSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeClusterSecurityGroupIngressResponse' smart constructor.
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Core.Maybe Types.ClusterSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClusterSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeClusterSecurityGroupIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeClusterSecurityGroupIngressResponse
mkRevokeClusterSecurityGroupIngressResponse responseStatus
  = RevokeClusterSecurityGroupIngressResponse'{clusterSecurityGroup =
                                                 Core.Nothing,
                                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsClusterSecurityGroup :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse (Core.Maybe Types.ClusterSecurityGroup)
rcsgirrsClusterSecurityGroup = Lens.field @"clusterSecurityGroup"
{-# INLINEABLE rcsgirrsClusterSecurityGroup #-}
{-# DEPRECATED clusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcsgirrsResponseStatus :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse Core.Int
rcsgirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcsgirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
