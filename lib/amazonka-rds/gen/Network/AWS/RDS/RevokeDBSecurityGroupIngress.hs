{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a DBSecurityGroup for previously authorized IP ranges or EC2 or VPC Security Groups. Required parameters for this API are one of CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or EC2SecurityGroupId).
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
    (
    -- * Creating a request
      RevokeDBSecurityGroupIngress (..)
    , mkRevokeDBSecurityGroupIngress
    -- ** Request lenses
    , rdbsgiDBSecurityGroupName
    , rdbsgiCIDRIP
    , rdbsgiEC2SecurityGroupId
    , rdbsgiEC2SecurityGroupName
    , rdbsgiEC2SecurityGroupOwnerId

    -- * Destructuring the response
    , RevokeDBSecurityGroupIngressResponse (..)
    , mkRevokeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , rdbsgirrsDBSecurityGroup
    , rdbsgirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRevokeDBSecurityGroupIngress' smart constructor.
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress'
  { dBSecurityGroupName :: Core.Text
    -- ^ The name of the DB security group to revoke ingress from.
  , cidrip :: Core.Maybe Core.Text
    -- ^ The IP range to revoke access from. Must be a valid CIDR range. If @CIDRIP@ is specified, @EC2SecurityGroupName@ , @EC2SecurityGroupId@ and @EC2SecurityGroupOwnerId@ can't be provided. 
  , eC2SecurityGroupId :: Core.Maybe Core.Text
    -- ^ The id of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  , eC2SecurityGroupName :: Core.Maybe Core.Text
    -- ^ The name of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  , eC2SecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeDBSecurityGroupIngress' value with any optional fields omitted.
mkRevokeDBSecurityGroupIngress
    :: Core.Text -- ^ 'dBSecurityGroupName'
    -> RevokeDBSecurityGroupIngress
mkRevokeDBSecurityGroupIngress dBSecurityGroupName
  = RevokeDBSecurityGroupIngress'{dBSecurityGroupName,
                                  cidrip = Core.Nothing, eC2SecurityGroupId = Core.Nothing,
                                  eC2SecurityGroupName = Core.Nothing,
                                  eC2SecurityGroupOwnerId = Core.Nothing}

-- | The name of the DB security group to revoke ingress from.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgiDBSecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress Core.Text
rdbsgiDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE rdbsgiDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

-- | The IP range to revoke access from. Must be a valid CIDR range. If @CIDRIP@ is specified, @EC2SecurityGroupName@ , @EC2SecurityGroupId@ and @EC2SecurityGroupOwnerId@ can't be provided. 
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgiCIDRIP :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
rdbsgiCIDRIP = Lens.field @"cidrip"
{-# INLINEABLE rdbsgiCIDRIP #-}
{-# DEPRECATED cidrip "Use generic-lens or generic-optics with 'cidrip' instead"  #-}

-- | The id of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgiEC2SecurityGroupId :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
rdbsgiEC2SecurityGroupId = Lens.field @"eC2SecurityGroupId"
{-# INLINEABLE rdbsgiEC2SecurityGroupId #-}
{-# DEPRECATED eC2SecurityGroupId "Use generic-lens or generic-optics with 'eC2SecurityGroupId' instead"  #-}

-- | The name of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgiEC2SecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
rdbsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE rdbsgiEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | The AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
rdbsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE rdbsgiEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

instance Core.ToQuery RevokeDBSecurityGroupIngress where
        toQuery RevokeDBSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("RevokeDBSecurityGroupIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBSecurityGroupName" dBSecurityGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "CIDRIP") cidrip
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EC2SecurityGroupId")
                eC2SecurityGroupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EC2SecurityGroupName")
                eC2SecurityGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EC2SecurityGroupOwnerId")
                eC2SecurityGroupOwnerId

instance Core.ToHeaders RevokeDBSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RevokeDBSecurityGroupIngress where
        type Rs RevokeDBSecurityGroupIngress =
             RevokeDBSecurityGroupIngressResponse
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
          = Response.receiveXMLWrapper "RevokeDBSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeDBSecurityGroupIngressResponse' Core.<$>
                   (x Core..@? "DBSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeDBSecurityGroupIngressResponse' smart constructor.
data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse'
  { dBSecurityGroup :: Core.Maybe Types.DBSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeDBSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeDBSecurityGroupIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeDBSecurityGroupIngressResponse
mkRevokeDBSecurityGroupIngressResponse responseStatus
  = RevokeDBSecurityGroupIngressResponse'{dBSecurityGroup =
                                            Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgirrsDBSecurityGroup :: Lens.Lens' RevokeDBSecurityGroupIngressResponse (Core.Maybe Types.DBSecurityGroup)
rdbsgirrsDBSecurityGroup = Lens.field @"dBSecurityGroup"
{-# INLINEABLE rdbsgirrsDBSecurityGroup #-}
{-# DEPRECATED dBSecurityGroup "Use generic-lens or generic-optics with 'dBSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbsgirrsResponseStatus :: Lens.Lens' RevokeDBSecurityGroupIngressResponse Core.Int
rdbsgirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdbsgirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
