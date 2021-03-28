{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables ingress to a DBSecurityGroup using one of two forms of authorization. First, EC2 or VPC security groups can be added to the DBSecurityGroup if the application using the database is running on EC2 or VPC instances. Second, IP ranges are available if the application accessing your database is running on the Internet. Required parameters for this API are one of CIDR range, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or EC2SecurityGroupId for non-VPC).
--
-- For an overview of CIDR ranges, go to the <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial> . 
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
    (
    -- * Creating a request
      AuthorizeDBSecurityGroupIngress (..)
    , mkAuthorizeDBSecurityGroupIngress
    -- ** Request lenses
    , adbsgiDBSecurityGroupName
    , adbsgiCIDRIP
    , adbsgiEC2SecurityGroupId
    , adbsgiEC2SecurityGroupName
    , adbsgiEC2SecurityGroupOwnerId

    -- * Destructuring the response
    , AuthorizeDBSecurityGroupIngressResponse (..)
    , mkAuthorizeDBSecurityGroupIngressResponse
    -- ** Response lenses
    , adbsgirrsDBSecurityGroup
    , adbsgirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkAuthorizeDBSecurityGroupIngress' smart constructor.
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
  { dBSecurityGroupName :: Core.Text
    -- ^ The name of the DB security group to add authorization to.
  , cidrip :: Core.Maybe Core.Text
    -- ^ The IP range to authorize.
  , eC2SecurityGroupId :: Core.Maybe Core.Text
    -- ^ Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  , eC2SecurityGroupName :: Core.Maybe Core.Text
    -- ^ Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  , eC2SecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeDBSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeDBSecurityGroupIngress
    :: Core.Text -- ^ 'dBSecurityGroupName'
    -> AuthorizeDBSecurityGroupIngress
mkAuthorizeDBSecurityGroupIngress dBSecurityGroupName
  = AuthorizeDBSecurityGroupIngress'{dBSecurityGroupName,
                                     cidrip = Core.Nothing, eC2SecurityGroupId = Core.Nothing,
                                     eC2SecurityGroupName = Core.Nothing,
                                     eC2SecurityGroupOwnerId = Core.Nothing}

-- | The name of the DB security group to add authorization to.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiDBSecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress Core.Text
adbsgiDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE adbsgiDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

-- | The IP range to authorize.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiCIDRIP :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Core.Text)
adbsgiCIDRIP = Lens.field @"cidrip"
{-# INLINEABLE adbsgiCIDRIP #-}
{-# DEPRECATED cidrip "Use generic-lens or generic-optics with 'cidrip' instead"  #-}

-- | Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Core.Text)
adbsgiEC2SecurityGroupId = Lens.field @"eC2SecurityGroupId"
{-# INLINEABLE adbsgiEC2SecurityGroupId #-}
{-# DEPRECATED eC2SecurityGroupId "Use generic-lens or generic-optics with 'eC2SecurityGroupId' instead"  #-}

-- | Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Core.Text)
adbsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# INLINEABLE adbsgiEC2SecurityGroupName #-}
{-# DEPRECATED eC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead"  #-}

-- | AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided. 
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Core.Text)
adbsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# INLINEABLE adbsgiEC2SecurityGroupOwnerId #-}
{-# DEPRECATED eC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead"  #-}

instance Core.ToQuery AuthorizeDBSecurityGroupIngress where
        toQuery AuthorizeDBSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("AuthorizeDBSecurityGroupIngress" :: Core.Text)
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

instance Core.ToHeaders AuthorizeDBSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AuthorizeDBSecurityGroupIngress where
        type Rs AuthorizeDBSecurityGroupIngress =
             AuthorizeDBSecurityGroupIngressResponse
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
              "AuthorizeDBSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeDBSecurityGroupIngressResponse' Core.<$>
                   (x Core..@? "DBSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeDBSecurityGroupIngressResponse' smart constructor.
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
  { dBSecurityGroup :: Core.Maybe Types.DBSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeDBSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeDBSecurityGroupIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AuthorizeDBSecurityGroupIngressResponse
mkAuthorizeDBSecurityGroupIngressResponse responseStatus
  = AuthorizeDBSecurityGroupIngressResponse'{dBSecurityGroup =
                                               Core.Nothing,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgirrsDBSecurityGroup :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse (Core.Maybe Types.DBSecurityGroup)
adbsgirrsDBSecurityGroup = Lens.field @"dBSecurityGroup"
{-# INLINEABLE adbsgirrsDBSecurityGroup #-}
{-# DEPRECATED dBSecurityGroup "Use generic-lens or generic-optics with 'dBSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgirrsResponseStatus :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse Core.Int
adbsgirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adbsgirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
