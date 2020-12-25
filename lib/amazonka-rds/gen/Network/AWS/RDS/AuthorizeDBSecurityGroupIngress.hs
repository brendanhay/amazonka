{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AuthorizeDBSecurityGroupIngress (..),
    mkAuthorizeDBSecurityGroupIngress,

    -- ** Request lenses
    adbsgiDBSecurityGroupName,
    adbsgiCIDRIP,
    adbsgiEC2SecurityGroupId,
    adbsgiEC2SecurityGroupName,
    adbsgiEC2SecurityGroupOwnerId,

    -- * Destructuring the response
    AuthorizeDBSecurityGroupIngressResponse (..),
    mkAuthorizeDBSecurityGroupIngressResponse,

    -- ** Response lenses
    adbsgirrsDBSecurityGroup,
    adbsgirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkAuthorizeDBSecurityGroupIngress' smart constructor.
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
  { -- | The name of the DB security group to add authorization to.
    dBSecurityGroupName :: Types.String,
    -- | The IP range to authorize.
    cidrip :: Core.Maybe Types.String,
    -- | Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupId :: Core.Maybe Types.String,
    -- | Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupName :: Core.Maybe Types.String,
    -- | AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupOwnerId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeDBSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeDBSecurityGroupIngress ::
  -- | 'dBSecurityGroupName'
  Types.String ->
  AuthorizeDBSecurityGroupIngress
mkAuthorizeDBSecurityGroupIngress dBSecurityGroupName =
  AuthorizeDBSecurityGroupIngress'
    { dBSecurityGroupName,
      cidrip = Core.Nothing,
      eC2SecurityGroupId = Core.Nothing,
      eC2SecurityGroupName = Core.Nothing,
      eC2SecurityGroupOwnerId = Core.Nothing
    }

-- | The name of the DB security group to add authorization to.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiDBSecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress Types.String
adbsgiDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# DEPRECATED adbsgiDBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead." #-}

-- | The IP range to authorize.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiCIDRIP :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Types.String)
adbsgiCIDRIP = Lens.field @"cidrip"
{-# DEPRECATED adbsgiCIDRIP "Use generic-lens or generic-optics with 'cidrip' instead." #-}

-- | Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'eC2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Types.String)
adbsgiEC2SecurityGroupId = Lens.field @"eC2SecurityGroupId"
{-# DEPRECATED adbsgiEC2SecurityGroupId "Use generic-lens or generic-optics with 'eC2SecurityGroupId' instead." #-}

-- | Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'eC2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Types.String)
adbsgiEC2SecurityGroupName = Lens.field @"eC2SecurityGroupName"
{-# DEPRECATED adbsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'eC2SecurityGroupName' instead." #-}

-- | AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'eC2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Core.Maybe Types.String)
adbsgiEC2SecurityGroupOwnerId = Lens.field @"eC2SecurityGroupOwnerId"
{-# DEPRECATED adbsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'eC2SecurityGroupOwnerId' instead." #-}

instance Core.AWSRequest AuthorizeDBSecurityGroupIngress where
  type
    Rs AuthorizeDBSecurityGroupIngress =
      AuthorizeDBSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "AuthorizeDBSecurityGroupIngress")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSecurityGroupName" dBSecurityGroupName)
                Core.<> (Core.toQueryValue "CIDRIP" Core.<$> cidrip)
                Core.<> ( Core.toQueryValue "EC2SecurityGroupId"
                            Core.<$> eC2SecurityGroupId
                        )
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
      "AuthorizeDBSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeDBSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "DBSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAuthorizeDBSecurityGroupIngressResponse' smart constructor.
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
  { dBSecurityGroup :: Core.Maybe Types.DBSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeDBSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeDBSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AuthorizeDBSecurityGroupIngressResponse
mkAuthorizeDBSecurityGroupIngressResponse responseStatus =
  AuthorizeDBSecurityGroupIngressResponse'
    { dBSecurityGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgirrsDBSecurityGroup :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse (Core.Maybe Types.DBSecurityGroup)
adbsgirrsDBSecurityGroup = Lens.field @"dBSecurityGroup"
{-# DEPRECATED adbsgirrsDBSecurityGroup "Use generic-lens or generic-optics with 'dBSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adbsgirrsResponseStatus :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse Core.Int
adbsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED adbsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
