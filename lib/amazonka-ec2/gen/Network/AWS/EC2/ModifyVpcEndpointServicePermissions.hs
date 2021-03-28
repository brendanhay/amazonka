{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcEndpointServicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the permissions for your <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC endpoint service> . You can add or remove permissions for service consumers (IAM users, IAM roles, and AWS accounts) to connect to your endpoint service.
--
-- If you grant permissions to all principals, the service is public. Any users who know the name of a public service can send a request to attach an endpoint. If the service does not require manual approval, attachments are automatically approved.
module Network.AWS.EC2.ModifyVpcEndpointServicePermissions
    (
    -- * Creating a request
      ModifyVpcEndpointServicePermissions (..)
    , mkModifyVpcEndpointServicePermissions
    -- ** Request lenses
    , mvespServiceId
    , mvespAddAllowedPrincipals
    , mvespDryRun
    , mvespRemoveAllowedPrincipals

    -- * Destructuring the response
    , ModifyVpcEndpointServicePermissionsResponse (..)
    , mkModifyVpcEndpointServicePermissionsResponse
    -- ** Response lenses
    , mvesprrsReturnValue
    , mvesprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcEndpointServicePermissions' smart constructor.
data ModifyVpcEndpointServicePermissions = ModifyVpcEndpointServicePermissions'
  { serviceId :: Types.VpcEndpointServiceId
    -- ^ The ID of the service.
  , addAllowedPrincipals :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARN) of one or more principals. Permissions are granted to the principals in this list. To grant permissions to all principals, specify an asterisk (*).
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , removeAllowedPrincipals :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Names (ARN) of one or more principals. Permissions are revoked for principals in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointServicePermissions' value with any optional fields omitted.
mkModifyVpcEndpointServicePermissions
    :: Types.VpcEndpointServiceId -- ^ 'serviceId'
    -> ModifyVpcEndpointServicePermissions
mkModifyVpcEndpointServicePermissions serviceId
  = ModifyVpcEndpointServicePermissions'{serviceId,
                                         addAllowedPrincipals = Core.Nothing, dryRun = Core.Nothing,
                                         removeAllowedPrincipals = Core.Nothing}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespServiceId :: Lens.Lens' ModifyVpcEndpointServicePermissions Types.VpcEndpointServiceId
mvespServiceId = Lens.field @"serviceId"
{-# INLINEABLE mvespServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions are granted to the principals in this list. To grant permissions to all principals, specify an asterisk (*).
--
-- /Note:/ Consider using 'addAllowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespAddAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Core.Maybe [Core.Text])
mvespAddAllowedPrincipals = Lens.field @"addAllowedPrincipals"
{-# INLINEABLE mvespAddAllowedPrincipals #-}
{-# DEPRECATED addAllowedPrincipals "Use generic-lens or generic-optics with 'addAllowedPrincipals' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespDryRun :: Lens.Lens' ModifyVpcEndpointServicePermissions (Core.Maybe Core.Bool)
mvespDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvespDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions are revoked for principals in this list.
--
-- /Note:/ Consider using 'removeAllowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespRemoveAllowedPrincipals :: Lens.Lens' ModifyVpcEndpointServicePermissions (Core.Maybe [Core.Text])
mvespRemoveAllowedPrincipals = Lens.field @"removeAllowedPrincipals"
{-# INLINEABLE mvespRemoveAllowedPrincipals #-}
{-# DEPRECATED removeAllowedPrincipals "Use generic-lens or generic-optics with 'removeAllowedPrincipals' instead"  #-}

instance Core.ToQuery ModifyVpcEndpointServicePermissions where
        toQuery ModifyVpcEndpointServicePermissions{..}
          = Core.toQueryPair "Action"
              ("ModifyVpcEndpointServicePermissions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddAllowedPrincipals")
                addAllowedPrincipals
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveAllowedPrincipals")
                removeAllowedPrincipals

instance Core.ToHeaders ModifyVpcEndpointServicePermissions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcEndpointServicePermissions where
        type Rs ModifyVpcEndpointServicePermissions =
             ModifyVpcEndpointServicePermissionsResponse
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
          = Response.receiveXML
              (\ s h x ->
                 ModifyVpcEndpointServicePermissionsResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcEndpointServicePermissionsResponse' smart constructor.
data ModifyVpcEndpointServicePermissionsResponse = ModifyVpcEndpointServicePermissionsResponse'
  { returnValue :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointServicePermissionsResponse' value with any optional fields omitted.
mkModifyVpcEndpointServicePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcEndpointServicePermissionsResponse
mkModifyVpcEndpointServicePermissionsResponse responseStatus
  = ModifyVpcEndpointServicePermissionsResponse'{returnValue =
                                                   Core.Nothing,
                                                 responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvesprrsReturnValue :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse (Core.Maybe Core.Bool)
mvesprrsReturnValue = Lens.field @"returnValue"
{-# INLINEABLE mvesprrsReturnValue #-}
{-# DEPRECATED returnValue "Use generic-lens or generic-optics with 'returnValue' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvesprrsResponseStatus :: Lens.Lens' ModifyVpcEndpointServicePermissionsResponse Core.Int
mvesprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvesprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
