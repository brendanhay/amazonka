{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS-authorized account permission to attach the specified network interface to an instance in their account.
--
-- You can grant permission to a single AWS account only, and only one account at a time.
module Network.AWS.EC2.CreateNetworkInterfacePermission
    (
    -- * Creating a request
      CreateNetworkInterfacePermission (..)
    , mkCreateNetworkInterfacePermission
    -- ** Request lenses
    , cnipNetworkInterfaceId
    , cnipPermission
    , cnipAwsAccountId
    , cnipAwsService
    , cnipDryRun

    -- * Destructuring the response
    , CreateNetworkInterfacePermissionResponse (..)
    , mkCreateNetworkInterfacePermissionResponse
    -- ** Response lenses
    , cniprrsInterfacePermission
    , cniprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , permission :: Types.InterfacePermissionType
    -- ^ The type of permission to grant.
  , awsAccountId :: Core.Maybe Core.Text
    -- ^ The AWS account ID.
  , awsService :: Core.Maybe Core.Text
    -- ^ The AWS service. Currently not supported.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkInterfacePermission' value with any optional fields omitted.
mkCreateNetworkInterfacePermission
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> Types.InterfacePermissionType -- ^ 'permission'
    -> CreateNetworkInterfacePermission
mkCreateNetworkInterfacePermission networkInterfaceId permission
  = CreateNetworkInterfacePermission'{networkInterfaceId, permission,
                                      awsAccountId = Core.Nothing, awsService = Core.Nothing,
                                      dryRun = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipNetworkInterfaceId :: Lens.Lens' CreateNetworkInterfacePermission Types.NetworkInterfaceId
cnipNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE cnipNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The type of permission to grant.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipPermission :: Lens.Lens' CreateNetworkInterfacePermission Types.InterfacePermissionType
cnipPermission = Lens.field @"permission"
{-# INLINEABLE cnipPermission #-}
{-# DEPRECATED permission "Use generic-lens or generic-optics with 'permission' instead"  #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAwsAccountId :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Text)
cnipAwsAccountId = Lens.field @"awsAccountId"
{-# INLINEABLE cnipAwsAccountId #-}
{-# DEPRECATED awsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead"  #-}

-- | The AWS service. Currently not supported.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAwsService :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Text)
cnipAwsService = Lens.field @"awsService"
{-# INLINEABLE cnipAwsService #-}
{-# DEPRECATED awsService "Use generic-lens or generic-optics with 'awsService' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipDryRun :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Bool)
cnipDryRun = Lens.field @"dryRun"
{-# INLINEABLE cnipDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateNetworkInterfacePermission where
        toQuery CreateNetworkInterfacePermission{..}
          = Core.toQueryPair "Action"
              ("CreateNetworkInterfacePermission" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<> Core.toQueryPair "Permission" permission
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AwsAccountId")
                awsAccountId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AwsService") awsService
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateNetworkInterfacePermission where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateNetworkInterfacePermission where
        type Rs CreateNetworkInterfacePermission =
             CreateNetworkInterfacePermissionResponse
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
                 CreateNetworkInterfacePermissionResponse' Core.<$>
                   (x Core..@? "interfacePermission") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermissionResponse' smart constructor.
data CreateNetworkInterfacePermissionResponse = CreateNetworkInterfacePermissionResponse'
  { interfacePermission :: Core.Maybe Types.NetworkInterfacePermission
    -- ^ Information about the permission for the network interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkInterfacePermissionResponse' value with any optional fields omitted.
mkCreateNetworkInterfacePermissionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNetworkInterfacePermissionResponse
mkCreateNetworkInterfacePermissionResponse responseStatus
  = CreateNetworkInterfacePermissionResponse'{interfacePermission =
                                                Core.Nothing,
                                              responseStatus}

-- | Information about the permission for the network interface.
--
-- /Note:/ Consider using 'interfacePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprrsInterfacePermission :: Lens.Lens' CreateNetworkInterfacePermissionResponse (Core.Maybe Types.NetworkInterfacePermission)
cniprrsInterfacePermission = Lens.field @"interfacePermission"
{-# INLINEABLE cniprrsInterfacePermission #-}
{-# DEPRECATED interfacePermission "Use generic-lens or generic-optics with 'interfacePermission' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprrsResponseStatus :: Lens.Lens' CreateNetworkInterfacePermissionResponse Core.Int
cniprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cniprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
