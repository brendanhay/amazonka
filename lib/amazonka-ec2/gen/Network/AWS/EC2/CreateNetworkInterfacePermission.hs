{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateNetworkInterfacePermission (..),
    mkCreateNetworkInterfacePermission,

    -- ** Request lenses
    cnipNetworkInterfaceId,
    cnipPermission,
    cnipAwsAccountId,
    cnipAwsService,
    cnipDryRun,

    -- * Destructuring the response
    CreateNetworkInterfacePermissionResponse (..),
    mkCreateNetworkInterfacePermissionResponse,

    -- ** Response lenses
    cniprrsInterfacePermission,
    cniprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { -- | The ID of the network interface.
    networkInterfaceId :: Types.NetworkInterfaceId,
    -- | The type of permission to grant.
    permission :: Types.InterfacePermissionType,
    -- | The AWS account ID.
    awsAccountId :: Core.Maybe Types.String,
    -- | The AWS service. Currently not supported.
    awsService :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkInterfacePermission' value with any optional fields omitted.
mkCreateNetworkInterfacePermission ::
  -- | 'networkInterfaceId'
  Types.NetworkInterfaceId ->
  -- | 'permission'
  Types.InterfacePermissionType ->
  CreateNetworkInterfacePermission
mkCreateNetworkInterfacePermission networkInterfaceId permission =
  CreateNetworkInterfacePermission'
    { networkInterfaceId,
      permission,
      awsAccountId = Core.Nothing,
      awsService = Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipNetworkInterfaceId :: Lens.Lens' CreateNetworkInterfacePermission Types.NetworkInterfaceId
cnipNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED cnipNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The type of permission to grant.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipPermission :: Lens.Lens' CreateNetworkInterfacePermission Types.InterfacePermissionType
cnipPermission = Lens.field @"permission"
{-# DEPRECATED cnipPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAwsAccountId :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Types.String)
cnipAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED cnipAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The AWS service. Currently not supported.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAwsService :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Types.String)
cnipAwsService = Lens.field @"awsService"
{-# DEPRECATED cnipAwsService "Use generic-lens or generic-optics with 'awsService' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipDryRun :: Lens.Lens' CreateNetworkInterfacePermission (Core.Maybe Core.Bool)
cnipDryRun = Lens.field @"dryRun"
{-# DEPRECATED cnipDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest CreateNetworkInterfacePermission where
  type
    Rs CreateNetworkInterfacePermission =
      CreateNetworkInterfacePermissionResponse
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
            ( Core.pure ("Action", "CreateNetworkInterfacePermission")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "NetworkInterfaceId" networkInterfaceId)
                Core.<> (Core.toQueryValue "Permission" permission)
                Core.<> (Core.toQueryValue "AwsAccountId" Core.<$> awsAccountId)
                Core.<> (Core.toQueryValue "AwsService" Core.<$> awsService)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInterfacePermissionResponse'
            Core.<$> (x Core..@? "interfacePermission")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermissionResponse' smart constructor.
data CreateNetworkInterfacePermissionResponse = CreateNetworkInterfacePermissionResponse'
  { -- | Information about the permission for the network interface.
    interfacePermission :: Core.Maybe Types.NetworkInterfacePermission,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkInterfacePermissionResponse' value with any optional fields omitted.
mkCreateNetworkInterfacePermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNetworkInterfacePermissionResponse
mkCreateNetworkInterfacePermissionResponse responseStatus =
  CreateNetworkInterfacePermissionResponse'
    { interfacePermission =
        Core.Nothing,
      responseStatus
    }

-- | Information about the permission for the network interface.
--
-- /Note:/ Consider using 'interfacePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprrsInterfacePermission :: Lens.Lens' CreateNetworkInterfacePermissionResponse (Core.Maybe Types.NetworkInterfacePermission)
cniprrsInterfacePermission = Lens.field @"interfacePermission"
{-# DEPRECATED cniprrsInterfacePermission "Use generic-lens or generic-optics with 'interfacePermission' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprrsResponseStatus :: Lens.Lens' CreateNetworkInterfacePermissionResponse Core.Int
cniprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cniprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
