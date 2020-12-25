{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.UpdateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a cluster's @ClusterState@ value is in the @AwaitingQuorum@ state, you can update some of the information associated with a cluster. Once the cluster changes to a different job state, usually 60 minutes after the cluster being created, this action is no longer available.
module Network.AWS.Snowball.UpdateCluster
  ( -- * Creating a request
    UpdateCluster (..),
    mkUpdateCluster,

    -- ** Request lenses
    ucClusterId,
    ucAddressId,
    ucDescription,
    ucForwardingAddressId,
    ucNotification,
    ucResources,
    ucRoleARN,
    ucShippingOption,

    -- * Destructuring the response
    UpdateClusterResponse (..),
    mkUpdateClusterResponse,

    -- ** Response lenses
    ucrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Types.ClusterId,
    -- | The ID of the updated 'Address' object.
    addressId :: Core.Maybe Types.AddressId,
    -- | The updated description of this cluster.
    description :: Core.Maybe Types.Description,
    -- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
    forwardingAddressId :: Core.Maybe Types.ForwardingAddressId,
    -- | The new or updated 'Notification' object.
    notification :: Core.Maybe Types.Notification,
    -- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
    resources :: Core.Maybe Types.JobResource,
    -- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The updated shipping option value of this cluster's 'ShippingDetails' object.
    shippingOption :: Core.Maybe Types.ShippingOption
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCluster' value with any optional fields omitted.
mkUpdateCluster ::
  -- | 'clusterId'
  Types.ClusterId ->
  UpdateCluster
mkUpdateCluster clusterId =
  UpdateCluster'
    { clusterId,
      addressId = Core.Nothing,
      description = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      notification = Core.Nothing,
      resources = Core.Nothing,
      roleARN = Core.Nothing,
      shippingOption = Core.Nothing
    }

-- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClusterId :: Lens.Lens' UpdateCluster Types.ClusterId
ucClusterId = Lens.field @"clusterId"
{-# DEPRECATED ucClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAddressId :: Lens.Lens' UpdateCluster (Core.Maybe Types.AddressId)
ucAddressId = Lens.field @"addressId"
{-# DEPRECATED ucAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The updated description of this cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCluster (Core.Maybe Types.Description)
ucDescription = Lens.field @"description"
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucForwardingAddressId :: Lens.Lens' UpdateCluster (Core.Maybe Types.ForwardingAddressId)
ucForwardingAddressId = Lens.field @"forwardingAddressId"
{-# DEPRECATED ucForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotification :: Lens.Lens' UpdateCluster (Core.Maybe Types.Notification)
ucNotification = Lens.field @"notification"
{-# DEPRECATED ucNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucResources :: Lens.Lens' UpdateCluster (Core.Maybe Types.JobResource)
ucResources = Lens.field @"resources"
{-# DEPRECATED ucResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRoleARN :: Lens.Lens' UpdateCluster (Core.Maybe Types.RoleARN)
ucRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ucRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The updated shipping option value of this cluster's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucShippingOption :: Lens.Lens' UpdateCluster (Core.Maybe Types.ShippingOption)
ucShippingOption = Lens.field @"shippingOption"
{-# DEPRECATED ucShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

instance Core.FromJSON UpdateCluster where
  toJSON UpdateCluster {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("AddressId" Core..=) Core.<$> addressId,
            ("Description" Core..=) Core.<$> description,
            ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
            ("Notification" Core..=) Core.<$> notification,
            ("Resources" Core..=) Core.<$> resources,
            ("RoleARN" Core..=) Core.<$> roleARN,
            ("ShippingOption" Core..=) Core.<$> shippingOption
          ]
      )

instance Core.AWSRequest UpdateCluster where
  type Rs UpdateCluster = UpdateClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSIESnowballJobManagementService.UpdateCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClusterResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateClusterResponse' smart constructor.
newtype UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClusterResponse' value with any optional fields omitted.
mkUpdateClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateClusterResponse
mkUpdateClusterResponse responseStatus =
  UpdateClusterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateClusterResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
