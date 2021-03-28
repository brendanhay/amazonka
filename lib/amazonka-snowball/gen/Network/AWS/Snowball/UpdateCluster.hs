{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateCluster (..)
    , mkUpdateCluster
    -- ** Request lenses
    , ucClusterId
    , ucAddressId
    , ucDescription
    , ucForwardingAddressId
    , ucNotification
    , ucResources
    , ucRoleARN
    , ucShippingOption

    -- * Destructuring the response
    , UpdateClusterResponse (..)
    , mkUpdateClusterResponse
    -- ** Response lenses
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { clusterId :: Types.ClusterId
    -- ^ The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
  , addressId :: Core.Maybe Types.AddressId
    -- ^ The ID of the updated 'Address' object.
  , description :: Core.Maybe Core.Text
    -- ^ The updated description of this cluster.
  , forwardingAddressId :: Core.Maybe Types.ForwardingAddressId
    -- ^ The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
  , notification :: Core.Maybe Types.Notification
    -- ^ The new or updated 'Notification' object.
  , resources :: Core.Maybe Types.JobResource
    -- ^ The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
  , shippingOption :: Core.Maybe Types.ShippingOption
    -- ^ The updated shipping option value of this cluster's 'ShippingDetails' object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCluster' value with any optional fields omitted.
mkUpdateCluster
    :: Types.ClusterId -- ^ 'clusterId'
    -> UpdateCluster
mkUpdateCluster clusterId
  = UpdateCluster'{clusterId, addressId = Core.Nothing,
                   description = Core.Nothing, forwardingAddressId = Core.Nothing,
                   notification = Core.Nothing, resources = Core.Nothing,
                   roleARN = Core.Nothing, shippingOption = Core.Nothing}

-- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClusterId :: Lens.Lens' UpdateCluster Types.ClusterId
ucClusterId = Lens.field @"clusterId"
{-# INLINEABLE ucClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAddressId :: Lens.Lens' UpdateCluster (Core.Maybe Types.AddressId)
ucAddressId = Lens.field @"addressId"
{-# INLINEABLE ucAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The updated description of this cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
ucDescription = Lens.field @"description"
{-# INLINEABLE ucDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucForwardingAddressId :: Lens.Lens' UpdateCluster (Core.Maybe Types.ForwardingAddressId)
ucForwardingAddressId = Lens.field @"forwardingAddressId"
{-# INLINEABLE ucForwardingAddressId #-}
{-# DEPRECATED forwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead"  #-}

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotification :: Lens.Lens' UpdateCluster (Core.Maybe Types.Notification)
ucNotification = Lens.field @"notification"
{-# INLINEABLE ucNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucResources :: Lens.Lens' UpdateCluster (Core.Maybe Types.JobResource)
ucResources = Lens.field @"resources"
{-# INLINEABLE ucResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRoleARN :: Lens.Lens' UpdateCluster (Core.Maybe Types.RoleARN)
ucRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ucRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The updated shipping option value of this cluster's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucShippingOption :: Lens.Lens' UpdateCluster (Core.Maybe Types.ShippingOption)
ucShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE ucShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

instance Core.ToQuery UpdateCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCluster where
        toHeaders UpdateCluster{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.UpdateCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCluster where
        toJSON UpdateCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("AddressId" Core..=) Core.<$> addressId,
                  ("Description" Core..=) Core.<$> description,
                  ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
                  ("Notification" Core..=) Core.<$> notification,
                  ("Resources" Core..=) Core.<$> resources,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("ShippingOption" Core..=) Core.<$> shippingOption])

instance Core.AWSRequest UpdateCluster where
        type Rs UpdateCluster = UpdateClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateClusterResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateClusterResponse' smart constructor.
newtype UpdateClusterResponse = UpdateClusterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClusterResponse' value with any optional fields omitted.
mkUpdateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateClusterResponse
mkUpdateClusterResponse responseStatus
  = UpdateClusterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateClusterResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
