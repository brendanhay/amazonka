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
    ucNotification,
    ucForwardingAddressId,
    ucAddressId,
    ucShippingOption,
    ucResources,
    ucClusterId,
    ucDescription,
    ucRoleARN,

    -- * Destructuring the response
    UpdateClusterResponse (..),
    mkUpdateClusterResponse,

    -- ** Response lenses
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The new or updated 'Notification' object.
    notification :: Lude.Maybe Notification,
    -- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
    forwardingAddressId :: Lude.Maybe Lude.Text,
    -- | The ID of the updated 'Address' object.
    addressId :: Lude.Maybe Lude.Text,
    -- | The updated shipping option value of this cluster's 'ShippingDetails' object.
    shippingOption :: Lude.Maybe ShippingOption,
    -- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
    resources :: Lude.Maybe JobResource,
    -- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Lude.Text,
    -- | The updated description of this cluster.
    description :: Lude.Maybe Lude.Text,
    -- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCluster' with the minimum fields required to make a request.
--
-- * 'notification' - The new or updated 'Notification' object.
-- * 'forwardingAddressId' - The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
-- * 'addressId' - The ID of the updated 'Address' object.
-- * 'shippingOption' - The updated shipping option value of this cluster's 'ShippingDetails' object.
-- * 'resources' - The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
-- * 'clusterId' - The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'description' - The updated description of this cluster.
-- * 'roleARN' - The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
mkUpdateCluster ::
  -- | 'clusterId'
  Lude.Text ->
  UpdateCluster
mkUpdateCluster pClusterId_ =
  UpdateCluster'
    { notification = Lude.Nothing,
      forwardingAddressId = Lude.Nothing,
      addressId = Lude.Nothing,
      shippingOption = Lude.Nothing,
      resources = Lude.Nothing,
      clusterId = pClusterId_,
      description = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The new or updated 'Notification' object.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNotification :: Lens.Lens' UpdateCluster (Lude.Maybe Notification)
ucNotification = Lens.lens (notification :: UpdateCluster -> Lude.Maybe Notification) (\s a -> s {notification = a} :: UpdateCluster)
{-# DEPRECATED ucNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucForwardingAddressId :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucForwardingAddressId = Lens.lens (forwardingAddressId :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: UpdateCluster)
{-# DEPRECATED ucForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The ID of the updated 'Address' object.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAddressId :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucAddressId = Lens.lens (addressId :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: UpdateCluster)
{-# DEPRECATED ucAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The updated shipping option value of this cluster's 'ShippingDetails' object.
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucShippingOption :: Lens.Lens' UpdateCluster (Lude.Maybe ShippingOption)
ucShippingOption = Lens.lens (shippingOption :: UpdateCluster -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: UpdateCluster)
{-# DEPRECATED ucShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucResources :: Lens.Lens' UpdateCluster (Lude.Maybe JobResource)
ucResources = Lens.lens (resources :: UpdateCluster -> Lude.Maybe JobResource) (\s a -> s {resources = a} :: UpdateCluster)
{-# DEPRECATED ucResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClusterId :: Lens.Lens' UpdateCluster Lude.Text
ucClusterId = Lens.lens (clusterId :: UpdateCluster -> Lude.Text) (\s a -> s {clusterId = a} :: UpdateCluster)
{-# DEPRECATED ucClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The updated description of this cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateCluster)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRoleARN :: Lens.Lens' UpdateCluster (Lude.Maybe Lude.Text)
ucRoleARN = Lens.lens (roleARN :: UpdateCluster -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateCluster)
{-# DEPRECATED ucRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateCluster where
  type Rs UpdateCluster = UpdateClusterResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateClusterResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.UpdateCluster" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Notification" Lude..=) Lude.<$> notification,
            ("ForwardingAddressId" Lude..=) Lude.<$> forwardingAddressId,
            ("AddressId" Lude..=) Lude.<$> addressId,
            ("ShippingOption" Lude..=) Lude.<$> shippingOption,
            ("Resources" Lude..=) Lude.<$> resources,
            Lude.Just ("ClusterId" Lude..= clusterId),
            ("Description" Lude..=) Lude.<$> description,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateClusterResponse' smart constructor.
newtype UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClusterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateClusterResponse
mkUpdateClusterResponse pResponseStatus_ =
  UpdateClusterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateClusterResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateClusterResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
