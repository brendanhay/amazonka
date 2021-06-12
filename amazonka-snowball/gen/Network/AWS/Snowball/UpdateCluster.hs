{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.UpdateCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a cluster\'s @ClusterState@ value is in the @AwaitingQuorum@
-- state, you can update some of the information associated with a cluster.
-- Once the cluster changes to a different job state, usually 60 minutes
-- after the cluster being created, this action is no longer available.
module Network.AWS.Snowball.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_roleARN,
    updateCluster_shippingOption,
    updateCluster_resources,
    updateCluster_description,
    updateCluster_addressId,
    updateCluster_forwardingAddressId,
    updateCluster_notification,
    updateCluster_clusterId,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The new role Amazon Resource Name (ARN) that you want to associate with
    -- this cluster. To create a role ARN, use the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in AWS Identity and Access Management (IAM).
    roleARN :: Core.Maybe Core.Text,
    -- | The updated shipping option value of this cluster\'s ShippingDetails
    -- object.
    shippingOption :: Core.Maybe ShippingOption,
    -- | The updated arrays of JobResource objects that can include updated
    -- S3Resource objects or LambdaResource objects.
    resources :: Core.Maybe JobResource,
    -- | The updated description of this cluster.
    description :: Core.Maybe Core.Text,
    -- | The ID of the updated Address object.
    addressId :: Core.Maybe Core.Text,
    -- | The updated ID for the forwarding address for a cluster. This field is
    -- not supported in most regions.
    forwardingAddressId :: Core.Maybe Core.Text,
    -- | The new or updated Notification object.
    notification :: Core.Maybe Notification,
    -- | The cluster ID of the cluster that you want to update, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'updateCluster_roleARN' - The new role Amazon Resource Name (ARN) that you want to associate with
-- this cluster. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
--
-- 'shippingOption', 'updateCluster_shippingOption' - The updated shipping option value of this cluster\'s ShippingDetails
-- object.
--
-- 'resources', 'updateCluster_resources' - The updated arrays of JobResource objects that can include updated
-- S3Resource objects or LambdaResource objects.
--
-- 'description', 'updateCluster_description' - The updated description of this cluster.
--
-- 'addressId', 'updateCluster_addressId' - The ID of the updated Address object.
--
-- 'forwardingAddressId', 'updateCluster_forwardingAddressId' - The updated ID for the forwarding address for a cluster. This field is
-- not supported in most regions.
--
-- 'notification', 'updateCluster_notification' - The new or updated Notification object.
--
-- 'clusterId', 'updateCluster_clusterId' - The cluster ID of the cluster that you want to update, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
newUpdateCluster ::
  -- | 'clusterId'
  Core.Text ->
  UpdateCluster
newUpdateCluster pClusterId_ =
  UpdateCluster'
    { roleARN = Core.Nothing,
      shippingOption = Core.Nothing,
      resources = Core.Nothing,
      description = Core.Nothing,
      addressId = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      notification = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The new role Amazon Resource Name (ARN) that you want to associate with
-- this cluster. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
updateCluster_roleARN :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
updateCluster_roleARN = Lens.lens (\UpdateCluster' {roleARN} -> roleARN) (\s@UpdateCluster' {} a -> s {roleARN = a} :: UpdateCluster)

-- | The updated shipping option value of this cluster\'s ShippingDetails
-- object.
updateCluster_shippingOption :: Lens.Lens' UpdateCluster (Core.Maybe ShippingOption)
updateCluster_shippingOption = Lens.lens (\UpdateCluster' {shippingOption} -> shippingOption) (\s@UpdateCluster' {} a -> s {shippingOption = a} :: UpdateCluster)

-- | The updated arrays of JobResource objects that can include updated
-- S3Resource objects or LambdaResource objects.
updateCluster_resources :: Lens.Lens' UpdateCluster (Core.Maybe JobResource)
updateCluster_resources = Lens.lens (\UpdateCluster' {resources} -> resources) (\s@UpdateCluster' {} a -> s {resources = a} :: UpdateCluster)

-- | The updated description of this cluster.
updateCluster_description :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
updateCluster_description = Lens.lens (\UpdateCluster' {description} -> description) (\s@UpdateCluster' {} a -> s {description = a} :: UpdateCluster)

-- | The ID of the updated Address object.
updateCluster_addressId :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
updateCluster_addressId = Lens.lens (\UpdateCluster' {addressId} -> addressId) (\s@UpdateCluster' {} a -> s {addressId = a} :: UpdateCluster)

-- | The updated ID for the forwarding address for a cluster. This field is
-- not supported in most regions.
updateCluster_forwardingAddressId :: Lens.Lens' UpdateCluster (Core.Maybe Core.Text)
updateCluster_forwardingAddressId = Lens.lens (\UpdateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@UpdateCluster' {} a -> s {forwardingAddressId = a} :: UpdateCluster)

-- | The new or updated Notification object.
updateCluster_notification :: Lens.Lens' UpdateCluster (Core.Maybe Notification)
updateCluster_notification = Lens.lens (\UpdateCluster' {notification} -> notification) (\s@UpdateCluster' {} a -> s {notification = a} :: UpdateCluster)

-- | The cluster ID of the cluster that you want to update, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
updateCluster_clusterId :: Lens.Lens' UpdateCluster Core.Text
updateCluster_clusterId = Lens.lens (\UpdateCluster' {clusterId} -> clusterId) (\s@UpdateCluster' {} a -> s {clusterId = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClusterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCluster

instance Core.NFData UpdateCluster

instance Core.ToHeaders UpdateCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.UpdateCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleARN" Core..=) Core.<$> roleARN,
            ("ShippingOption" Core..=) Core.<$> shippingOption,
            ("Resources" Core..=) Core.<$> resources,
            ("Description" Core..=) Core.<$> description,
            ("AddressId" Core..=) Core.<$> addressId,
            ("ForwardingAddressId" Core..=)
              Core.<$> forwardingAddressId,
            ("Notification" Core..=) Core.<$> notification,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath UpdateCluster where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateClusterResponse_httpStatus' - The response's http status code.
newUpdateClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ =
  UpdateClusterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Core.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

instance Core.NFData UpdateClusterResponse
