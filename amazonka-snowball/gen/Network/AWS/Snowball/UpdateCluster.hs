{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The new role Amazon Resource Name (ARN) that you want to associate with
    -- this cluster. To create a role ARN, use the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in AWS Identity and Access Management (IAM).
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The updated shipping option value of this cluster\'s ShippingDetails
    -- object.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | The updated arrays of JobResource objects that can include updated
    -- S3Resource objects or LambdaResource objects.
    resources :: Prelude.Maybe JobResource,
    -- | The updated description of this cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the updated Address object.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The updated ID for the forwarding address for a cluster. This field is
    -- not supported in most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The new or updated Notification object.
    notification :: Prelude.Maybe Notification,
    -- | The cluster ID of the cluster that you want to update, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateCluster
newUpdateCluster pClusterId_ =
  UpdateCluster'
    { roleARN = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      resources = Prelude.Nothing,
      description = Prelude.Nothing,
      addressId = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      notification = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The new role Amazon Resource Name (ARN) that you want to associate with
-- this cluster. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
updateCluster_roleARN :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_roleARN = Lens.lens (\UpdateCluster' {roleARN} -> roleARN) (\s@UpdateCluster' {} a -> s {roleARN = a} :: UpdateCluster)

-- | The updated shipping option value of this cluster\'s ShippingDetails
-- object.
updateCluster_shippingOption :: Lens.Lens' UpdateCluster (Prelude.Maybe ShippingOption)
updateCluster_shippingOption = Lens.lens (\UpdateCluster' {shippingOption} -> shippingOption) (\s@UpdateCluster' {} a -> s {shippingOption = a} :: UpdateCluster)

-- | The updated arrays of JobResource objects that can include updated
-- S3Resource objects or LambdaResource objects.
updateCluster_resources :: Lens.Lens' UpdateCluster (Prelude.Maybe JobResource)
updateCluster_resources = Lens.lens (\UpdateCluster' {resources} -> resources) (\s@UpdateCluster' {} a -> s {resources = a} :: UpdateCluster)

-- | The updated description of this cluster.
updateCluster_description :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_description = Lens.lens (\UpdateCluster' {description} -> description) (\s@UpdateCluster' {} a -> s {description = a} :: UpdateCluster)

-- | The ID of the updated Address object.
updateCluster_addressId :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_addressId = Lens.lens (\UpdateCluster' {addressId} -> addressId) (\s@UpdateCluster' {} a -> s {addressId = a} :: UpdateCluster)

-- | The updated ID for the forwarding address for a cluster. This field is
-- not supported in most regions.
updateCluster_forwardingAddressId :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_forwardingAddressId = Lens.lens (\UpdateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@UpdateCluster' {} a -> s {forwardingAddressId = a} :: UpdateCluster)

-- | The new or updated Notification object.
updateCluster_notification :: Lens.Lens' UpdateCluster (Prelude.Maybe Notification)
updateCluster_notification = Lens.lens (\UpdateCluster' {notification} -> notification) (\s@UpdateCluster' {} a -> s {notification = a} :: UpdateCluster)

-- | The cluster ID of the cluster that you want to update, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
updateCluster_clusterId :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_clusterId = Lens.lens (\UpdateCluster' {clusterId} -> clusterId) (\s@UpdateCluster' {} a -> s {clusterId = a} :: UpdateCluster)

instance Prelude.AWSRequest UpdateCluster where
  type Rs UpdateCluster = UpdateClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCluster

instance Prelude.NFData UpdateCluster

instance Prelude.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.UpdateCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("ShippingOption" Prelude..=)
              Prelude.<$> shippingOption,
            ("Resources" Prelude..=) Prelude.<$> resources,
            ("Description" Prelude..=) Prelude.<$> description,
            ("AddressId" Prelude..=) Prelude.<$> addressId,
            ("ForwardingAddressId" Prelude..=)
              Prelude.<$> forwardingAddressId,
            ("Notification" Prelude..=) Prelude.<$> notification,
            Prelude.Just ("ClusterId" Prelude..= clusterId)
          ]
      )

instance Prelude.ToPath UpdateCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ =
  UpdateClusterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Prelude.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

instance Prelude.NFData UpdateClusterResponse
