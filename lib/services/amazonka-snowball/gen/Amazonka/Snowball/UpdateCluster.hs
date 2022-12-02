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
-- Module      : Amazonka.Snowball.UpdateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a cluster\'s @ClusterState@ value is in the @AwaitingQuorum@
-- state, you can update some of the information associated with a cluster.
-- Once the cluster changes to a different job state, usually 60 minutes
-- after the cluster being created, this action is no longer available.
module Amazonka.Snowball.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_roleARN,
    updateCluster_forwardingAddressId,
    updateCluster_description,
    updateCluster_notification,
    updateCluster_resources,
    updateCluster_addressId,
    updateCluster_shippingOption,
    updateCluster_onDeviceServiceConfiguration,
    updateCluster_clusterId,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The new role Amazon Resource Name (ARN) that you want to associate with
    -- this cluster. To create a role ARN, use the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in Identity and Access Management (IAM).
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The updated ID for the forwarding address for a cluster. This field is
    -- not supported in most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The updated description of this cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new or updated Notification object.
    notification :: Prelude.Maybe Notification,
    -- | The updated arrays of JobResource objects that can include updated
    -- S3Resource objects or LambdaResource objects.
    resources :: Prelude.Maybe JobResource,
    -- | The ID of the updated Address object.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The updated shipping option value of this cluster\'s ShippingDetails
    -- object.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | Specifies the service or services on the Snow Family device that your
    -- transferred data will be exported from or imported into. Amazon Web
    -- Services Snow Family device clusters support Amazon S3 and NFS (Network
    -- File System).
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration,
    -- | The cluster ID of the cluster that you want to update, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- API action in Identity and Access Management (IAM).
--
-- 'forwardingAddressId', 'updateCluster_forwardingAddressId' - The updated ID for the forwarding address for a cluster. This field is
-- not supported in most regions.
--
-- 'description', 'updateCluster_description' - The updated description of this cluster.
--
-- 'notification', 'updateCluster_notification' - The new or updated Notification object.
--
-- 'resources', 'updateCluster_resources' - The updated arrays of JobResource objects that can include updated
-- S3Resource objects or LambdaResource objects.
--
-- 'addressId', 'updateCluster_addressId' - The ID of the updated Address object.
--
-- 'shippingOption', 'updateCluster_shippingOption' - The updated shipping option value of this cluster\'s ShippingDetails
-- object.
--
-- 'onDeviceServiceConfiguration', 'updateCluster_onDeviceServiceConfiguration' - Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family device clusters support Amazon S3 and NFS (Network
-- File System).
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
      forwardingAddressId = Prelude.Nothing,
      description = Prelude.Nothing,
      notification = Prelude.Nothing,
      resources = Prelude.Nothing,
      addressId = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      onDeviceServiceConfiguration = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The new role Amazon Resource Name (ARN) that you want to associate with
-- this cluster. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
updateCluster_roleARN :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_roleARN = Lens.lens (\UpdateCluster' {roleARN} -> roleARN) (\s@UpdateCluster' {} a -> s {roleARN = a} :: UpdateCluster)

-- | The updated ID for the forwarding address for a cluster. This field is
-- not supported in most regions.
updateCluster_forwardingAddressId :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_forwardingAddressId = Lens.lens (\UpdateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@UpdateCluster' {} a -> s {forwardingAddressId = a} :: UpdateCluster)

-- | The updated description of this cluster.
updateCluster_description :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_description = Lens.lens (\UpdateCluster' {description} -> description) (\s@UpdateCluster' {} a -> s {description = a} :: UpdateCluster)

-- | The new or updated Notification object.
updateCluster_notification :: Lens.Lens' UpdateCluster (Prelude.Maybe Notification)
updateCluster_notification = Lens.lens (\UpdateCluster' {notification} -> notification) (\s@UpdateCluster' {} a -> s {notification = a} :: UpdateCluster)

-- | The updated arrays of JobResource objects that can include updated
-- S3Resource objects or LambdaResource objects.
updateCluster_resources :: Lens.Lens' UpdateCluster (Prelude.Maybe JobResource)
updateCluster_resources = Lens.lens (\UpdateCluster' {resources} -> resources) (\s@UpdateCluster' {} a -> s {resources = a} :: UpdateCluster)

-- | The ID of the updated Address object.
updateCluster_addressId :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_addressId = Lens.lens (\UpdateCluster' {addressId} -> addressId) (\s@UpdateCluster' {} a -> s {addressId = a} :: UpdateCluster)

-- | The updated shipping option value of this cluster\'s ShippingDetails
-- object.
updateCluster_shippingOption :: Lens.Lens' UpdateCluster (Prelude.Maybe ShippingOption)
updateCluster_shippingOption = Lens.lens (\UpdateCluster' {shippingOption} -> shippingOption) (\s@UpdateCluster' {} a -> s {shippingOption = a} :: UpdateCluster)

-- | Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family device clusters support Amazon S3 and NFS (Network
-- File System).
updateCluster_onDeviceServiceConfiguration :: Lens.Lens' UpdateCluster (Prelude.Maybe OnDeviceServiceConfiguration)
updateCluster_onDeviceServiceConfiguration = Lens.lens (\UpdateCluster' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@UpdateCluster' {} a -> s {onDeviceServiceConfiguration = a} :: UpdateCluster)

-- | The cluster ID of the cluster that you want to update, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
updateCluster_clusterId :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_clusterId = Lens.lens (\UpdateCluster' {clusterId} -> clusterId) (\s@UpdateCluster' {} a -> s {clusterId = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCluster where
  hashWithSalt _salt UpdateCluster' {..} =
    _salt `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` shippingOption
      `Prelude.hashWithSalt` onDeviceServiceConfiguration
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData UpdateCluster where
  rnf UpdateCluster' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf forwardingAddressId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf shippingOption
      `Prelude.seq` Prelude.rnf onDeviceServiceConfiguration
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.UpdateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("ForwardingAddressId" Data..=)
              Prelude.<$> forwardingAddressId,
            ("Description" Data..=) Prelude.<$> description,
            ("Notification" Data..=) Prelude.<$> notification,
            ("Resources" Data..=) Prelude.<$> resources,
            ("AddressId" Data..=) Prelude.<$> addressId,
            ("ShippingOption" Data..=)
              Prelude.<$> shippingOption,
            ("OnDeviceServiceConfiguration" Data..=)
              Prelude.<$> onDeviceServiceConfiguration,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath UpdateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateClusterResponse where
  rnf UpdateClusterResponse' {..} =
    Prelude.rnf httpStatus
