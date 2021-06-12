{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus

-- | Information about a provisioned product.
--
-- /See:/ 'newProvisionedProductDetail' smart constructor.
data ProvisionedProductDetail = ProvisionedProductDetail'
  { -- | The current status message of the provisioned product.
    statusMessage :: Core.Maybe Core.Text,
    -- | The record identifier of the last successful request performed on this
    -- provisioned product of the following types:
    --
    -- -   ProvisionedProduct
    --
    -- -   UpdateProvisionedProduct
    --
    -- -   ExecuteProvisionedProductPlan
    --
    -- -   TerminateProvisionedProduct
    lastSuccessfulProvisioningRecordId :: Core.Maybe Core.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Maybe Core.Text,
    -- | The current status of the provisioned product.
    --
    -- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
    --     recent operation succeeded and completed.
    --
    -- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
    --     have valid results. Wait for an @AVAILABLE@ status before performing
    --     operations.
    --
    -- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
    --     has completed the requested operation but is not exactly what was
    --     requested. For example, a request to update to a new version failed
    --     and the stack rolled back to the current version.
    --
    -- -   @ERROR@ - An unexpected error occurred. The provisioned product
    --     exists but the stack is not running. For example, CloudFormation
    --     received a parameter value that was not valid and could not launch
    --     the stack.
    --
    -- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
    --     performed to provision a new product, but resources have not yet
    --     been created. After reviewing the list of resources to be created,
    --     execute the plan. Wait for an @AVAILABLE@ status before performing
    --     operations.
    status :: Core.Maybe ProvisionedProductStatus,
    -- | The ARN of the provisioned product.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the provisioned product.
    id :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The user-friendly name of the provisioned product.
    name :: Core.Maybe Core.Text,
    -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Core.Maybe Core.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Core.Maybe Core.Text,
    -- | The record identifier of the last request performed on this provisioned
    -- product of the following types:
    --
    -- -   ProvisionedProduct
    --
    -- -   UpdateProvisionedProduct
    --
    -- -   ExecuteProvisionedProductPlan
    --
    -- -   TerminateProvisionedProduct
    lastProvisioningRecordId :: Core.Maybe Core.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    type' :: Core.Maybe Core.Text,
    -- | The record identifier of the last request performed on this provisioned
    -- product.
    lastRecordId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedProductDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'provisionedProductDetail_statusMessage' - The current status message of the provisioned product.
--
-- 'lastSuccessfulProvisioningRecordId', 'provisionedProductDetail_lastSuccessfulProvisioningRecordId' - The record identifier of the last successful request performed on this
-- provisioned product of the following types:
--
-- -   ProvisionedProduct
--
-- -   UpdateProvisionedProduct
--
-- -   ExecuteProvisionedProductPlan
--
-- -   TerminateProvisionedProduct
--
-- 'idempotencyToken', 'provisionedProductDetail_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
--
-- 'status', 'provisionedProductDetail_status' - The current status of the provisioned product.
--
-- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
--     recent operation succeeded and completed.
--
-- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
--     have valid results. Wait for an @AVAILABLE@ status before performing
--     operations.
--
-- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
--     has completed the requested operation but is not exactly what was
--     requested. For example, a request to update to a new version failed
--     and the stack rolled back to the current version.
--
-- -   @ERROR@ - An unexpected error occurred. The provisioned product
--     exists but the stack is not running. For example, CloudFormation
--     received a parameter value that was not valid and could not launch
--     the stack.
--
-- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
--     performed to provision a new product, but resources have not yet
--     been created. After reviewing the list of resources to be created,
--     execute the plan. Wait for an @AVAILABLE@ status before performing
--     operations.
--
-- 'arn', 'provisionedProductDetail_arn' - The ARN of the provisioned product.
--
-- 'id', 'provisionedProductDetail_id' - The identifier of the provisioned product.
--
-- 'createdTime', 'provisionedProductDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'provisioningArtifactId', 'provisionedProductDetail_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
--
-- 'name', 'provisionedProductDetail_name' - The user-friendly name of the provisioned product.
--
-- 'launchRoleArn', 'provisionedProductDetail_launchRoleArn' - The ARN of the launch role associated with the provisioned product.
--
-- 'productId', 'provisionedProductDetail_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'lastProvisioningRecordId', 'provisionedProductDetail_lastProvisioningRecordId' - The record identifier of the last request performed on this provisioned
-- product of the following types:
--
-- -   ProvisionedProduct
--
-- -   UpdateProvisionedProduct
--
-- -   ExecuteProvisionedProductPlan
--
-- -   TerminateProvisionedProduct
--
-- 'type'', 'provisionedProductDetail_type' - The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
--
-- 'lastRecordId', 'provisionedProductDetail_lastRecordId' - The record identifier of the last request performed on this provisioned
-- product.
newProvisionedProductDetail ::
  ProvisionedProductDetail
newProvisionedProductDetail =
  ProvisionedProductDetail'
    { statusMessage =
        Core.Nothing,
      lastSuccessfulProvisioningRecordId = Core.Nothing,
      idempotencyToken = Core.Nothing,
      status = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      createdTime = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      name = Core.Nothing,
      launchRoleArn = Core.Nothing,
      productId = Core.Nothing,
      lastProvisioningRecordId = Core.Nothing,
      type' = Core.Nothing,
      lastRecordId = Core.Nothing
    }

-- | The current status message of the provisioned product.
provisionedProductDetail_statusMessage :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_statusMessage = Lens.lens (\ProvisionedProductDetail' {statusMessage} -> statusMessage) (\s@ProvisionedProductDetail' {} a -> s {statusMessage = a} :: ProvisionedProductDetail)

-- | The record identifier of the last successful request performed on this
-- provisioned product of the following types:
--
-- -   ProvisionedProduct
--
-- -   UpdateProvisionedProduct
--
-- -   ExecuteProvisionedProductPlan
--
-- -   TerminateProvisionedProduct
provisionedProductDetail_lastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_lastSuccessfulProvisioningRecordId = Lens.lens (\ProvisionedProductDetail' {lastSuccessfulProvisioningRecordId} -> lastSuccessfulProvisioningRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductDetail)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
provisionedProductDetail_idempotencyToken :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_idempotencyToken = Lens.lens (\ProvisionedProductDetail' {idempotencyToken} -> idempotencyToken) (\s@ProvisionedProductDetail' {} a -> s {idempotencyToken = a} :: ProvisionedProductDetail)

-- | The current status of the provisioned product.
--
-- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
--     recent operation succeeded and completed.
--
-- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
--     have valid results. Wait for an @AVAILABLE@ status before performing
--     operations.
--
-- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
--     has completed the requested operation but is not exactly what was
--     requested. For example, a request to update to a new version failed
--     and the stack rolled back to the current version.
--
-- -   @ERROR@ - An unexpected error occurred. The provisioned product
--     exists but the stack is not running. For example, CloudFormation
--     received a parameter value that was not valid and could not launch
--     the stack.
--
-- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
--     performed to provision a new product, but resources have not yet
--     been created. After reviewing the list of resources to be created,
--     execute the plan. Wait for an @AVAILABLE@ status before performing
--     operations.
provisionedProductDetail_status :: Lens.Lens' ProvisionedProductDetail (Core.Maybe ProvisionedProductStatus)
provisionedProductDetail_status = Lens.lens (\ProvisionedProductDetail' {status} -> status) (\s@ProvisionedProductDetail' {} a -> s {status = a} :: ProvisionedProductDetail)

-- | The ARN of the provisioned product.
provisionedProductDetail_arn :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_arn = Lens.lens (\ProvisionedProductDetail' {arn} -> arn) (\s@ProvisionedProductDetail' {} a -> s {arn = a} :: ProvisionedProductDetail)

-- | The identifier of the provisioned product.
provisionedProductDetail_id :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_id = Lens.lens (\ProvisionedProductDetail' {id} -> id) (\s@ProvisionedProductDetail' {} a -> s {id = a} :: ProvisionedProductDetail)

-- | The UTC time stamp of the creation time.
provisionedProductDetail_createdTime :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.UTCTime)
provisionedProductDetail_createdTime = Lens.lens (\ProvisionedProductDetail' {createdTime} -> createdTime) (\s@ProvisionedProductDetail' {} a -> s {createdTime = a} :: ProvisionedProductDetail) Core.. Lens.mapping Core._Time

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
provisionedProductDetail_provisioningArtifactId :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_provisioningArtifactId = Lens.lens (\ProvisionedProductDetail' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductDetail' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductDetail)

-- | The user-friendly name of the provisioned product.
provisionedProductDetail_name :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_name = Lens.lens (\ProvisionedProductDetail' {name} -> name) (\s@ProvisionedProductDetail' {} a -> s {name = a} :: ProvisionedProductDetail)

-- | The ARN of the launch role associated with the provisioned product.
provisionedProductDetail_launchRoleArn :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_launchRoleArn = Lens.lens (\ProvisionedProductDetail' {launchRoleArn} -> launchRoleArn) (\s@ProvisionedProductDetail' {} a -> s {launchRoleArn = a} :: ProvisionedProductDetail)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
provisionedProductDetail_productId :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_productId = Lens.lens (\ProvisionedProductDetail' {productId} -> productId) (\s@ProvisionedProductDetail' {} a -> s {productId = a} :: ProvisionedProductDetail)

-- | The record identifier of the last request performed on this provisioned
-- product of the following types:
--
-- -   ProvisionedProduct
--
-- -   UpdateProvisionedProduct
--
-- -   ExecuteProvisionedProductPlan
--
-- -   TerminateProvisionedProduct
provisionedProductDetail_lastProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_lastProvisioningRecordId = Lens.lens (\ProvisionedProductDetail' {lastProvisioningRecordId} -> lastProvisioningRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastProvisioningRecordId = a} :: ProvisionedProductDetail)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
provisionedProductDetail_type :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_type = Lens.lens (\ProvisionedProductDetail' {type'} -> type') (\s@ProvisionedProductDetail' {} a -> s {type' = a} :: ProvisionedProductDetail)

-- | The record identifier of the last request performed on this provisioned
-- product.
provisionedProductDetail_lastRecordId :: Lens.Lens' ProvisionedProductDetail (Core.Maybe Core.Text)
provisionedProductDetail_lastRecordId = Lens.lens (\ProvisionedProductDetail' {lastRecordId} -> lastRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastRecordId = a} :: ProvisionedProductDetail)

instance Core.FromJSON ProvisionedProductDetail where
  parseJSON =
    Core.withObject
      "ProvisionedProductDetail"
      ( \x ->
          ProvisionedProductDetail'
            Core.<$> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "LastSuccessfulProvisioningRecordId")
            Core.<*> (x Core..:? "IdempotencyToken")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ProvisioningArtifactId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "LaunchRoleArn")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..:? "LastProvisioningRecordId")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "LastRecordId")
      )

instance Core.Hashable ProvisionedProductDetail

instance Core.NFData ProvisionedProductDetail
