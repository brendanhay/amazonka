{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus

-- | Information about a provisioned product.
--
-- /See:/ 'newProvisionedProductDetail' smart constructor.
data ProvisionedProductDetail = ProvisionedProductDetail'
  { -- | The current status message of the provisioned product.
    statusMessage :: Prelude.Maybe Prelude.Text,
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
    lastSuccessfulProvisioningRecordId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe ProvisionedProductStatus,
    -- | The ARN of the provisioned product.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product.
    id :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the provisioned product.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Prelude.Maybe Prelude.Text,
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
    lastProvisioningRecordId :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The record identifier of the last request performed on this provisioned
    -- product.
    lastRecordId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      lastSuccessfulProvisioningRecordId =
        Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      status = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      name = Prelude.Nothing,
      launchRoleArn = Prelude.Nothing,
      productId = Prelude.Nothing,
      lastProvisioningRecordId = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastRecordId = Prelude.Nothing
    }

-- | The current status message of the provisioned product.
provisionedProductDetail_statusMessage :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
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
provisionedProductDetail_lastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_lastSuccessfulProvisioningRecordId = Lens.lens (\ProvisionedProductDetail' {lastSuccessfulProvisioningRecordId} -> lastSuccessfulProvisioningRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductDetail)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
provisionedProductDetail_idempotencyToken :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
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
provisionedProductDetail_status :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe ProvisionedProductStatus)
provisionedProductDetail_status = Lens.lens (\ProvisionedProductDetail' {status} -> status) (\s@ProvisionedProductDetail' {} a -> s {status = a} :: ProvisionedProductDetail)

-- | The ARN of the provisioned product.
provisionedProductDetail_arn :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_arn = Lens.lens (\ProvisionedProductDetail' {arn} -> arn) (\s@ProvisionedProductDetail' {} a -> s {arn = a} :: ProvisionedProductDetail)

-- | The identifier of the provisioned product.
provisionedProductDetail_id :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_id = Lens.lens (\ProvisionedProductDetail' {id} -> id) (\s@ProvisionedProductDetail' {} a -> s {id = a} :: ProvisionedProductDetail)

-- | The UTC time stamp of the creation time.
provisionedProductDetail_createdTime :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.UTCTime)
provisionedProductDetail_createdTime = Lens.lens (\ProvisionedProductDetail' {createdTime} -> createdTime) (\s@ProvisionedProductDetail' {} a -> s {createdTime = a} :: ProvisionedProductDetail) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
provisionedProductDetail_provisioningArtifactId :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_provisioningArtifactId = Lens.lens (\ProvisionedProductDetail' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductDetail' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductDetail)

-- | The user-friendly name of the provisioned product.
provisionedProductDetail_name :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_name = Lens.lens (\ProvisionedProductDetail' {name} -> name) (\s@ProvisionedProductDetail' {} a -> s {name = a} :: ProvisionedProductDetail)

-- | The ARN of the launch role associated with the provisioned product.
provisionedProductDetail_launchRoleArn :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_launchRoleArn = Lens.lens (\ProvisionedProductDetail' {launchRoleArn} -> launchRoleArn) (\s@ProvisionedProductDetail' {} a -> s {launchRoleArn = a} :: ProvisionedProductDetail)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
provisionedProductDetail_productId :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
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
provisionedProductDetail_lastProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_lastProvisioningRecordId = Lens.lens (\ProvisionedProductDetail' {lastProvisioningRecordId} -> lastProvisioningRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastProvisioningRecordId = a} :: ProvisionedProductDetail)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
provisionedProductDetail_type :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_type = Lens.lens (\ProvisionedProductDetail' {type'} -> type') (\s@ProvisionedProductDetail' {} a -> s {type' = a} :: ProvisionedProductDetail)

-- | The record identifier of the last request performed on this provisioned
-- product.
provisionedProductDetail_lastRecordId :: Lens.Lens' ProvisionedProductDetail (Prelude.Maybe Prelude.Text)
provisionedProductDetail_lastRecordId = Lens.lens (\ProvisionedProductDetail' {lastRecordId} -> lastRecordId) (\s@ProvisionedProductDetail' {} a -> s {lastRecordId = a} :: ProvisionedProductDetail)

instance Prelude.FromJSON ProvisionedProductDetail where
  parseJSON =
    Prelude.withObject
      "ProvisionedProductDetail"
      ( \x ->
          ProvisionedProductDetail'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "LastSuccessfulProvisioningRecordId")
            Prelude.<*> (x Prelude..:? "IdempotencyToken")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "ProvisioningArtifactId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "LaunchRoleArn")
            Prelude.<*> (x Prelude..:? "ProductId")
            Prelude.<*> (x Prelude..:? "LastProvisioningRecordId")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "LastRecordId")
      )

instance Prelude.Hashable ProvisionedProductDetail

instance Prelude.NFData ProvisionedProductDetail
