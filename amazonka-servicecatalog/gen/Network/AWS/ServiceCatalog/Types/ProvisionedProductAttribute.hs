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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
import Network.AWS.ServiceCatalog.Types.Tag

-- | Information about a provisioned product.
--
-- /See:/ 'newProvisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
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
    -- | The Amazon Resource Name (ARN) of the IAM user.
    userArn :: Core.Maybe Core.Text,
    -- | The name of the provisioning artifact.
    provisioningArtifactName :: Core.Maybe Core.Text,
    -- | The ARN of the provisioned product.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the provisioned product.
    id :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The user-friendly name of the provisioned product.
    name :: Core.Maybe Core.Text,
    -- | The name of the product.
    productName :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe [Tag],
    -- | The product identifier.
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
    -- | The assigned identifier for the resource, such as an EC2 instance ID or
    -- an S3 bucket name.
    physicalId :: Core.Maybe Core.Text,
    -- | The ARN of the IAM user in the session. This ARN might contain a session
    -- ID.
    userArnSession :: Core.Maybe Core.Text,
    -- | The record identifier of the last request performed on this provisioned
    -- product.
    lastRecordId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedProductAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'provisionedProductAttribute_statusMessage' - The current status message of the provisioned product.
--
-- 'lastSuccessfulProvisioningRecordId', 'provisionedProductAttribute_lastSuccessfulProvisioningRecordId' - The record identifier of the last successful request performed on this
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
-- 'idempotencyToken', 'provisionedProductAttribute_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
--
-- 'status', 'provisionedProductAttribute_status' - The current status of the provisioned product.
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
-- 'userArn', 'provisionedProductAttribute_userArn' - The Amazon Resource Name (ARN) of the IAM user.
--
-- 'provisioningArtifactName', 'provisionedProductAttribute_provisioningArtifactName' - The name of the provisioning artifact.
--
-- 'arn', 'provisionedProductAttribute_arn' - The ARN of the provisioned product.
--
-- 'id', 'provisionedProductAttribute_id' - The identifier of the provisioned product.
--
-- 'createdTime', 'provisionedProductAttribute_createdTime' - The UTC time stamp of the creation time.
--
-- 'provisioningArtifactId', 'provisionedProductAttribute_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'name', 'provisionedProductAttribute_name' - The user-friendly name of the provisioned product.
--
-- 'productName', 'provisionedProductAttribute_productName' - The name of the product.
--
-- 'tags', 'provisionedProductAttribute_tags' - One or more tags.
--
-- 'productId', 'provisionedProductAttribute_productId' - The product identifier.
--
-- 'lastProvisioningRecordId', 'provisionedProductAttribute_lastProvisioningRecordId' - The record identifier of the last request performed on this provisioned
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
-- 'type'', 'provisionedProductAttribute_type' - The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
--
-- 'physicalId', 'provisionedProductAttribute_physicalId' - The assigned identifier for the resource, such as an EC2 instance ID or
-- an S3 bucket name.
--
-- 'userArnSession', 'provisionedProductAttribute_userArnSession' - The ARN of the IAM user in the session. This ARN might contain a session
-- ID.
--
-- 'lastRecordId', 'provisionedProductAttribute_lastRecordId' - The record identifier of the last request performed on this provisioned
-- product.
newProvisionedProductAttribute ::
  ProvisionedProductAttribute
newProvisionedProductAttribute =
  ProvisionedProductAttribute'
    { statusMessage =
        Core.Nothing,
      lastSuccessfulProvisioningRecordId =
        Core.Nothing,
      idempotencyToken = Core.Nothing,
      status = Core.Nothing,
      userArn = Core.Nothing,
      provisioningArtifactName = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      createdTime = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      name = Core.Nothing,
      productName = Core.Nothing,
      tags = Core.Nothing,
      productId = Core.Nothing,
      lastProvisioningRecordId = Core.Nothing,
      type' = Core.Nothing,
      physicalId = Core.Nothing,
      userArnSession = Core.Nothing,
      lastRecordId = Core.Nothing
    }

-- | The current status message of the provisioned product.
provisionedProductAttribute_statusMessage :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_statusMessage = Lens.lens (\ProvisionedProductAttribute' {statusMessage} -> statusMessage) (\s@ProvisionedProductAttribute' {} a -> s {statusMessage = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_lastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_lastSuccessfulProvisioningRecordId = Lens.lens (\ProvisionedProductAttribute' {lastSuccessfulProvisioningRecordId} -> lastSuccessfulProvisioningRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductAttribute)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
provisionedProductAttribute_idempotencyToken :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_idempotencyToken = Lens.lens (\ProvisionedProductAttribute' {idempotencyToken} -> idempotencyToken) (\s@ProvisionedProductAttribute' {} a -> s {idempotencyToken = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_status :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe ProvisionedProductStatus)
provisionedProductAttribute_status = Lens.lens (\ProvisionedProductAttribute' {status} -> status) (\s@ProvisionedProductAttribute' {} a -> s {status = a} :: ProvisionedProductAttribute)

-- | The Amazon Resource Name (ARN) of the IAM user.
provisionedProductAttribute_userArn :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_userArn = Lens.lens (\ProvisionedProductAttribute' {userArn} -> userArn) (\s@ProvisionedProductAttribute' {} a -> s {userArn = a} :: ProvisionedProductAttribute)

-- | The name of the provisioning artifact.
provisionedProductAttribute_provisioningArtifactName :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_provisioningArtifactName = Lens.lens (\ProvisionedProductAttribute' {provisioningArtifactName} -> provisioningArtifactName) (\s@ProvisionedProductAttribute' {} a -> s {provisioningArtifactName = a} :: ProvisionedProductAttribute)

-- | The ARN of the provisioned product.
provisionedProductAttribute_arn :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_arn = Lens.lens (\ProvisionedProductAttribute' {arn} -> arn) (\s@ProvisionedProductAttribute' {} a -> s {arn = a} :: ProvisionedProductAttribute)

-- | The identifier of the provisioned product.
provisionedProductAttribute_id :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_id = Lens.lens (\ProvisionedProductAttribute' {id} -> id) (\s@ProvisionedProductAttribute' {} a -> s {id = a} :: ProvisionedProductAttribute)

-- | The UTC time stamp of the creation time.
provisionedProductAttribute_createdTime :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.UTCTime)
provisionedProductAttribute_createdTime = Lens.lens (\ProvisionedProductAttribute' {createdTime} -> createdTime) (\s@ProvisionedProductAttribute' {} a -> s {createdTime = a} :: ProvisionedProductAttribute) Core.. Lens.mapping Core._Time

-- | The identifier of the provisioning artifact.
provisionedProductAttribute_provisioningArtifactId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_provisioningArtifactId = Lens.lens (\ProvisionedProductAttribute' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductAttribute' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductAttribute)

-- | The user-friendly name of the provisioned product.
provisionedProductAttribute_name :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_name = Lens.lens (\ProvisionedProductAttribute' {name} -> name) (\s@ProvisionedProductAttribute' {} a -> s {name = a} :: ProvisionedProductAttribute)

-- | The name of the product.
provisionedProductAttribute_productName :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_productName = Lens.lens (\ProvisionedProductAttribute' {productName} -> productName) (\s@ProvisionedProductAttribute' {} a -> s {productName = a} :: ProvisionedProductAttribute)

-- | One or more tags.
provisionedProductAttribute_tags :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe [Tag])
provisionedProductAttribute_tags = Lens.lens (\ProvisionedProductAttribute' {tags} -> tags) (\s@ProvisionedProductAttribute' {} a -> s {tags = a} :: ProvisionedProductAttribute) Core.. Lens.mapping Lens._Coerce

-- | The product identifier.
provisionedProductAttribute_productId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_productId = Lens.lens (\ProvisionedProductAttribute' {productId} -> productId) (\s@ProvisionedProductAttribute' {} a -> s {productId = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_lastProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_lastProvisioningRecordId = Lens.lens (\ProvisionedProductAttribute' {lastProvisioningRecordId} -> lastProvisioningRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastProvisioningRecordId = a} :: ProvisionedProductAttribute)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
provisionedProductAttribute_type :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_type = Lens.lens (\ProvisionedProductAttribute' {type'} -> type') (\s@ProvisionedProductAttribute' {} a -> s {type' = a} :: ProvisionedProductAttribute)

-- | The assigned identifier for the resource, such as an EC2 instance ID or
-- an S3 bucket name.
provisionedProductAttribute_physicalId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_physicalId = Lens.lens (\ProvisionedProductAttribute' {physicalId} -> physicalId) (\s@ProvisionedProductAttribute' {} a -> s {physicalId = a} :: ProvisionedProductAttribute)

-- | The ARN of the IAM user in the session. This ARN might contain a session
-- ID.
provisionedProductAttribute_userArnSession :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_userArnSession = Lens.lens (\ProvisionedProductAttribute' {userArnSession} -> userArnSession) (\s@ProvisionedProductAttribute' {} a -> s {userArnSession = a} :: ProvisionedProductAttribute)

-- | The record identifier of the last request performed on this provisioned
-- product.
provisionedProductAttribute_lastRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.Text)
provisionedProductAttribute_lastRecordId = Lens.lens (\ProvisionedProductAttribute' {lastRecordId} -> lastRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastRecordId = a} :: ProvisionedProductAttribute)

instance Core.FromJSON ProvisionedProductAttribute where
  parseJSON =
    Core.withObject
      "ProvisionedProductAttribute"
      ( \x ->
          ProvisionedProductAttribute'
            Core.<$> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "LastSuccessfulProvisioningRecordId")
            Core.<*> (x Core..:? "IdempotencyToken")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "UserArn")
            Core.<*> (x Core..:? "ProvisioningArtifactName")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ProvisioningArtifactId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ProductName")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..:? "LastProvisioningRecordId")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "PhysicalId")
            Core.<*> (x Core..:? "UserArnSession")
            Core.<*> (x Core..:? "LastRecordId")
      )

instance Core.Hashable ProvisionedProductAttribute

instance Core.NFData ProvisionedProductAttribute
