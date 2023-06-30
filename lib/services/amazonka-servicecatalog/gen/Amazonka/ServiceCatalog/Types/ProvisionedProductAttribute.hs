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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisionedProductAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisionedProductAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProvisionedProductStatus
import Amazonka.ServiceCatalog.Types.Tag

-- | Information about a provisioned product.
--
-- /See:/ 'newProvisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
  { -- | The ARN of the provisioned product.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the provisioned product.
    id :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
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
    -- | The record identifier of the last request performed on this provisioned
    -- product.
    lastRecordId :: Prelude.Maybe Prelude.Text,
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
    -- | The user-friendly name of the provisioned product.
    name :: Prelude.Maybe Prelude.Text,
    -- | The assigned identifier for the resource, such as an EC2 instance ID or
    -- an S3 bucket name.
    physicalId :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The name of the product.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning artifact.
    provisioningArtifactName :: Prelude.Maybe Prelude.Text,
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
    -- | The current status message of the provisioned product.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe [Tag],
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM user.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM user in the session. This ARN might contain a session
    -- ID.
    userArnSession :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedProductAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'provisionedProductAttribute_arn' - The ARN of the provisioned product.
--
-- 'createdTime', 'provisionedProductAttribute_createdTime' - The UTC time stamp of the creation time.
--
-- 'id', 'provisionedProductAttribute_id' - The identifier of the provisioned product.
--
-- 'idempotencyToken', 'provisionedProductAttribute_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
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
-- 'lastRecordId', 'provisionedProductAttribute_lastRecordId' - The record identifier of the last request performed on this provisioned
-- product.
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
-- 'name', 'provisionedProductAttribute_name' - The user-friendly name of the provisioned product.
--
-- 'physicalId', 'provisionedProductAttribute_physicalId' - The assigned identifier for the resource, such as an EC2 instance ID or
-- an S3 bucket name.
--
-- 'productId', 'provisionedProductAttribute_productId' - The product identifier.
--
-- 'productName', 'provisionedProductAttribute_productName' - The name of the product.
--
-- 'provisioningArtifactId', 'provisionedProductAttribute_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'provisioningArtifactName', 'provisionedProductAttribute_provisioningArtifactName' - The name of the provisioning artifact.
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
-- 'statusMessage', 'provisionedProductAttribute_statusMessage' - The current status message of the provisioned product.
--
-- 'tags', 'provisionedProductAttribute_tags' - One or more tags.
--
-- 'type'', 'provisionedProductAttribute_type' - The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
--
-- 'userArn', 'provisionedProductAttribute_userArn' - The Amazon Resource Name (ARN) of the IAM user.
--
-- 'userArnSession', 'provisionedProductAttribute_userArnSession' - The ARN of the IAM user in the session. This ARN might contain a session
-- ID.
newProvisionedProductAttribute ::
  ProvisionedProductAttribute
newProvisionedProductAttribute =
  ProvisionedProductAttribute'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      id = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      lastProvisioningRecordId = Prelude.Nothing,
      lastRecordId = Prelude.Nothing,
      lastSuccessfulProvisioningRecordId =
        Prelude.Nothing,
      name = Prelude.Nothing,
      physicalId = Prelude.Nothing,
      productId = Prelude.Nothing,
      productName = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      provisioningArtifactName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userArnSession = Prelude.Nothing
    }

-- | The ARN of the provisioned product.
provisionedProductAttribute_arn :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_arn = Lens.lens (\ProvisionedProductAttribute' {arn} -> arn) (\s@ProvisionedProductAttribute' {} a -> s {arn = a} :: ProvisionedProductAttribute)

-- | The UTC time stamp of the creation time.
provisionedProductAttribute_createdTime :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.UTCTime)
provisionedProductAttribute_createdTime = Lens.lens (\ProvisionedProductAttribute' {createdTime} -> createdTime) (\s@ProvisionedProductAttribute' {} a -> s {createdTime = a} :: ProvisionedProductAttribute) Prelude.. Lens.mapping Data._Time

-- | The identifier of the provisioned product.
provisionedProductAttribute_id :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_id = Lens.lens (\ProvisionedProductAttribute' {id} -> id) (\s@ProvisionedProductAttribute' {} a -> s {id = a} :: ProvisionedProductAttribute)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
provisionedProductAttribute_idempotencyToken :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_idempotencyToken = Lens.lens (\ProvisionedProductAttribute' {idempotencyToken} -> idempotencyToken) (\s@ProvisionedProductAttribute' {} a -> s {idempotencyToken = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_lastProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_lastProvisioningRecordId = Lens.lens (\ProvisionedProductAttribute' {lastProvisioningRecordId} -> lastProvisioningRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastProvisioningRecordId = a} :: ProvisionedProductAttribute)

-- | The record identifier of the last request performed on this provisioned
-- product.
provisionedProductAttribute_lastRecordId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_lastRecordId = Lens.lens (\ProvisionedProductAttribute' {lastRecordId} -> lastRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastRecordId = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_lastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_lastSuccessfulProvisioningRecordId = Lens.lens (\ProvisionedProductAttribute' {lastSuccessfulProvisioningRecordId} -> lastSuccessfulProvisioningRecordId) (\s@ProvisionedProductAttribute' {} a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductAttribute)

-- | The user-friendly name of the provisioned product.
provisionedProductAttribute_name :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_name = Lens.lens (\ProvisionedProductAttribute' {name} -> name) (\s@ProvisionedProductAttribute' {} a -> s {name = a} :: ProvisionedProductAttribute)

-- | The assigned identifier for the resource, such as an EC2 instance ID or
-- an S3 bucket name.
provisionedProductAttribute_physicalId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_physicalId = Lens.lens (\ProvisionedProductAttribute' {physicalId} -> physicalId) (\s@ProvisionedProductAttribute' {} a -> s {physicalId = a} :: ProvisionedProductAttribute)

-- | The product identifier.
provisionedProductAttribute_productId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_productId = Lens.lens (\ProvisionedProductAttribute' {productId} -> productId) (\s@ProvisionedProductAttribute' {} a -> s {productId = a} :: ProvisionedProductAttribute)

-- | The name of the product.
provisionedProductAttribute_productName :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_productName = Lens.lens (\ProvisionedProductAttribute' {productName} -> productName) (\s@ProvisionedProductAttribute' {} a -> s {productName = a} :: ProvisionedProductAttribute)

-- | The identifier of the provisioning artifact.
provisionedProductAttribute_provisioningArtifactId :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_provisioningArtifactId = Lens.lens (\ProvisionedProductAttribute' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductAttribute' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductAttribute)

-- | The name of the provisioning artifact.
provisionedProductAttribute_provisioningArtifactName :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_provisioningArtifactName = Lens.lens (\ProvisionedProductAttribute' {provisioningArtifactName} -> provisioningArtifactName) (\s@ProvisionedProductAttribute' {} a -> s {provisioningArtifactName = a} :: ProvisionedProductAttribute)

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
provisionedProductAttribute_status :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe ProvisionedProductStatus)
provisionedProductAttribute_status = Lens.lens (\ProvisionedProductAttribute' {status} -> status) (\s@ProvisionedProductAttribute' {} a -> s {status = a} :: ProvisionedProductAttribute)

-- | The current status message of the provisioned product.
provisionedProductAttribute_statusMessage :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_statusMessage = Lens.lens (\ProvisionedProductAttribute' {statusMessage} -> statusMessage) (\s@ProvisionedProductAttribute' {} a -> s {statusMessage = a} :: ProvisionedProductAttribute)

-- | One or more tags.
provisionedProductAttribute_tags :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe [Tag])
provisionedProductAttribute_tags = Lens.lens (\ProvisionedProductAttribute' {tags} -> tags) (\s@ProvisionedProductAttribute' {} a -> s {tags = a} :: ProvisionedProductAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
provisionedProductAttribute_type :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_type = Lens.lens (\ProvisionedProductAttribute' {type'} -> type') (\s@ProvisionedProductAttribute' {} a -> s {type' = a} :: ProvisionedProductAttribute)

-- | The Amazon Resource Name (ARN) of the IAM user.
provisionedProductAttribute_userArn :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_userArn = Lens.lens (\ProvisionedProductAttribute' {userArn} -> userArn) (\s@ProvisionedProductAttribute' {} a -> s {userArn = a} :: ProvisionedProductAttribute)

-- | The ARN of the IAM user in the session. This ARN might contain a session
-- ID.
provisionedProductAttribute_userArnSession :: Lens.Lens' ProvisionedProductAttribute (Prelude.Maybe Prelude.Text)
provisionedProductAttribute_userArnSession = Lens.lens (\ProvisionedProductAttribute' {userArnSession} -> userArnSession) (\s@ProvisionedProductAttribute' {} a -> s {userArnSession = a} :: ProvisionedProductAttribute)

instance Data.FromJSON ProvisionedProductAttribute where
  parseJSON =
    Data.withObject
      "ProvisionedProductAttribute"
      ( \x ->
          ProvisionedProductAttribute'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IdempotencyToken")
            Prelude.<*> (x Data..:? "LastProvisioningRecordId")
            Prelude.<*> (x Data..:? "LastRecordId")
            Prelude.<*> (x Data..:? "LastSuccessfulProvisioningRecordId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PhysicalId")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "ProductName")
            Prelude.<*> (x Data..:? "ProvisioningArtifactId")
            Prelude.<*> (x Data..:? "ProvisioningArtifactName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "UserArn")
            Prelude.<*> (x Data..:? "UserArnSession")
      )

instance Prelude.Hashable ProvisionedProductAttribute where
  hashWithSalt _salt ProvisionedProductAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` lastProvisioningRecordId
      `Prelude.hashWithSalt` lastRecordId
      `Prelude.hashWithSalt` lastSuccessfulProvisioningRecordId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` physicalId
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` provisioningArtifactName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` userArnSession

instance Prelude.NFData ProvisionedProductAttribute where
  rnf ProvisionedProductAttribute' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf lastProvisioningRecordId
      `Prelude.seq` Prelude.rnf lastRecordId
      `Prelude.seq` Prelude.rnf lastSuccessfulProvisioningRecordId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf physicalId
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisioningArtifactName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userArnSession
