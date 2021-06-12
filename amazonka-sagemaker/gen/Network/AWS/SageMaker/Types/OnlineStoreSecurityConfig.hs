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
-- Module      : Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The security configuration for @OnlineStore@.
--
-- /See:/ 'newOnlineStoreSecurityConfig' smart constructor.
data OnlineStoreSecurityConfig = OnlineStoreSecurityConfig'
  { -- | The ID of the AWS Key Management Service (AWS KMS) key that SageMaker
    -- Feature Store uses to encrypt the Amazon S3 objects at rest using Amazon
    -- S3 server-side encryption.
    --
    -- The caller (either IAM user or IAM role) of @CreateFeatureGroup@ must
    -- have below permissions to the @OnlineStore@ @KmsKeyId@:
    --
    -- -   @\"kms:Encrypt\"@
    --
    -- -   @\"kms:Decrypt\"@
    --
    -- -   @\"kms:DescribeKey\"@
    --
    -- -   @\"kms:CreateGrant\"@
    --
    -- -   @\"kms:RetireGrant\"@
    --
    -- -   @\"kms:ReEncryptFrom\"@
    --
    -- -   @\"kms:ReEncryptTo\"@
    --
    -- -   @\"kms:GenerateDataKey\"@
    --
    -- -   @\"kms:ListAliases\"@
    --
    -- -   @\"kms:ListGrants\"@
    --
    -- -   @\"kms:RevokeGrant\"@
    --
    -- The caller (either IAM user or IAM role) to all DataPlane operations
    -- (@PutRecord@, @GetRecord@, @DeleteRecord@) must have the following
    -- permissions to the @KmsKeyId@:
    --
    -- -   @\"kms:Decrypt\"@
    kmsKeyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OnlineStoreSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'onlineStoreSecurityConfig_kmsKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that SageMaker
-- Feature Store uses to encrypt the Amazon S3 objects at rest using Amazon
-- S3 server-side encryption.
--
-- The caller (either IAM user or IAM role) of @CreateFeatureGroup@ must
-- have below permissions to the @OnlineStore@ @KmsKeyId@:
--
-- -   @\"kms:Encrypt\"@
--
-- -   @\"kms:Decrypt\"@
--
-- -   @\"kms:DescribeKey\"@
--
-- -   @\"kms:CreateGrant\"@
--
-- -   @\"kms:RetireGrant\"@
--
-- -   @\"kms:ReEncryptFrom\"@
--
-- -   @\"kms:ReEncryptTo\"@
--
-- -   @\"kms:GenerateDataKey\"@
--
-- -   @\"kms:ListAliases\"@
--
-- -   @\"kms:ListGrants\"@
--
-- -   @\"kms:RevokeGrant\"@
--
-- The caller (either IAM user or IAM role) to all DataPlane operations
-- (@PutRecord@, @GetRecord@, @DeleteRecord@) must have the following
-- permissions to the @KmsKeyId@:
--
-- -   @\"kms:Decrypt\"@
newOnlineStoreSecurityConfig ::
  OnlineStoreSecurityConfig
newOnlineStoreSecurityConfig =
  OnlineStoreSecurityConfig' {kmsKeyId = Core.Nothing}

-- | The ID of the AWS Key Management Service (AWS KMS) key that SageMaker
-- Feature Store uses to encrypt the Amazon S3 objects at rest using Amazon
-- S3 server-side encryption.
--
-- The caller (either IAM user or IAM role) of @CreateFeatureGroup@ must
-- have below permissions to the @OnlineStore@ @KmsKeyId@:
--
-- -   @\"kms:Encrypt\"@
--
-- -   @\"kms:Decrypt\"@
--
-- -   @\"kms:DescribeKey\"@
--
-- -   @\"kms:CreateGrant\"@
--
-- -   @\"kms:RetireGrant\"@
--
-- -   @\"kms:ReEncryptFrom\"@
--
-- -   @\"kms:ReEncryptTo\"@
--
-- -   @\"kms:GenerateDataKey\"@
--
-- -   @\"kms:ListAliases\"@
--
-- -   @\"kms:ListGrants\"@
--
-- -   @\"kms:RevokeGrant\"@
--
-- The caller (either IAM user or IAM role) to all DataPlane operations
-- (@PutRecord@, @GetRecord@, @DeleteRecord@) must have the following
-- permissions to the @KmsKeyId@:
--
-- -   @\"kms:Decrypt\"@
onlineStoreSecurityConfig_kmsKeyId :: Lens.Lens' OnlineStoreSecurityConfig (Core.Maybe Core.Text)
onlineStoreSecurityConfig_kmsKeyId = Lens.lens (\OnlineStoreSecurityConfig' {kmsKeyId} -> kmsKeyId) (\s@OnlineStoreSecurityConfig' {} a -> s {kmsKeyId = a} :: OnlineStoreSecurityConfig)

instance Core.FromJSON OnlineStoreSecurityConfig where
  parseJSON =
    Core.withObject
      "OnlineStoreSecurityConfig"
      ( \x ->
          OnlineStoreSecurityConfig'
            Core.<$> (x Core..:? "KmsKeyId")
      )

instance Core.Hashable OnlineStoreSecurityConfig

instance Core.NFData OnlineStoreSecurityConfig

instance Core.ToJSON OnlineStoreSecurityConfig where
  toJSON OnlineStoreSecurityConfig' {..} =
    Core.object
      ( Core.catMaybes
          [("KmsKeyId" Core..=) Core.<$> kmsKeyId]
      )
