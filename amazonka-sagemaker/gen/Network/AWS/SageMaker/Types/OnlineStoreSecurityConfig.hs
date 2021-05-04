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
-- Module      : Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  OnlineStoreSecurityConfig'
    { kmsKeyId =
        Prelude.Nothing
    }

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
onlineStoreSecurityConfig_kmsKeyId :: Lens.Lens' OnlineStoreSecurityConfig (Prelude.Maybe Prelude.Text)
onlineStoreSecurityConfig_kmsKeyId = Lens.lens (\OnlineStoreSecurityConfig' {kmsKeyId} -> kmsKeyId) (\s@OnlineStoreSecurityConfig' {} a -> s {kmsKeyId = a} :: OnlineStoreSecurityConfig)

instance Prelude.FromJSON OnlineStoreSecurityConfig where
  parseJSON =
    Prelude.withObject
      "OnlineStoreSecurityConfig"
      ( \x ->
          OnlineStoreSecurityConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
      )

instance Prelude.Hashable OnlineStoreSecurityConfig

instance Prelude.NFData OnlineStoreSecurityConfig

instance Prelude.ToJSON OnlineStoreSecurityConfig where
  toJSON OnlineStoreSecurityConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId]
      )
