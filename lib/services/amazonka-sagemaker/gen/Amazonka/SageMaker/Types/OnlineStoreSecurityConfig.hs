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
-- Module      : Amazonka.SageMaker.Types.OnlineStoreSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OnlineStoreSecurityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The security configuration for @OnlineStore@.
--
-- /See:/ 'newOnlineStoreSecurityConfig' smart constructor.
data OnlineStoreSecurityConfig = OnlineStoreSecurityConfig'
  { -- | The Amazon Web Services Key Management Service (KMS) key ARN that
    -- SageMaker Feature Store uses to encrypt the Amazon S3 objects at rest
    -- using Amazon S3 server-side encryption.
    --
    -- The caller (either user or IAM role) of @CreateFeatureGroup@ must have
    -- below permissions to the @OnlineStore@ @KmsKeyId@:
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
    -- The caller (either user or IAM role) to all DataPlane operations
    -- (@PutRecord@, @GetRecord@, @DeleteRecord@) must have the following
    -- permissions to the @KmsKeyId@:
    --
    -- -   @\"kms:Decrypt\"@
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnlineStoreSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'onlineStoreSecurityConfig_kmsKeyId' - The Amazon Web Services Key Management Service (KMS) key ARN that
-- SageMaker Feature Store uses to encrypt the Amazon S3 objects at rest
-- using Amazon S3 server-side encryption.
--
-- The caller (either user or IAM role) of @CreateFeatureGroup@ must have
-- below permissions to the @OnlineStore@ @KmsKeyId@:
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
-- The caller (either user or IAM role) to all DataPlane operations
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

-- | The Amazon Web Services Key Management Service (KMS) key ARN that
-- SageMaker Feature Store uses to encrypt the Amazon S3 objects at rest
-- using Amazon S3 server-side encryption.
--
-- The caller (either user or IAM role) of @CreateFeatureGroup@ must have
-- below permissions to the @OnlineStore@ @KmsKeyId@:
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
-- The caller (either user or IAM role) to all DataPlane operations
-- (@PutRecord@, @GetRecord@, @DeleteRecord@) must have the following
-- permissions to the @KmsKeyId@:
--
-- -   @\"kms:Decrypt\"@
onlineStoreSecurityConfig_kmsKeyId :: Lens.Lens' OnlineStoreSecurityConfig (Prelude.Maybe Prelude.Text)
onlineStoreSecurityConfig_kmsKeyId = Lens.lens (\OnlineStoreSecurityConfig' {kmsKeyId} -> kmsKeyId) (\s@OnlineStoreSecurityConfig' {} a -> s {kmsKeyId = a} :: OnlineStoreSecurityConfig)

instance Data.FromJSON OnlineStoreSecurityConfig where
  parseJSON =
    Data.withObject
      "OnlineStoreSecurityConfig"
      ( \x ->
          OnlineStoreSecurityConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
      )

instance Prelude.Hashable OnlineStoreSecurityConfig where
  hashWithSalt _salt OnlineStoreSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData OnlineStoreSecurityConfig where
  rnf OnlineStoreSecurityConfig' {..} =
    Prelude.rnf kmsKeyId

instance Data.ToJSON OnlineStoreSecurityConfig where
  toJSON OnlineStoreSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("KmsKeyId" Data..=) Prelude.<$> kmsKeyId]
      )
