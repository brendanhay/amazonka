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
-- Module      : Amazonka.DevOpsGuru.Types.KMSServerSideEncryptionIntegrationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.KMSServerSideEncryptionIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.OptInStatus
import Amazonka.DevOpsGuru.Types.ServerSideEncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Information about whether DevOps Guru is configured to encrypt
-- server-side data using KMS.
--
-- /See:/ 'newKMSServerSideEncryptionIntegrationConfig' smart constructor.
data KMSServerSideEncryptionIntegrationConfig = KMSServerSideEncryptionIntegrationConfig'
  { -- | Describes the specified KMS key.
    --
    -- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix it with \"alias\/\". If you specify a
    -- predefined Amazon Web Services alias (an Amazon Web Services alias with
    -- no key ID), Amazon Web Services KMS associates the alias with an Amazon
    -- Web Services managed key and returns its KeyId and Arn in the response.
    -- To specify a KMS key in a different Amazon Web Services account, you
    -- must use the key ARN or alias ARN.
    --
    -- For example:
    --
    -- Key ID: 1234abcd-12ab-34cd-56ef-1234567890ab
    --
    -- Key ARN:
    -- arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab
    --
    -- Alias name: alias\/ExampleAlias
    --
    -- Alias ARN: arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies if DevOps Guru is enabled for KMS integration.
    optInStatus :: Prelude.Maybe OptInStatus,
    -- | The type of KMS key used. Customer managed keys are the KMS keys that
    -- you create. Amazon Web Services owned keys are keys that are owned and
    -- managed by DevOps Guru.
    type' :: Prelude.Maybe ServerSideEncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KMSServerSideEncryptionIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'kmsServerSideEncryptionIntegrationConfig_kmsKeyId' - Describes the specified KMS key.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with \"alias\/\". If you specify a
-- predefined Amazon Web Services alias (an Amazon Web Services alias with
-- no key ID), Amazon Web Services KMS associates the alias with an Amazon
-- Web Services managed key and returns its KeyId and Arn in the response.
-- To specify a KMS key in a different Amazon Web Services account, you
-- must use the key ARN or alias ARN.
--
-- For example:
--
-- Key ID: 1234abcd-12ab-34cd-56ef-1234567890ab
--
-- Key ARN:
-- arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab
--
-- Alias name: alias\/ExampleAlias
--
-- Alias ARN: arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias
--
-- 'optInStatus', 'kmsServerSideEncryptionIntegrationConfig_optInStatus' - Specifies if DevOps Guru is enabled for KMS integration.
--
-- 'type'', 'kmsServerSideEncryptionIntegrationConfig_type' - The type of KMS key used. Customer managed keys are the KMS keys that
-- you create. Amazon Web Services owned keys are keys that are owned and
-- managed by DevOps Guru.
newKMSServerSideEncryptionIntegrationConfig ::
  KMSServerSideEncryptionIntegrationConfig
newKMSServerSideEncryptionIntegrationConfig =
  KMSServerSideEncryptionIntegrationConfig'
    { kmsKeyId =
        Prelude.Nothing,
      optInStatus = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Describes the specified KMS key.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with \"alias\/\". If you specify a
-- predefined Amazon Web Services alias (an Amazon Web Services alias with
-- no key ID), Amazon Web Services KMS associates the alias with an Amazon
-- Web Services managed key and returns its KeyId and Arn in the response.
-- To specify a KMS key in a different Amazon Web Services account, you
-- must use the key ARN or alias ARN.
--
-- For example:
--
-- Key ID: 1234abcd-12ab-34cd-56ef-1234567890ab
--
-- Key ARN:
-- arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab
--
-- Alias name: alias\/ExampleAlias
--
-- Alias ARN: arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias
kmsServerSideEncryptionIntegrationConfig_kmsKeyId :: Lens.Lens' KMSServerSideEncryptionIntegrationConfig (Prelude.Maybe Prelude.Text)
kmsServerSideEncryptionIntegrationConfig_kmsKeyId = Lens.lens (\KMSServerSideEncryptionIntegrationConfig' {kmsKeyId} -> kmsKeyId) (\s@KMSServerSideEncryptionIntegrationConfig' {} a -> s {kmsKeyId = a} :: KMSServerSideEncryptionIntegrationConfig)

-- | Specifies if DevOps Guru is enabled for KMS integration.
kmsServerSideEncryptionIntegrationConfig_optInStatus :: Lens.Lens' KMSServerSideEncryptionIntegrationConfig (Prelude.Maybe OptInStatus)
kmsServerSideEncryptionIntegrationConfig_optInStatus = Lens.lens (\KMSServerSideEncryptionIntegrationConfig' {optInStatus} -> optInStatus) (\s@KMSServerSideEncryptionIntegrationConfig' {} a -> s {optInStatus = a} :: KMSServerSideEncryptionIntegrationConfig)

-- | The type of KMS key used. Customer managed keys are the KMS keys that
-- you create. Amazon Web Services owned keys are keys that are owned and
-- managed by DevOps Guru.
kmsServerSideEncryptionIntegrationConfig_type :: Lens.Lens' KMSServerSideEncryptionIntegrationConfig (Prelude.Maybe ServerSideEncryptionType)
kmsServerSideEncryptionIntegrationConfig_type = Lens.lens (\KMSServerSideEncryptionIntegrationConfig' {type'} -> type') (\s@KMSServerSideEncryptionIntegrationConfig' {} a -> s {type' = a} :: KMSServerSideEncryptionIntegrationConfig)

instance
  Prelude.Hashable
    KMSServerSideEncryptionIntegrationConfig
  where
  hashWithSalt
    _salt
    KMSServerSideEncryptionIntegrationConfig' {..} =
      _salt
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` optInStatus
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    KMSServerSideEncryptionIntegrationConfig
  where
  rnf KMSServerSideEncryptionIntegrationConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf optInStatus
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    KMSServerSideEncryptionIntegrationConfig
  where
  toJSON KMSServerSideEncryptionIntegrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("OptInStatus" Data..=) Prelude.<$> optInStatus,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
