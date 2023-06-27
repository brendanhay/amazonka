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
-- Module      : Amazonka.DevOpsGuru.Types.KMSServerSideEncryptionIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.KMSServerSideEncryptionIntegration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.OptInStatus
import Amazonka.DevOpsGuru.Types.ServerSideEncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Information about the KMS encryption used with DevOps Guru.
--
-- /See:/ 'newKMSServerSideEncryptionIntegration' smart constructor.
data KMSServerSideEncryptionIntegration = KMSServerSideEncryptionIntegration'
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
    -- | Specifies if DevOps Guru is enabled for customer managed keys.
    optInStatus :: Prelude.Maybe OptInStatus,
    -- | The type of KMS key used. Customer managed keys are the KMS keys that
    -- you create. Amazon Web Services owned keys are keys that are owned and
    -- managed by DevOps Guru.
    type' :: Prelude.Maybe ServerSideEncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KMSServerSideEncryptionIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'kmsServerSideEncryptionIntegration_kmsKeyId' - Describes the specified KMS key.
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
-- 'optInStatus', 'kmsServerSideEncryptionIntegration_optInStatus' - Specifies if DevOps Guru is enabled for customer managed keys.
--
-- 'type'', 'kmsServerSideEncryptionIntegration_type' - The type of KMS key used. Customer managed keys are the KMS keys that
-- you create. Amazon Web Services owned keys are keys that are owned and
-- managed by DevOps Guru.
newKMSServerSideEncryptionIntegration ::
  KMSServerSideEncryptionIntegration
newKMSServerSideEncryptionIntegration =
  KMSServerSideEncryptionIntegration'
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
kmsServerSideEncryptionIntegration_kmsKeyId :: Lens.Lens' KMSServerSideEncryptionIntegration (Prelude.Maybe Prelude.Text)
kmsServerSideEncryptionIntegration_kmsKeyId = Lens.lens (\KMSServerSideEncryptionIntegration' {kmsKeyId} -> kmsKeyId) (\s@KMSServerSideEncryptionIntegration' {} a -> s {kmsKeyId = a} :: KMSServerSideEncryptionIntegration)

-- | Specifies if DevOps Guru is enabled for customer managed keys.
kmsServerSideEncryptionIntegration_optInStatus :: Lens.Lens' KMSServerSideEncryptionIntegration (Prelude.Maybe OptInStatus)
kmsServerSideEncryptionIntegration_optInStatus = Lens.lens (\KMSServerSideEncryptionIntegration' {optInStatus} -> optInStatus) (\s@KMSServerSideEncryptionIntegration' {} a -> s {optInStatus = a} :: KMSServerSideEncryptionIntegration)

-- | The type of KMS key used. Customer managed keys are the KMS keys that
-- you create. Amazon Web Services owned keys are keys that are owned and
-- managed by DevOps Guru.
kmsServerSideEncryptionIntegration_type :: Lens.Lens' KMSServerSideEncryptionIntegration (Prelude.Maybe ServerSideEncryptionType)
kmsServerSideEncryptionIntegration_type = Lens.lens (\KMSServerSideEncryptionIntegration' {type'} -> type') (\s@KMSServerSideEncryptionIntegration' {} a -> s {type' = a} :: KMSServerSideEncryptionIntegration)

instance
  Data.FromJSON
    KMSServerSideEncryptionIntegration
  where
  parseJSON =
    Data.withObject
      "KMSServerSideEncryptionIntegration"
      ( \x ->
          KMSServerSideEncryptionIntegration'
            Prelude.<$> (x Data..:? "KMSKeyId")
            Prelude.<*> (x Data..:? "OptInStatus")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    KMSServerSideEncryptionIntegration
  where
  hashWithSalt
    _salt
    KMSServerSideEncryptionIntegration' {..} =
      _salt
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` optInStatus
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    KMSServerSideEncryptionIntegration
  where
  rnf KMSServerSideEncryptionIntegration' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf optInStatus
      `Prelude.seq` Prelude.rnf type'
