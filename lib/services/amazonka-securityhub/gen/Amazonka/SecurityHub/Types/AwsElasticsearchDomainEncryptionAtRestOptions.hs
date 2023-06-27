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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainEncryptionAtRestOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainEncryptionAtRestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the configuration for encryption at rest.
--
-- /See:/ 'newAwsElasticsearchDomainEncryptionAtRestOptions' smart constructor.
data AwsElasticsearchDomainEncryptionAtRestOptions = AwsElasticsearchDomainEncryptionAtRestOptions'
  { -- | Whether encryption at rest is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID. Takes the form @1a2a3a4-1a2a-3a4a-5a6a-1a2a3a4a5a6a@.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainEncryptionAtRestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsElasticsearchDomainEncryptionAtRestOptions_enabled' - Whether encryption at rest is enabled.
--
-- 'kmsKeyId', 'awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId' - The KMS key ID. Takes the form @1a2a3a4-1a2a-3a4a-5a6a-1a2a3a4a5a6a@.
newAwsElasticsearchDomainEncryptionAtRestOptions ::
  AwsElasticsearchDomainEncryptionAtRestOptions
newAwsElasticsearchDomainEncryptionAtRestOptions =
  AwsElasticsearchDomainEncryptionAtRestOptions'
    { enabled =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | Whether encryption at rest is enabled.
awsElasticsearchDomainEncryptionAtRestOptions_enabled :: Lens.Lens' AwsElasticsearchDomainEncryptionAtRestOptions (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainEncryptionAtRestOptions_enabled = Lens.lens (\AwsElasticsearchDomainEncryptionAtRestOptions' {enabled} -> enabled) (\s@AwsElasticsearchDomainEncryptionAtRestOptions' {} a -> s {enabled = a} :: AwsElasticsearchDomainEncryptionAtRestOptions)

-- | The KMS key ID. Takes the form @1a2a3a4-1a2a-3a4a-5a6a-1a2a3a4a5a6a@.
awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId :: Lens.Lens' AwsElasticsearchDomainEncryptionAtRestOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId = Lens.lens (\AwsElasticsearchDomainEncryptionAtRestOptions' {kmsKeyId} -> kmsKeyId) (\s@AwsElasticsearchDomainEncryptionAtRestOptions' {} a -> s {kmsKeyId = a} :: AwsElasticsearchDomainEncryptionAtRestOptions)

instance
  Data.FromJSON
    AwsElasticsearchDomainEncryptionAtRestOptions
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainEncryptionAtRestOptions"
      ( \x ->
          AwsElasticsearchDomainEncryptionAtRestOptions'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "KmsKeyId")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainEncryptionAtRestOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainEncryptionAtRestOptions' {..} =
      _salt
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    AwsElasticsearchDomainEncryptionAtRestOptions
  where
  rnf
    AwsElasticsearchDomainEncryptionAtRestOptions' {..} =
      Prelude.rnf enabled
        `Prelude.seq` Prelude.rnf kmsKeyId

instance
  Data.ToJSON
    AwsElasticsearchDomainEncryptionAtRestOptions
  where
  toJSON
    AwsElasticsearchDomainEncryptionAtRestOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Enabled" Data..=) Prelude.<$> enabled,
              ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId
            ]
        )
