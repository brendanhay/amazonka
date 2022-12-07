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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the configuration for encryption at rest for the
-- OpenSearch domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails = AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails'
  { -- | Whether encryption at rest is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled' - Whether encryption at rest is enabled.
--
-- 'kmsKeyId', 'awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId' - The KMS key ID.
newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails ::
  AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails =
  AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails'
    { enabled =
        Prelude.Nothing,
      kmsKeyId =
        Prelude.Nothing
    }

-- | Whether encryption at rest is enabled.
awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled :: Lens.Lens' AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled = Lens.lens (\AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {enabled} -> enabled) (\s@AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {} a -> s {enabled = a} :: AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails)

-- | The KMS key ID.
awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId :: Lens.Lens' AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId = Lens.lens (\AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {} a -> s {kmsKeyId = a} :: AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails'
            Prelude.<$> (x Data..:? "Enabled")
              Prelude.<*> (x Data..:? "KmsKeyId")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {..} =
      Prelude.rnf enabled
        `Prelude.seq` Prelude.rnf kmsKeyId

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Enabled" Data..=) Prelude.<$> enabled,
              ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId
            ]
        )
