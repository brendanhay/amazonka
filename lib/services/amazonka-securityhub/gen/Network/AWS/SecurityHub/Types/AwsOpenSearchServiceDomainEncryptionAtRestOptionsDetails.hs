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
-- Module      : Network.AWS.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  Core.FromJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails'
            Prelude.<$> (x Core..:? "Enabled")
              Prelude.<*> (x Core..:? "KmsKeyId")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Enabled" Core..=) Prelude.<$> enabled,
              ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId
            ]
        )
