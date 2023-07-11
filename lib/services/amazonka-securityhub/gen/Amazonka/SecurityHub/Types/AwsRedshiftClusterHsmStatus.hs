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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about whether an Amazon Redshift cluster finished applying
-- any hardware changes to security module (HSM) settings that were
-- specified in a modify cluster command.
--
-- /See:/ 'newAwsRedshiftClusterHsmStatus' smart constructor.
data AwsRedshiftClusterHsmStatus = AwsRedshiftClusterHsmStatus'
  { -- | The name of the HSM client certificate that the Amazon Redshift cluster
    -- uses to retrieve the data encryption keys that are stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the HSM configuration that contains the information that the
    -- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Amazon Redshift cluster has finished applying any
    -- HSM settings changes specified in a modify cluster command.
    --
    -- Type: String
    --
    -- Valid values: @active@ | @applying@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterHsmStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificateIdentifier', 'awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier' - The name of the HSM client certificate that the Amazon Redshift cluster
-- uses to retrieve the data encryption keys that are stored in an HSM.
--
-- 'hsmConfigurationIdentifier', 'awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier' - The name of the HSM configuration that contains the information that the
-- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- 'status', 'awsRedshiftClusterHsmStatus_status' - Indicates whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Type: String
--
-- Valid values: @active@ | @applying@
newAwsRedshiftClusterHsmStatus ::
  AwsRedshiftClusterHsmStatus
newAwsRedshiftClusterHsmStatus =
  AwsRedshiftClusterHsmStatus'
    { hsmClientCertificateIdentifier =
        Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the HSM client certificate that the Amazon Redshift cluster
-- uses to retrieve the data encryption keys that are stored in an HSM.
awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier = Lens.lens (\AwsRedshiftClusterHsmStatus' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {hsmClientCertificateIdentifier = a} :: AwsRedshiftClusterHsmStatus)

-- | The name of the HSM configuration that contains the information that the
-- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier = Lens.lens (\AwsRedshiftClusterHsmStatus' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {hsmConfigurationIdentifier = a} :: AwsRedshiftClusterHsmStatus)

-- | Indicates whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Type: String
--
-- Valid values: @active@ | @applying@
awsRedshiftClusterHsmStatus_status :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_status = Lens.lens (\AwsRedshiftClusterHsmStatus' {status} -> status) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {status = a} :: AwsRedshiftClusterHsmStatus)

instance Data.FromJSON AwsRedshiftClusterHsmStatus where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterHsmStatus"
      ( \x ->
          AwsRedshiftClusterHsmStatus'
            Prelude.<$> (x Data..:? "HsmClientCertificateIdentifier")
            Prelude.<*> (x Data..:? "HsmConfigurationIdentifier")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AwsRedshiftClusterHsmStatus where
  hashWithSalt _salt AwsRedshiftClusterHsmStatus' {..} =
    _salt
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsRedshiftClusterHsmStatus where
  rnf AwsRedshiftClusterHsmStatus' {..} =
    Prelude.rnf hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf hsmConfigurationIdentifier
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsRedshiftClusterHsmStatus where
  toJSON AwsRedshiftClusterHsmStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HsmClientCertificateIdentifier" Data..=)
              Prelude.<$> hsmClientCertificateIdentifier,
            ("HsmConfigurationIdentifier" Data..=)
              Prelude.<$> hsmConfigurationIdentifier,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
