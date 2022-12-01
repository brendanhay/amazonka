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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about whether an Amazon Redshift cluster finished applying
-- any hardware changes to security module (HSM) settings that were
-- specified in a modify cluster command.
--
-- /See:/ 'newAwsRedshiftClusterHsmStatus' smart constructor.
data AwsRedshiftClusterHsmStatus = AwsRedshiftClusterHsmStatus'
  { -- | Indicates whether the Amazon Redshift cluster has finished applying any
    -- HSM settings changes specified in a modify cluster command.
    --
    -- Type: String
    --
    -- Valid values: @active@ | @applying@
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the HSM client certificate that the Amazon Redshift cluster
    -- uses to retrieve the data encryption keys that are stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the HSM configuration that contains the information that the
    -- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text
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
-- 'status', 'awsRedshiftClusterHsmStatus_status' - Indicates whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Type: String
--
-- Valid values: @active@ | @applying@
--
-- 'hsmClientCertificateIdentifier', 'awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier' - The name of the HSM client certificate that the Amazon Redshift cluster
-- uses to retrieve the data encryption keys that are stored in an HSM.
--
-- 'hsmConfigurationIdentifier', 'awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier' - The name of the HSM configuration that contains the information that the
-- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
newAwsRedshiftClusterHsmStatus ::
  AwsRedshiftClusterHsmStatus
newAwsRedshiftClusterHsmStatus =
  AwsRedshiftClusterHsmStatus'
    { status =
        Prelude.Nothing,
      hsmClientCertificateIdentifier =
        Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing
    }

-- | Indicates whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Type: String
--
-- Valid values: @active@ | @applying@
awsRedshiftClusterHsmStatus_status :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_status = Lens.lens (\AwsRedshiftClusterHsmStatus' {status} -> status) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {status = a} :: AwsRedshiftClusterHsmStatus)

-- | The name of the HSM client certificate that the Amazon Redshift cluster
-- uses to retrieve the data encryption keys that are stored in an HSM.
awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier = Lens.lens (\AwsRedshiftClusterHsmStatus' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {hsmClientCertificateIdentifier = a} :: AwsRedshiftClusterHsmStatus)

-- | The name of the HSM configuration that contains the information that the
-- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier :: Lens.Lens' AwsRedshiftClusterHsmStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier = Lens.lens (\AwsRedshiftClusterHsmStatus' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@AwsRedshiftClusterHsmStatus' {} a -> s {hsmConfigurationIdentifier = a} :: AwsRedshiftClusterHsmStatus)

instance Core.FromJSON AwsRedshiftClusterHsmStatus where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterHsmStatus"
      ( \x ->
          AwsRedshiftClusterHsmStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "HsmClientCertificateIdentifier")
            Prelude.<*> (x Core..:? "HsmConfigurationIdentifier")
      )

instance Prelude.Hashable AwsRedshiftClusterHsmStatus where
  hashWithSalt _salt AwsRedshiftClusterHsmStatus' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmConfigurationIdentifier

instance Prelude.NFData AwsRedshiftClusterHsmStatus where
  rnf AwsRedshiftClusterHsmStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf hsmConfigurationIdentifier

instance Core.ToJSON AwsRedshiftClusterHsmStatus where
  toJSON AwsRedshiftClusterHsmStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("HsmClientCertificateIdentifier" Core..=)
              Prelude.<$> hsmClientCertificateIdentifier,
            ("HsmConfigurationIdentifier" Core..=)
              Prelude.<$> hsmConfigurationIdentifier
          ]
      )
