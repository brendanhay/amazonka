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
-- Module      : Network.AWS.CloudHSMv2.Types.Certificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Certificates where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains one or more certificates or a certificate signing request
-- (CSR).
--
-- /See:/ 'newCertificates' smart constructor.
data Certificates = Certificates'
  { -- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
    awsHardwareCertificate :: Prelude.Maybe Prelude.Text,
    -- | The HSM certificate issued (signed) by the HSM hardware.
    hsmCertificate :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s certificate signing request (CSR). The CSR exists only
    -- when the cluster\'s state is @UNINITIALIZED@.
    clusterCsr :: Prelude.Maybe Prelude.Text,
    -- | The cluster certificate issued (signed) by the issuing certificate
    -- authority (CA) of the cluster\'s owner.
    clusterCertificate :: Prelude.Maybe Prelude.Text,
    -- | The HSM hardware certificate issued (signed) by the hardware
    -- manufacturer.
    manufacturerHardwareCertificate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Certificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsHardwareCertificate', 'certificates_awsHardwareCertificate' - The HSM hardware certificate issued (signed) by AWS CloudHSM.
--
-- 'hsmCertificate', 'certificates_hsmCertificate' - The HSM certificate issued (signed) by the HSM hardware.
--
-- 'clusterCsr', 'certificates_clusterCsr' - The cluster\'s certificate signing request (CSR). The CSR exists only
-- when the cluster\'s state is @UNINITIALIZED@.
--
-- 'clusterCertificate', 'certificates_clusterCertificate' - The cluster certificate issued (signed) by the issuing certificate
-- authority (CA) of the cluster\'s owner.
--
-- 'manufacturerHardwareCertificate', 'certificates_manufacturerHardwareCertificate' - The HSM hardware certificate issued (signed) by the hardware
-- manufacturer.
newCertificates ::
  Certificates
newCertificates =
  Certificates'
    { awsHardwareCertificate =
        Prelude.Nothing,
      hsmCertificate = Prelude.Nothing,
      clusterCsr = Prelude.Nothing,
      clusterCertificate = Prelude.Nothing,
      manufacturerHardwareCertificate = Prelude.Nothing
    }

-- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
certificates_awsHardwareCertificate :: Lens.Lens' Certificates (Prelude.Maybe Prelude.Text)
certificates_awsHardwareCertificate = Lens.lens (\Certificates' {awsHardwareCertificate} -> awsHardwareCertificate) (\s@Certificates' {} a -> s {awsHardwareCertificate = a} :: Certificates)

-- | The HSM certificate issued (signed) by the HSM hardware.
certificates_hsmCertificate :: Lens.Lens' Certificates (Prelude.Maybe Prelude.Text)
certificates_hsmCertificate = Lens.lens (\Certificates' {hsmCertificate} -> hsmCertificate) (\s@Certificates' {} a -> s {hsmCertificate = a} :: Certificates)

-- | The cluster\'s certificate signing request (CSR). The CSR exists only
-- when the cluster\'s state is @UNINITIALIZED@.
certificates_clusterCsr :: Lens.Lens' Certificates (Prelude.Maybe Prelude.Text)
certificates_clusterCsr = Lens.lens (\Certificates' {clusterCsr} -> clusterCsr) (\s@Certificates' {} a -> s {clusterCsr = a} :: Certificates)

-- | The cluster certificate issued (signed) by the issuing certificate
-- authority (CA) of the cluster\'s owner.
certificates_clusterCertificate :: Lens.Lens' Certificates (Prelude.Maybe Prelude.Text)
certificates_clusterCertificate = Lens.lens (\Certificates' {clusterCertificate} -> clusterCertificate) (\s@Certificates' {} a -> s {clusterCertificate = a} :: Certificates)

-- | The HSM hardware certificate issued (signed) by the hardware
-- manufacturer.
certificates_manufacturerHardwareCertificate :: Lens.Lens' Certificates (Prelude.Maybe Prelude.Text)
certificates_manufacturerHardwareCertificate = Lens.lens (\Certificates' {manufacturerHardwareCertificate} -> manufacturerHardwareCertificate) (\s@Certificates' {} a -> s {manufacturerHardwareCertificate = a} :: Certificates)

instance Prelude.FromJSON Certificates where
  parseJSON =
    Prelude.withObject
      "Certificates"
      ( \x ->
          Certificates'
            Prelude.<$> (x Prelude..:? "AwsHardwareCertificate")
            Prelude.<*> (x Prelude..:? "HsmCertificate")
            Prelude.<*> (x Prelude..:? "ClusterCsr")
            Prelude.<*> (x Prelude..:? "ClusterCertificate")
            Prelude.<*> (x Prelude..:? "ManufacturerHardwareCertificate")
      )

instance Prelude.Hashable Certificates

instance Prelude.NFData Certificates
