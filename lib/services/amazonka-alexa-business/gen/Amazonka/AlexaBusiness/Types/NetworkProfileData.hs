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
-- Module      : Amazonka.AlexaBusiness.Types.NetworkProfileData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.NetworkProfileData where

import Amazonka.AlexaBusiness.Types.NetworkEapMethod
import Amazonka.AlexaBusiness.Types.NetworkSecurityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data associated with a network profile.
--
-- /See:/ 'newNetworkProfileData' smart constructor.
data NetworkProfileData = NetworkProfileData'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Maybe Prelude.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Prelude.Maybe NetworkSecurityType,
    -- | The SSID of the Wi-Fi network.
    ssid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkProfileData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'networkProfileData_certificateAuthorityArn' - The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
--
-- 'description', 'networkProfileData_description' - Detailed information about a device\'s network profile.
--
-- 'eapMethod', 'networkProfileData_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'networkProfileArn', 'networkProfileData_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'networkProfileName', 'networkProfileData_networkProfileName' - The name of the network profile associated with a device.
--
-- 'securityType', 'networkProfileData_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'ssid', 'networkProfileData_ssid' - The SSID of the Wi-Fi network.
newNetworkProfileData ::
  NetworkProfileData
newNetworkProfileData =
  NetworkProfileData'
    { certificateAuthorityArn =
        Prelude.Nothing,
      description = Prelude.Nothing,
      eapMethod = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      networkProfileName = Prelude.Nothing,
      securityType = Prelude.Nothing,
      ssid = Prelude.Nothing
    }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
networkProfileData_certificateAuthorityArn :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_certificateAuthorityArn = Lens.lens (\NetworkProfileData' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@NetworkProfileData' {} a -> s {certificateAuthorityArn = a} :: NetworkProfileData)

-- | Detailed information about a device\'s network profile.
networkProfileData_description :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_description = Lens.lens (\NetworkProfileData' {description} -> description) (\s@NetworkProfileData' {} a -> s {description = a} :: NetworkProfileData)

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
networkProfileData_eapMethod :: Lens.Lens' NetworkProfileData (Prelude.Maybe NetworkEapMethod)
networkProfileData_eapMethod = Lens.lens (\NetworkProfileData' {eapMethod} -> eapMethod) (\s@NetworkProfileData' {} a -> s {eapMethod = a} :: NetworkProfileData)

-- | The ARN of the network profile associated with a device.
networkProfileData_networkProfileArn :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_networkProfileArn = Lens.lens (\NetworkProfileData' {networkProfileArn} -> networkProfileArn) (\s@NetworkProfileData' {} a -> s {networkProfileArn = a} :: NetworkProfileData)

-- | The name of the network profile associated with a device.
networkProfileData_networkProfileName :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_networkProfileName = Lens.lens (\NetworkProfileData' {networkProfileName} -> networkProfileName) (\s@NetworkProfileData' {} a -> s {networkProfileName = a} :: NetworkProfileData)

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
networkProfileData_securityType :: Lens.Lens' NetworkProfileData (Prelude.Maybe NetworkSecurityType)
networkProfileData_securityType = Lens.lens (\NetworkProfileData' {securityType} -> securityType) (\s@NetworkProfileData' {} a -> s {securityType = a} :: NetworkProfileData)

-- | The SSID of the Wi-Fi network.
networkProfileData_ssid :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_ssid = Lens.lens (\NetworkProfileData' {ssid} -> ssid) (\s@NetworkProfileData' {} a -> s {ssid = a} :: NetworkProfileData)

instance Data.FromJSON NetworkProfileData where
  parseJSON =
    Data.withObject
      "NetworkProfileData"
      ( \x ->
          NetworkProfileData'
            Prelude.<$> (x Data..:? "CertificateAuthorityArn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EapMethod")
            Prelude.<*> (x Data..:? "NetworkProfileArn")
            Prelude.<*> (x Data..:? "NetworkProfileName")
            Prelude.<*> (x Data..:? "SecurityType")
            Prelude.<*> (x Data..:? "Ssid")
      )

instance Prelude.Hashable NetworkProfileData where
  hashWithSalt _salt NetworkProfileData' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eapMethod
      `Prelude.hashWithSalt` networkProfileArn
      `Prelude.hashWithSalt` networkProfileName
      `Prelude.hashWithSalt` securityType
      `Prelude.hashWithSalt` ssid

instance Prelude.NFData NetworkProfileData where
  rnf NetworkProfileData' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eapMethod
      `Prelude.seq` Prelude.rnf networkProfileArn
      `Prelude.seq` Prelude.rnf networkProfileName
      `Prelude.seq` Prelude.rnf securityType
      `Prelude.seq` Prelude.rnf ssid
