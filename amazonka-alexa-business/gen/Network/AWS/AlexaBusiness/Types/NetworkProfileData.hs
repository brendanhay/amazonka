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
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfileData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfileData where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The data associated with a network profile.
--
-- /See:/ 'newNetworkProfileData' smart constructor.
data NetworkProfileData = NetworkProfileData'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Maybe Prelude.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Prelude.Maybe NetworkSecurityType,
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The SSID of the Wi-Fi network.
    ssid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'eapMethod', 'networkProfileData_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'networkProfileName', 'networkProfileData_networkProfileName' - The name of the network profile associated with a device.
--
-- 'securityType', 'networkProfileData_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'description', 'networkProfileData_description' - Detailed information about a device\'s network profile.
--
-- 'networkProfileArn', 'networkProfileData_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'ssid', 'networkProfileData_ssid' - The SSID of the Wi-Fi network.
newNetworkProfileData ::
  NetworkProfileData
newNetworkProfileData =
  NetworkProfileData'
    { certificateAuthorityArn =
        Prelude.Nothing,
      eapMethod = Prelude.Nothing,
      networkProfileName = Prelude.Nothing,
      securityType = Prelude.Nothing,
      description = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      ssid = Prelude.Nothing
    }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
networkProfileData_certificateAuthorityArn :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_certificateAuthorityArn = Lens.lens (\NetworkProfileData' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@NetworkProfileData' {} a -> s {certificateAuthorityArn = a} :: NetworkProfileData)

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
networkProfileData_eapMethod :: Lens.Lens' NetworkProfileData (Prelude.Maybe NetworkEapMethod)
networkProfileData_eapMethod = Lens.lens (\NetworkProfileData' {eapMethod} -> eapMethod) (\s@NetworkProfileData' {} a -> s {eapMethod = a} :: NetworkProfileData)

-- | The name of the network profile associated with a device.
networkProfileData_networkProfileName :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_networkProfileName = Lens.lens (\NetworkProfileData' {networkProfileName} -> networkProfileName) (\s@NetworkProfileData' {} a -> s {networkProfileName = a} :: NetworkProfileData)

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
networkProfileData_securityType :: Lens.Lens' NetworkProfileData (Prelude.Maybe NetworkSecurityType)
networkProfileData_securityType = Lens.lens (\NetworkProfileData' {securityType} -> securityType) (\s@NetworkProfileData' {} a -> s {securityType = a} :: NetworkProfileData)

-- | Detailed information about a device\'s network profile.
networkProfileData_description :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_description = Lens.lens (\NetworkProfileData' {description} -> description) (\s@NetworkProfileData' {} a -> s {description = a} :: NetworkProfileData)

-- | The ARN of the network profile associated with a device.
networkProfileData_networkProfileArn :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_networkProfileArn = Lens.lens (\NetworkProfileData' {networkProfileArn} -> networkProfileArn) (\s@NetworkProfileData' {} a -> s {networkProfileArn = a} :: NetworkProfileData)

-- | The SSID of the Wi-Fi network.
networkProfileData_ssid :: Lens.Lens' NetworkProfileData (Prelude.Maybe Prelude.Text)
networkProfileData_ssid = Lens.lens (\NetworkProfileData' {ssid} -> ssid) (\s@NetworkProfileData' {} a -> s {ssid = a} :: NetworkProfileData)

instance Prelude.FromJSON NetworkProfileData where
  parseJSON =
    Prelude.withObject
      "NetworkProfileData"
      ( \x ->
          NetworkProfileData'
            Prelude.<$> (x Prelude..:? "CertificateAuthorityArn")
            Prelude.<*> (x Prelude..:? "EapMethod")
            Prelude.<*> (x Prelude..:? "NetworkProfileName")
            Prelude.<*> (x Prelude..:? "SecurityType")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "NetworkProfileArn")
            Prelude.<*> (x Prelude..:? "Ssid")
      )

instance Prelude.Hashable NetworkProfileData

instance Prelude.NFData NetworkProfileData
