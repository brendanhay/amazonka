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
-- Module      : Amazonka.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.NetworkProfile where

import Amazonka.AlexaBusiness.Types.NetworkEapMethod
import Amazonka.AlexaBusiness.Types.NetworkSecurityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The network profile associated with a device.
--
-- /See:/ 'newNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The current password of the Wi-Fi network.
    currentPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Maybe Prelude.Text,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Prelude.Maybe NetworkSecurityType,
    -- | The SSID of the Wi-Fi network.
    ssid :: Prelude.Maybe Prelude.Text,
    -- | The root certificates of your authentication server, which is installed
    -- on your devices and used to trust your authentication server during EAP
    -- negotiation.
    trustAnchors :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'networkProfile_certificateAuthorityArn' - The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
--
-- 'currentPassword', 'networkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'description', 'networkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'eapMethod', 'networkProfile_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'networkProfileArn', 'networkProfile_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'networkProfileName', 'networkProfile_networkProfileName' - The name of the network profile associated with a device.
--
-- 'nextPassword', 'networkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'securityType', 'networkProfile_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'ssid', 'networkProfile_ssid' - The SSID of the Wi-Fi network.
--
-- 'trustAnchors', 'networkProfile_trustAnchors' - The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
newNetworkProfile ::
  NetworkProfile
newNetworkProfile =
  NetworkProfile'
    { certificateAuthorityArn =
        Prelude.Nothing,
      currentPassword = Prelude.Nothing,
      description = Prelude.Nothing,
      eapMethod = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      networkProfileName = Prelude.Nothing,
      nextPassword = Prelude.Nothing,
      securityType = Prelude.Nothing,
      ssid = Prelude.Nothing,
      trustAnchors = Prelude.Nothing
    }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
networkProfile_certificateAuthorityArn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_certificateAuthorityArn = Lens.lens (\NetworkProfile' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@NetworkProfile' {} a -> s {certificateAuthorityArn = a} :: NetworkProfile)

-- | The current password of the Wi-Fi network.
networkProfile_currentPassword :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_currentPassword = Lens.lens (\NetworkProfile' {currentPassword} -> currentPassword) (\s@NetworkProfile' {} a -> s {currentPassword = a} :: NetworkProfile) Prelude.. Lens.mapping Data._Sensitive

-- | Detailed information about a device\'s network profile.
networkProfile_description :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_description = Lens.lens (\NetworkProfile' {description} -> description) (\s@NetworkProfile' {} a -> s {description = a} :: NetworkProfile)

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
networkProfile_eapMethod :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkEapMethod)
networkProfile_eapMethod = Lens.lens (\NetworkProfile' {eapMethod} -> eapMethod) (\s@NetworkProfile' {} a -> s {eapMethod = a} :: NetworkProfile)

-- | The ARN of the network profile associated with a device.
networkProfile_networkProfileArn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_networkProfileArn = Lens.lens (\NetworkProfile' {networkProfileArn} -> networkProfileArn) (\s@NetworkProfile' {} a -> s {networkProfileArn = a} :: NetworkProfile)

-- | The name of the network profile associated with a device.
networkProfile_networkProfileName :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_networkProfileName = Lens.lens (\NetworkProfile' {networkProfileName} -> networkProfileName) (\s@NetworkProfile' {} a -> s {networkProfileName = a} :: NetworkProfile)

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
networkProfile_nextPassword :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_nextPassword = Lens.lens (\NetworkProfile' {nextPassword} -> nextPassword) (\s@NetworkProfile' {} a -> s {nextPassword = a} :: NetworkProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
networkProfile_securityType :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkSecurityType)
networkProfile_securityType = Lens.lens (\NetworkProfile' {securityType} -> securityType) (\s@NetworkProfile' {} a -> s {securityType = a} :: NetworkProfile)

-- | The SSID of the Wi-Fi network.
networkProfile_ssid :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_ssid = Lens.lens (\NetworkProfile' {ssid} -> ssid) (\s@NetworkProfile' {} a -> s {ssid = a} :: NetworkProfile)

-- | The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
networkProfile_trustAnchors :: Lens.Lens' NetworkProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkProfile_trustAnchors = Lens.lens (\NetworkProfile' {trustAnchors} -> trustAnchors) (\s@NetworkProfile' {} a -> s {trustAnchors = a} :: NetworkProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkProfile where
  parseJSON =
    Data.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Prelude.<$> (x Data..:? "CertificateAuthorityArn")
            Prelude.<*> (x Data..:? "CurrentPassword")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EapMethod")
            Prelude.<*> (x Data..:? "NetworkProfileArn")
            Prelude.<*> (x Data..:? "NetworkProfileName")
            Prelude.<*> (x Data..:? "NextPassword")
            Prelude.<*> (x Data..:? "SecurityType")
            Prelude.<*> (x Data..:? "Ssid")
            Prelude.<*> (x Data..:? "TrustAnchors")
      )

instance Prelude.Hashable NetworkProfile where
  hashWithSalt _salt NetworkProfile' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` currentPassword
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eapMethod
      `Prelude.hashWithSalt` networkProfileArn
      `Prelude.hashWithSalt` networkProfileName
      `Prelude.hashWithSalt` nextPassword
      `Prelude.hashWithSalt` securityType
      `Prelude.hashWithSalt` ssid
      `Prelude.hashWithSalt` trustAnchors

instance Prelude.NFData NetworkProfile where
  rnf NetworkProfile' {..} =
    Prelude.rnf certificateAuthorityArn `Prelude.seq`
      Prelude.rnf currentPassword `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf eapMethod `Prelude.seq`
            Prelude.rnf networkProfileArn `Prelude.seq`
              Prelude.rnf networkProfileName `Prelude.seq`
                Prelude.rnf nextPassword `Prelude.seq`
                  Prelude.rnf securityType `Prelude.seq`
                    Prelude.rnf ssid `Prelude.seq`
                      Prelude.rnf trustAnchors
