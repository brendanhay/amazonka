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
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfile where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The network profile associated with a device.
--
-- /See:/ 'newNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Core.Maybe Core.Text,
    -- | The root certificates of your authentication server, which is installed
    -- on your devices and used to trust your authentication server during EAP
    -- negotiation.
    trustAnchors :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The current password of the Wi-Fi network.
    currentPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Core.Maybe NetworkEapMethod,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Core.Maybe Core.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Core.Maybe NetworkSecurityType,
    -- | Detailed information about a device\'s network profile.
    description :: Core.Maybe Core.Text,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Maybe Core.Text,
    -- | The SSID of the Wi-Fi network.
    ssid :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'trustAnchors', 'networkProfile_trustAnchors' - The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
--
-- 'currentPassword', 'networkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'eapMethod', 'networkProfile_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'networkProfileName', 'networkProfile_networkProfileName' - The name of the network profile associated with a device.
--
-- 'securityType', 'networkProfile_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'description', 'networkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'nextPassword', 'networkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'networkProfileArn', 'networkProfile_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'ssid', 'networkProfile_ssid' - The SSID of the Wi-Fi network.
newNetworkProfile ::
  NetworkProfile
newNetworkProfile =
  NetworkProfile'
    { certificateAuthorityArn =
        Core.Nothing,
      trustAnchors = Core.Nothing,
      currentPassword = Core.Nothing,
      eapMethod = Core.Nothing,
      networkProfileName = Core.Nothing,
      securityType = Core.Nothing,
      description = Core.Nothing,
      nextPassword = Core.Nothing,
      networkProfileArn = Core.Nothing,
      ssid = Core.Nothing
    }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
networkProfile_certificateAuthorityArn :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_certificateAuthorityArn = Lens.lens (\NetworkProfile' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@NetworkProfile' {} a -> s {certificateAuthorityArn = a} :: NetworkProfile)

-- | The root certificates of your authentication server, which is installed
-- on your devices and used to trust your authentication server during EAP
-- negotiation.
networkProfile_trustAnchors :: Lens.Lens' NetworkProfile (Core.Maybe (Core.NonEmpty Core.Text))
networkProfile_trustAnchors = Lens.lens (\NetworkProfile' {trustAnchors} -> trustAnchors) (\s@NetworkProfile' {} a -> s {trustAnchors = a} :: NetworkProfile) Core.. Lens.mapping Lens._Coerce

-- | The current password of the Wi-Fi network.
networkProfile_currentPassword :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_currentPassword = Lens.lens (\NetworkProfile' {currentPassword} -> currentPassword) (\s@NetworkProfile' {} a -> s {currentPassword = a} :: NetworkProfile) Core.. Lens.mapping Core._Sensitive

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
networkProfile_eapMethod :: Lens.Lens' NetworkProfile (Core.Maybe NetworkEapMethod)
networkProfile_eapMethod = Lens.lens (\NetworkProfile' {eapMethod} -> eapMethod) (\s@NetworkProfile' {} a -> s {eapMethod = a} :: NetworkProfile)

-- | The name of the network profile associated with a device.
networkProfile_networkProfileName :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_networkProfileName = Lens.lens (\NetworkProfile' {networkProfileName} -> networkProfileName) (\s@NetworkProfile' {} a -> s {networkProfileName = a} :: NetworkProfile)

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
networkProfile_securityType :: Lens.Lens' NetworkProfile (Core.Maybe NetworkSecurityType)
networkProfile_securityType = Lens.lens (\NetworkProfile' {securityType} -> securityType) (\s@NetworkProfile' {} a -> s {securityType = a} :: NetworkProfile)

-- | Detailed information about a device\'s network profile.
networkProfile_description :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_description = Lens.lens (\NetworkProfile' {description} -> description) (\s@NetworkProfile' {} a -> s {description = a} :: NetworkProfile)

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
networkProfile_nextPassword :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_nextPassword = Lens.lens (\NetworkProfile' {nextPassword} -> nextPassword) (\s@NetworkProfile' {} a -> s {nextPassword = a} :: NetworkProfile) Core.. Lens.mapping Core._Sensitive

-- | The ARN of the network profile associated with a device.
networkProfile_networkProfileArn :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_networkProfileArn = Lens.lens (\NetworkProfile' {networkProfileArn} -> networkProfileArn) (\s@NetworkProfile' {} a -> s {networkProfileArn = a} :: NetworkProfile)

-- | The SSID of the Wi-Fi network.
networkProfile_ssid :: Lens.Lens' NetworkProfile (Core.Maybe Core.Text)
networkProfile_ssid = Lens.lens (\NetworkProfile' {ssid} -> ssid) (\s@NetworkProfile' {} a -> s {ssid = a} :: NetworkProfile)

instance Core.FromJSON NetworkProfile where
  parseJSON =
    Core.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Core.<$> (x Core..:? "CertificateAuthorityArn")
            Core.<*> (x Core..:? "TrustAnchors")
            Core.<*> (x Core..:? "CurrentPassword")
            Core.<*> (x Core..:? "EapMethod")
            Core.<*> (x Core..:? "NetworkProfileName")
            Core.<*> (x Core..:? "SecurityType")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "NextPassword")
            Core.<*> (x Core..:? "NetworkProfileArn")
            Core.<*> (x Core..:? "Ssid")
      )

instance Core.Hashable NetworkProfile

instance Core.NFData NetworkProfile
