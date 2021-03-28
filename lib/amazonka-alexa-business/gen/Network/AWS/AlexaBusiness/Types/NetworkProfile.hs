{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.NetworkProfile
  ( NetworkProfile (..)
  -- * Smart constructor
  , mkNetworkProfile
  -- * Lenses
  , npCertificateAuthorityArn
  , npCurrentPassword
  , npDescription
  , npEapMethod
  , npNetworkProfileArn
  , npNetworkProfileName
  , npNextPassword
  , npSecurityType
  , npSsid
  , npTrustAnchors
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.CurrentWiFiPassword as Types
import qualified Network.AWS.AlexaBusiness.Types.NetworkEapMethod as Types
import qualified Network.AWS.AlexaBusiness.Types.NetworkProfileDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.NetworkProfileName as Types
import qualified Network.AWS.AlexaBusiness.Types.NetworkSecurityType as Types
import qualified Network.AWS.AlexaBusiness.Types.NextWiFiPassword as Types
import qualified Network.AWS.AlexaBusiness.Types.Ssid as Types
import qualified Network.AWS.AlexaBusiness.Types.TrustAnchor as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The network profile associated with a device.
--
-- /See:/ 'mkNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { certificateAuthorityArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices. 
  , currentPassword :: Core.Maybe Types.CurrentWiFiPassword
    -- ^ The current password of the Wi-Fi network.
  , description :: Core.Maybe Types.NetworkProfileDescription
    -- ^ Detailed information about a device's network profile.
  , eapMethod :: Core.Maybe Types.NetworkEapMethod
    -- ^ The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported. 
  , networkProfileArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the network profile associated with a device.
  , networkProfileName :: Core.Maybe Types.NetworkProfileName
    -- ^ The name of the network profile associated with a device.
  , nextPassword :: Core.Maybe Types.NextWiFiPassword
    -- ^ The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword. 
  , securityType :: Core.Maybe Types.NetworkSecurityType
    -- ^ The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
  , ssid :: Core.Maybe Types.Ssid
    -- ^ The SSID of the Wi-Fi network.
  , trustAnchors :: Core.Maybe (Core.NonEmpty Types.TrustAnchor)
    -- ^ The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkProfile' value with any optional fields omitted.
mkNetworkProfile
    :: NetworkProfile
mkNetworkProfile
  = NetworkProfile'{certificateAuthorityArn = Core.Nothing,
                    currentPassword = Core.Nothing, description = Core.Nothing,
                    eapMethod = Core.Nothing, networkProfileArn = Core.Nothing,
                    networkProfileName = Core.Nothing, nextPassword = Core.Nothing,
                    securityType = Core.Nothing, ssid = Core.Nothing,
                    trustAnchors = Core.Nothing}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices. 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npCertificateAuthorityArn :: Lens.Lens' NetworkProfile (Core.Maybe Types.Arn)
npCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE npCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npCurrentPassword :: Lens.Lens' NetworkProfile (Core.Maybe Types.CurrentWiFiPassword)
npCurrentPassword = Lens.field @"currentPassword"
{-# INLINEABLE npCurrentPassword #-}
{-# DEPRECATED currentPassword "Use generic-lens or generic-optics with 'currentPassword' instead"  #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDescription :: Lens.Lens' NetworkProfile (Core.Maybe Types.NetworkProfileDescription)
npDescription = Lens.field @"description"
{-# INLINEABLE npDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported. 
--
-- /Note:/ Consider using 'eapMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npEapMethod :: Lens.Lens' NetworkProfile (Core.Maybe Types.NetworkEapMethod)
npEapMethod = Lens.field @"eapMethod"
{-# INLINEABLE npEapMethod #-}
{-# DEPRECATED eapMethod "Use generic-lens or generic-optics with 'eapMethod' instead"  #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNetworkProfileArn :: Lens.Lens' NetworkProfile (Core.Maybe Types.Arn)
npNetworkProfileArn = Lens.field @"networkProfileArn"
{-# INLINEABLE npNetworkProfileArn #-}
{-# DEPRECATED networkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead"  #-}

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNetworkProfileName :: Lens.Lens' NetworkProfile (Core.Maybe Types.NetworkProfileName)
npNetworkProfileName = Lens.field @"networkProfileName"
{-# INLINEABLE npNetworkProfileName #-}
{-# DEPRECATED networkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead"  #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword. 
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNextPassword :: Lens.Lens' NetworkProfile (Core.Maybe Types.NextWiFiPassword)
npNextPassword = Lens.field @"nextPassword"
{-# INLINEABLE npNextPassword #-}
{-# DEPRECATED nextPassword "Use generic-lens or generic-optics with 'nextPassword' instead"  #-}

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- /Note:/ Consider using 'securityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npSecurityType :: Lens.Lens' NetworkProfile (Core.Maybe Types.NetworkSecurityType)
npSecurityType = Lens.field @"securityType"
{-# INLINEABLE npSecurityType #-}
{-# DEPRECATED securityType "Use generic-lens or generic-optics with 'securityType' instead"  #-}

-- | The SSID of the Wi-Fi network.
--
-- /Note:/ Consider using 'ssid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npSsid :: Lens.Lens' NetworkProfile (Core.Maybe Types.Ssid)
npSsid = Lens.field @"ssid"
{-# INLINEABLE npSsid #-}
{-# DEPRECATED ssid "Use generic-lens or generic-optics with 'ssid' instead"  #-}

-- | The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npTrustAnchors :: Lens.Lens' NetworkProfile (Core.Maybe (Core.NonEmpty Types.TrustAnchor))
npTrustAnchors = Lens.field @"trustAnchors"
{-# INLINEABLE npTrustAnchors #-}
{-# DEPRECATED trustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead"  #-}

instance Core.FromJSON NetworkProfile where
        parseJSON
          = Core.withObject "NetworkProfile" Core.$
              \ x ->
                NetworkProfile' Core.<$>
                  (x Core..:? "CertificateAuthorityArn") Core.<*>
                    x Core..:? "CurrentPassword"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "EapMethod"
                    Core.<*> x Core..:? "NetworkProfileArn"
                    Core.<*> x Core..:? "NetworkProfileName"
                    Core.<*> x Core..:? "NextPassword"
                    Core.<*> x Core..:? "SecurityType"
                    Core.<*> x Core..:? "Ssid"
                    Core.<*> x Core..:? "TrustAnchors"
