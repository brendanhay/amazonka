{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile with the specified details.
module Network.AWS.AlexaBusiness.CreateNetworkProfile
  ( -- * Creating a request
    CreateNetworkProfile (..),
    mkCreateNetworkProfile,

    -- ** Request lenses
    cnpNetworkProfileName,
    cnpSsid,
    cnpSecurityType,
    cnpClientRequestToken,
    cnpCertificateAuthorityArn,
    cnpCurrentPassword,
    cnpDescription,
    cnpEapMethod,
    cnpNextPassword,
    cnpTrustAnchors,

    -- * Destructuring the response
    CreateNetworkProfileResponse (..),
    mkCreateNetworkProfileResponse,

    -- ** Response lenses
    cnprrsNetworkProfileArn,
    cnprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The name of the network profile associated with a device.
    networkProfileName :: Types.NetworkProfileName,
    -- | The SSID of the Wi-Fi network.
    ssid :: Types.Ssid,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: Types.NetworkSecurityType,
    clientRequestToken :: Types.ClientRequestToken,
    -- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
    certificateAuthorityArn :: Core.Maybe Types.Arn,
    -- | The current password of the Wi-Fi network.
    currentPassword :: Core.Maybe Types.CurrentWiFiPassword,
    -- | Detailed information about a device's network profile.
    description :: Core.Maybe Types.Description,
    -- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
    eapMethod :: Core.Maybe Types.NetworkEapMethod,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
    nextPassword :: Core.Maybe Types.NextPassword,
    -- | The root certificates of your authentication server that is installed on your devices and used to trust your authentication server during EAP negotiation.
    trustAnchors :: Core.Maybe (Core.NonEmpty Types.TrustAnchor)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkProfile' value with any optional fields omitted.
mkCreateNetworkProfile ::
  -- | 'networkProfileName'
  Types.NetworkProfileName ->
  -- | 'ssid'
  Types.Ssid ->
  -- | 'securityType'
  Types.NetworkSecurityType ->
  -- | 'clientRequestToken'
  Types.ClientRequestToken ->
  CreateNetworkProfile
mkCreateNetworkProfile
  networkProfileName
  ssid
  securityType
  clientRequestToken =
    CreateNetworkProfile'
      { networkProfileName,
        ssid,
        securityType,
        clientRequestToken,
        certificateAuthorityArn = Core.Nothing,
        currentPassword = Core.Nothing,
        description = Core.Nothing,
        eapMethod = Core.Nothing,
        nextPassword = Core.Nothing,
        trustAnchors = Core.Nothing
      }

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpNetworkProfileName :: Lens.Lens' CreateNetworkProfile Types.NetworkProfileName
cnpNetworkProfileName = Lens.field @"networkProfileName"
{-# DEPRECATED cnpNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The SSID of the Wi-Fi network.
--
-- /Note:/ Consider using 'ssid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpSsid :: Lens.Lens' CreateNetworkProfile Types.Ssid
cnpSsid = Lens.field @"ssid"
{-# DEPRECATED cnpSsid "Use generic-lens or generic-optics with 'ssid' instead." #-}

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- /Note:/ Consider using 'securityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpSecurityType :: Lens.Lens' CreateNetworkProfile Types.NetworkSecurityType
cnpSecurityType = Lens.field @"securityType"
{-# DEPRECATED cnpSecurityType "Use generic-lens or generic-optics with 'securityType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpClientRequestToken :: Lens.Lens' CreateNetworkProfile Types.ClientRequestToken
cnpClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cnpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpCertificateAuthorityArn :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.Arn)
cnpCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED cnpCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpCurrentPassword :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.CurrentWiFiPassword)
cnpCurrentPassword = Lens.field @"currentPassword"
{-# DEPRECATED cnpCurrentPassword "Use generic-lens or generic-optics with 'currentPassword' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDescription :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.Description)
cnpDescription = Lens.field @"description"
{-# DEPRECATED cnpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- /Note:/ Consider using 'eapMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpEapMethod :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.NetworkEapMethod)
cnpEapMethod = Lens.field @"eapMethod"
{-# DEPRECATED cnpEapMethod "Use generic-lens or generic-optics with 'eapMethod' instead." #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpNextPassword :: Lens.Lens' CreateNetworkProfile (Core.Maybe Types.NextPassword)
cnpNextPassword = Lens.field @"nextPassword"
{-# DEPRECATED cnpNextPassword "Use generic-lens or generic-optics with 'nextPassword' instead." #-}

-- | The root certificates of your authentication server that is installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpTrustAnchors :: Lens.Lens' CreateNetworkProfile (Core.Maybe (Core.NonEmpty Types.TrustAnchor))
cnpTrustAnchors = Lens.field @"trustAnchors"
{-# DEPRECATED cnpTrustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead." #-}

instance Core.FromJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NetworkProfileName" Core..= networkProfileName),
            Core.Just ("Ssid" Core..= ssid),
            Core.Just ("SecurityType" Core..= securityType),
            Core.Just ("ClientRequestToken" Core..= clientRequestToken),
            ("CertificateAuthorityArn" Core..=)
              Core.<$> certificateAuthorityArn,
            ("CurrentPassword" Core..=) Core.<$> currentPassword,
            ("Description" Core..=) Core.<$> description,
            ("EapMethod" Core..=) Core.<$> eapMethod,
            ("NextPassword" Core..=) Core.<$> nextPassword,
            ("TrustAnchors" Core..=) Core.<$> trustAnchors
          ]
      )

instance Core.AWSRequest CreateNetworkProfile where
  type Rs CreateNetworkProfile = CreateNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateNetworkProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Core.<$> (x Core..:? "NetworkProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Maybe Types.NetworkProfileArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkProfileResponse' value with any optional fields omitted.
mkCreateNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNetworkProfileResponse
mkCreateNetworkProfileResponse responseStatus =
  CreateNetworkProfileResponse'
    { networkProfileArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsNetworkProfileArn :: Lens.Lens' CreateNetworkProfileResponse (Core.Maybe Types.NetworkProfileArn)
cnprrsNetworkProfileArn = Lens.field @"networkProfileArn"
{-# DEPRECATED cnprrsNetworkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprrsResponseStatus :: Lens.Lens' CreateNetworkProfileResponse Core.Int
cnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
