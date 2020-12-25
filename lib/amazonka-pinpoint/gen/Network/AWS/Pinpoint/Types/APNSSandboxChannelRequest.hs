{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
  ( APNSSandboxChannelRequest (..),

    -- * Smart constructor
    mkAPNSSandboxChannelRequest,

    -- * Lenses
    apnsscrfBundleId,
    apnsscrfCertificate,
    apnsscrfDefaultAuthenticationMethod,
    apnsscrfEnabled,
    apnsscrfPrivateKey,
    apnsscrfTeamId,
    apnsscrfTokenKey,
    apnsscrfTokenKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the APNs (Apple Push Notification service) sandbox channel for an application.
--
-- /See:/ 'mkAPNSSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
  { -- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
    bundleId :: Core.Maybe Core.Text,
    -- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
    certificate :: Core.Maybe Core.Text,
    -- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | Specifies whether to enable the APNs sandbox channel for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
    privateKey :: Core.Maybe Core.Text,
    -- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
    teamId :: Core.Maybe Core.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Core.Maybe Core.Text,
    -- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
    tokenKeyId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSSandboxChannelRequest' value with any optional fields omitted.
mkAPNSSandboxChannelRequest ::
  APNSSandboxChannelRequest
mkAPNSSandboxChannelRequest =
  APNSSandboxChannelRequest'
    { bundleId = Core.Nothing,
      certificate = Core.Nothing,
      defaultAuthenticationMethod = Core.Nothing,
      enabled = Core.Nothing,
      privateKey = Core.Nothing,
      teamId = Core.Nothing,
      tokenKey = Core.Nothing,
      tokenKeyId = Core.Nothing
    }

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfBundleId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfBundleId = Lens.field @"bundleId"
{-# DEPRECATED apnsscrfBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfCertificate :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfCertificate = Lens.field @"certificate"
{-# DEPRECATED apnsscrfCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfDefaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# DEPRECATED apnsscrfDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether to enable the APNs sandbox channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfEnabled :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Bool)
apnsscrfEnabled = Lens.field @"enabled"
{-# DEPRECATED apnsscrfEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfPrivateKey :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfPrivateKey = Lens.field @"privateKey"
{-# DEPRECATED apnsscrfPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfTeamId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfTeamId = Lens.field @"teamId"
{-# DEPRECATED apnsscrfTeamId "Use generic-lens or generic-optics with 'teamId' instead." #-}

-- | The authentication key to use for APNs tokens.
--
-- /Note:/ Consider using 'tokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfTokenKey :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfTokenKey = Lens.field @"tokenKey"
{-# DEPRECATED apnsscrfTokenKey "Use generic-lens or generic-optics with 'tokenKey' instead." #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrfTokenKeyId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
apnsscrfTokenKeyId = Lens.field @"tokenKeyId"
{-# DEPRECATED apnsscrfTokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead." #-}

instance Core.FromJSON APNSSandboxChannelRequest where
  toJSON APNSSandboxChannelRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("BundleId" Core..=) Core.<$> bundleId,
            ("Certificate" Core..=) Core.<$> certificate,
            ("DefaultAuthenticationMethod" Core..=)
              Core.<$> defaultAuthenticationMethod,
            ("Enabled" Core..=) Core.<$> enabled,
            ("PrivateKey" Core..=) Core.<$> privateKey,
            ("TeamId" Core..=) Core.<$> teamId,
            ("TokenKey" Core..=) Core.<$> tokenKey,
            ("TokenKeyId" Core..=) Core.<$> tokenKeyId
          ]
      )
