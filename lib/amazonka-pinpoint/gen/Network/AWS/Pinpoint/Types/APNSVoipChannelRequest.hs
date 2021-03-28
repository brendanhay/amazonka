{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
  ( APNSVoipChannelRequest (..)
  -- * Smart constructor
  , mkAPNSVoipChannelRequest
  -- * Lenses
  , apnsvcrfBundleId
  , apnsvcrfCertificate
  , apnsvcrfDefaultAuthenticationMethod
  , apnsvcrfEnabled
  , apnsvcrfPrivateKey
  , apnsvcrfTeamId
  , apnsvcrfTokenKey
  , apnsvcrfTokenKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
-- /See:/ 'mkAPNSVoipChannelRequest' smart constructor.
data APNSVoipChannelRequest = APNSVoipChannelRequest'
  { bundleId :: Core.Maybe Core.Text
    -- ^ The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
  , certificate :: Core.Maybe Core.Text
    -- ^ The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
  , defaultAuthenticationMethod :: Core.Maybe Core.Text
    -- ^ The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable the APNs VoIP channel for the application.
  , privateKey :: Core.Maybe Core.Text
    -- ^ The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
  , teamId :: Core.Maybe Core.Text
    -- ^ The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
  , tokenKey :: Core.Maybe Core.Text
    -- ^ The authentication key to use for APNs tokens.
  , tokenKeyId :: Core.Maybe Core.Text
    -- ^ The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSVoipChannelRequest' value with any optional fields omitted.
mkAPNSVoipChannelRequest
    :: APNSVoipChannelRequest
mkAPNSVoipChannelRequest
  = APNSVoipChannelRequest'{bundleId = Core.Nothing,
                            certificate = Core.Nothing,
                            defaultAuthenticationMethod = Core.Nothing, enabled = Core.Nothing,
                            privateKey = Core.Nothing, teamId = Core.Nothing,
                            tokenKey = Core.Nothing, tokenKeyId = Core.Nothing}

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfBundleId :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfBundleId = Lens.field @"bundleId"
{-# INLINEABLE apnsvcrfBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfCertificate :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfCertificate = Lens.field @"certificate"
{-# INLINEABLE apnsvcrfCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfDefaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# INLINEABLE apnsvcrfDefaultAuthenticationMethod #-}
{-# DEPRECATED defaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead"  #-}

-- | Specifies whether to enable the APNs VoIP channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfEnabled :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Bool)
apnsvcrfEnabled = Lens.field @"enabled"
{-# INLINEABLE apnsvcrfEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfPrivateKey :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE apnsvcrfPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfTeamId :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfTeamId = Lens.field @"teamId"
{-# INLINEABLE apnsvcrfTeamId #-}
{-# DEPRECATED teamId "Use generic-lens or generic-optics with 'teamId' instead"  #-}

-- | The authentication key to use for APNs tokens.
--
-- /Note:/ Consider using 'tokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfTokenKey :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfTokenKey = Lens.field @"tokenKey"
{-# INLINEABLE apnsvcrfTokenKey #-}
{-# DEPRECATED tokenKey "Use generic-lens or generic-optics with 'tokenKey' instead"  #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrfTokenKeyId :: Lens.Lens' APNSVoipChannelRequest (Core.Maybe Core.Text)
apnsvcrfTokenKeyId = Lens.field @"tokenKeyId"
{-# INLINEABLE apnsvcrfTokenKeyId #-}
{-# DEPRECATED tokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead"  #-}

instance Core.FromJSON APNSVoipChannelRequest where
        toJSON APNSVoipChannelRequest{..}
          = Core.object
              (Core.catMaybes
                 [("BundleId" Core..=) Core.<$> bundleId,
                  ("Certificate" Core..=) Core.<$> certificate,
                  ("DefaultAuthenticationMethod" Core..=) Core.<$>
                    defaultAuthenticationMethod,
                  ("Enabled" Core..=) Core.<$> enabled,
                  ("PrivateKey" Core..=) Core.<$> privateKey,
                  ("TeamId" Core..=) Core.<$> teamId,
                  ("TokenKey" Core..=) Core.<$> tokenKey,
                  ("TokenKeyId" Core..=) Core.<$> tokenKeyId])
