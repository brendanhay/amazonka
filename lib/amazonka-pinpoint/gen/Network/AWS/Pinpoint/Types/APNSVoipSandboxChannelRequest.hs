{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
  ( APNSVoipSandboxChannelRequest (..)
  -- * Smart constructor
  , mkAPNSVoipSandboxChannelRequest
  -- * Lenses
  , aBundleId
  , aCertificate
  , aDefaultAuthenticationMethod
  , aEnabled
  , aPrivateKey
  , aTeamId
  , aTokenKey
  , aTokenKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'mkAPNSVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { bundleId :: Core.Maybe Core.Text
    -- ^ The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
  , certificate :: Core.Maybe Core.Text
    -- ^ The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
  , defaultAuthenticationMethod :: Core.Maybe Core.Text
    -- ^ The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the APNs VoIP sandbox channel is enabled for the application.
  , privateKey :: Core.Maybe Core.Text
    -- ^ The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
  , teamId :: Core.Maybe Core.Text
    -- ^ The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
  , tokenKey :: Core.Maybe Core.Text
    -- ^ The authentication key to use for APNs tokens.
  , tokenKeyId :: Core.Maybe Core.Text
    -- ^ The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSVoipSandboxChannelRequest' value with any optional fields omitted.
mkAPNSVoipSandboxChannelRequest
    :: APNSVoipSandboxChannelRequest
mkAPNSVoipSandboxChannelRequest
  = APNSVoipSandboxChannelRequest'{bundleId = Core.Nothing,
                                   certificate = Core.Nothing,
                                   defaultAuthenticationMethod = Core.Nothing,
                                   enabled = Core.Nothing, privateKey = Core.Nothing,
                                   teamId = Core.Nothing, tokenKey = Core.Nothing,
                                   tokenKeyId = Core.Nothing}

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBundleId :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aBundleId = Lens.field @"bundleId"
{-# INLINEABLE aBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCertificate :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aCertificate = Lens.field @"certificate"
{-# INLINEABLE aCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDefaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# INLINEABLE aDefaultAuthenticationMethod #-}
{-# DEPRECATED defaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead"  #-}

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnabled :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Bool)
aEnabled = Lens.field @"enabled"
{-# INLINEABLE aEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrivateKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE aPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTeamId :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aTeamId = Lens.field @"teamId"
{-# INLINEABLE aTeamId #-}
{-# DEPRECATED teamId "Use generic-lens or generic-optics with 'teamId' instead"  #-}

-- | The authentication key to use for APNs tokens.
--
-- /Note:/ Consider using 'tokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTokenKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aTokenKey = Lens.field @"tokenKey"
{-# INLINEABLE aTokenKey #-}
{-# DEPRECATED tokenKey "Use generic-lens or generic-optics with 'tokenKey' instead"  #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTokenKeyId :: Lens.Lens' APNSVoipSandboxChannelRequest (Core.Maybe Core.Text)
aTokenKeyId = Lens.field @"tokenKeyId"
{-# INLINEABLE aTokenKeyId #-}
{-# DEPRECATED tokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead"  #-}

instance Core.FromJSON APNSVoipSandboxChannelRequest where
        toJSON APNSVoipSandboxChannelRequest{..}
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
