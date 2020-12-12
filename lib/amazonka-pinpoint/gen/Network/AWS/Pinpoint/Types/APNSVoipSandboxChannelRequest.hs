{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
  ( APNSVoipSandboxChannelRequest (..),

    -- * Smart constructor
    mkAPNSVoipSandboxChannelRequest,

    -- * Lenses
    avscrTokenKey,
    avscrPrivateKey,
    avscrEnabled,
    avscrTeamId,
    avscrBundleId,
    avscrDefaultAuthenticationMethod,
    avscrCertificate,
    avscrTokenKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'mkAPNSVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { tokenKey ::
      Lude.Maybe Lude.Text,
    privateKey ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    teamId :: Lude.Maybe Lude.Text,
    bundleId ::
      Lude.Maybe Lude.Text,
    defaultAuthenticationMethod ::
      Lude.Maybe Lude.Text,
    certificate ::
      Lude.Maybe Lude.Text,
    tokenKeyId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSVoipSandboxChannelRequest' with the minimum fields required to make a request.
--
-- * 'bundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
-- * 'certificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
-- * 'defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
-- * 'enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the application.
-- * 'privateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
-- * 'teamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
-- * 'tokenKey' - The authentication key to use for APNs tokens.
-- * 'tokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
mkAPNSVoipSandboxChannelRequest ::
  APNSVoipSandboxChannelRequest
mkAPNSVoipSandboxChannelRequest =
  APNSVoipSandboxChannelRequest'
    { tokenKey = Lude.Nothing,
      privateKey = Lude.Nothing,
      enabled = Lude.Nothing,
      teamId = Lude.Nothing,
      bundleId = Lude.Nothing,
      defaultAuthenticationMethod = Lude.Nothing,
      certificate = Lude.Nothing,
      tokenKeyId = Lude.Nothing
    }

-- | The authentication key to use for APNs tokens.
--
-- /Note:/ Consider using 'tokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrTokenKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrTokenKey = Lens.lens (tokenKey :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKey = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrTokenKey "Use generic-lens or generic-optics with 'tokenKey' instead." #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrPrivateKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrPrivateKey = Lens.lens (privateKey :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {privateKey = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrEnabled :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Bool)
avscrEnabled = Lens.lens (enabled :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrTeamId :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrTeamId = Lens.lens (teamId :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {teamId = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrTeamId "Use generic-lens or generic-optics with 'teamId' instead." #-}

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrBundleId :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrBundleId = Lens.lens (bundleId :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrDefaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrCertificate :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrCertificate = Lens.lens (certificate :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscrTokenKeyId :: Lens.Lens' APNSVoipSandboxChannelRequest (Lude.Maybe Lude.Text)
avscrTokenKeyId = Lens.lens (tokenKeyId :: APNSVoipSandboxChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyId = a} :: APNSVoipSandboxChannelRequest)
{-# DEPRECATED avscrTokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead." #-}

instance Lude.ToJSON APNSVoipSandboxChannelRequest where
  toJSON APNSVoipSandboxChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TokenKey" Lude..=) Lude.<$> tokenKey,
            ("PrivateKey" Lude..=) Lude.<$> privateKey,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("TeamId" Lude..=) Lude.<$> teamId,
            ("BundleId" Lude..=) Lude.<$> bundleId,
            ("DefaultAuthenticationMethod" Lude..=)
              Lude.<$> defaultAuthenticationMethod,
            ("Certificate" Lude..=) Lude.<$> certificate,
            ("TokenKeyId" Lude..=) Lude.<$> tokenKeyId
          ]
      )
