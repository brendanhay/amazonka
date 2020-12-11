-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
  ( APNSVoipChannelRequest (..),

    -- * Smart constructor
    mkAPNSVoipChannelRequest,

    -- * Lenses
    avcrTokenKey,
    avcrPrivateKey,
    avcrEnabled,
    avcrTeamId,
    avcrBundleId,
    avcrDefaultAuthenticationMethod,
    avcrCertificate,
    avcrTokenKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
-- /See:/ 'mkAPNSVoipChannelRequest' smart constructor.
data APNSVoipChannelRequest = APNSVoipChannelRequest'
  { tokenKey ::
      Lude.Maybe Lude.Text,
    privateKey :: Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    teamId :: Lude.Maybe Lude.Text,
    bundleId :: Lude.Maybe Lude.Text,
    defaultAuthenticationMethod ::
      Lude.Maybe Lude.Text,
    certificate :: Lude.Maybe Lude.Text,
    tokenKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSVoipChannelRequest' with the minimum fields required to make a request.
--
-- * 'bundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
-- * 'certificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
-- * 'defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
-- * 'enabled' - Specifies whether to enable the APNs VoIP channel for the application.
-- * 'privateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
-- * 'teamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
-- * 'tokenKey' - The authentication key to use for APNs tokens.
-- * 'tokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
mkAPNSVoipChannelRequest ::
  APNSVoipChannelRequest
mkAPNSVoipChannelRequest =
  APNSVoipChannelRequest'
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
avcrTokenKey :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrTokenKey = Lens.lens (tokenKey :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKey = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrTokenKey "Use generic-lens or generic-optics with 'tokenKey' instead." #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrPrivateKey :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrPrivateKey = Lens.lens (privateKey :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {privateKey = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | Specifies whether to enable the APNs VoIP channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrEnabled :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Bool)
avcrEnabled = Lens.lens (enabled :: APNSVoipChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrTeamId :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrTeamId = Lens.lens (teamId :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {teamId = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrTeamId "Use generic-lens or generic-optics with 'teamId' instead." #-}

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrBundleId :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrBundleId = Lens.lens (bundleId :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrDefaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrCertificate :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrCertificate = Lens.lens (certificate :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcrTokenKeyId :: Lens.Lens' APNSVoipChannelRequest (Lude.Maybe Lude.Text)
avcrTokenKeyId = Lens.lens (tokenKeyId :: APNSVoipChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyId = a} :: APNSVoipChannelRequest)
{-# DEPRECATED avcrTokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead." #-}

instance Lude.ToJSON APNSVoipChannelRequest where
  toJSON APNSVoipChannelRequest' {..} =
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
