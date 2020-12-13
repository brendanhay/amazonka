{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelRequest
  ( APNSChannelRequest (..),

    -- * Smart constructor
    mkAPNSChannelRequest,

    -- * Lenses
    acrTokenKey,
    acrPrivateKey,
    acrEnabled,
    acrTeamId,
    acrBundleId,
    acrDefaultAuthenticationMethod,
    acrCertificate,
    acrTokenKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) channel for an application.
--
-- /See:/ 'mkAPNSChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
  { -- | The authentication key to use for APNs tokens.
    tokenKey :: Lude.Maybe Lude.Text,
    -- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
    privateKey :: Lude.Maybe Lude.Text,
    -- | Specifies whether to enable the APNs channel for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
    teamId :: Lude.Maybe Lude.Text,
    -- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
    bundleId :: Lude.Maybe Lude.Text,
    -- | The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
    defaultAuthenticationMethod :: Lude.Maybe Lude.Text,
    -- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
    certificate :: Lude.Maybe Lude.Text,
    -- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
    tokenKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSChannelRequest' with the minimum fields required to make a request.
--
-- * 'tokenKey' - The authentication key to use for APNs tokens.
-- * 'privateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
-- * 'enabled' - Specifies whether to enable the APNs channel for the application.
-- * 'teamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
-- * 'bundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
-- * 'defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
-- * 'certificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
-- * 'tokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
mkAPNSChannelRequest ::
  APNSChannelRequest
mkAPNSChannelRequest =
  APNSChannelRequest'
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
acrTokenKey :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrTokenKey = Lens.lens (tokenKey :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKey = a} :: APNSChannelRequest)
{-# DEPRECATED acrTokenKey "Use generic-lens or generic-optics with 'tokenKey' instead." #-}

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrPrivateKey :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrPrivateKey = Lens.lens (privateKey :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {privateKey = a} :: APNSChannelRequest)
{-# DEPRECATED acrPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | Specifies whether to enable the APNs channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrEnabled :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Bool)
acrEnabled = Lens.lens (enabled :: APNSChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSChannelRequest)
{-# DEPRECATED acrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'teamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrTeamId :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrTeamId = Lens.lens (teamId :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {teamId = a} :: APNSChannelRequest)
{-# DEPRECATED acrTeamId "Use generic-lens or generic-optics with 'teamId' instead." #-}

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrBundleId :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrBundleId = Lens.lens (bundleId :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: APNSChannelRequest)
{-# DEPRECATED acrBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrDefaultAuthenticationMethod :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSChannelRequest)
{-# DEPRECATED acrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrCertificate :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrCertificate = Lens.lens (certificate :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: APNSChannelRequest)
{-# DEPRECATED acrCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
--
-- /Note:/ Consider using 'tokenKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrTokenKeyId :: Lens.Lens' APNSChannelRequest (Lude.Maybe Lude.Text)
acrTokenKeyId = Lens.lens (tokenKeyId :: APNSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyId = a} :: APNSChannelRequest)
{-# DEPRECATED acrTokenKeyId "Use generic-lens or generic-optics with 'tokenKeyId' instead." #-}

instance Lude.ToJSON APNSChannelRequest where
  toJSON APNSChannelRequest' {..} =
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
