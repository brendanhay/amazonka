-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NetworkInputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NetworkInputSettings
  ( NetworkInputSettings (..),

    -- * Smart constructor
    mkNetworkInputSettings,

    -- * Lenses
    nisHlsInputSettings,
    nisServerValidation,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsInputSettings
import Network.AWS.MediaLive.Types.NetworkInputServerValidation
import qualified Network.AWS.Prelude as Lude

-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
--
-- /See:/ 'mkNetworkInputSettings' smart constructor.
data NetworkInputSettings = NetworkInputSettings'
  { hlsInputSettings ::
      Lude.Maybe HlsInputSettings,
    serverValidation ::
      Lude.Maybe NetworkInputServerValidation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInputSettings' with the minimum fields required to make a request.
--
-- * 'hlsInputSettings' - Specifies HLS input settings when the uri is for a HLS manifest.
-- * 'serverValidation' - Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
mkNetworkInputSettings ::
  NetworkInputSettings
mkNetworkInputSettings =
  NetworkInputSettings'
    { hlsInputSettings = Lude.Nothing,
      serverValidation = Lude.Nothing
    }

-- | Specifies HLS input settings when the uri is for a HLS manifest.
--
-- /Note:/ Consider using 'hlsInputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisHlsInputSettings :: Lens.Lens' NetworkInputSettings (Lude.Maybe HlsInputSettings)
nisHlsInputSettings = Lens.lens (hlsInputSettings :: NetworkInputSettings -> Lude.Maybe HlsInputSettings) (\s a -> s {hlsInputSettings = a} :: NetworkInputSettings)
{-# DEPRECATED nisHlsInputSettings "Use generic-lens or generic-optics with 'hlsInputSettings' instead." #-}

-- | Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
--
-- /Note:/ Consider using 'serverValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisServerValidation :: Lens.Lens' NetworkInputSettings (Lude.Maybe NetworkInputServerValidation)
nisServerValidation = Lens.lens (serverValidation :: NetworkInputSettings -> Lude.Maybe NetworkInputServerValidation) (\s a -> s {serverValidation = a} :: NetworkInputSettings)
{-# DEPRECATED nisServerValidation "Use generic-lens or generic-optics with 'serverValidation' instead." #-}

instance Lude.FromJSON NetworkInputSettings where
  parseJSON =
    Lude.withObject
      "NetworkInputSettings"
      ( \x ->
          NetworkInputSettings'
            Lude.<$> (x Lude..:? "hlsInputSettings")
            Lude.<*> (x Lude..:? "serverValidation")
      )

instance Lude.ToJSON NetworkInputSettings where
  toJSON NetworkInputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hlsInputSettings" Lude..=) Lude.<$> hlsInputSettings,
            ("serverValidation" Lude..=) Lude.<$> serverValidation
          ]
      )
