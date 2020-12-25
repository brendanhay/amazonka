{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.MediaLive.Types.HlsInputSettings as Types
import qualified Network.AWS.MediaLive.Types.NetworkInputServerValidation as Types
import qualified Network.AWS.Prelude as Core

-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
--
-- /See:/ 'mkNetworkInputSettings' smart constructor.
data NetworkInputSettings = NetworkInputSettings'
  { -- | Specifies HLS input settings when the uri is for a HLS manifest.
    hlsInputSettings :: Core.Maybe Types.HlsInputSettings,
    -- | Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
    serverValidation :: Core.Maybe Types.NetworkInputServerValidation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInputSettings' value with any optional fields omitted.
mkNetworkInputSettings ::
  NetworkInputSettings
mkNetworkInputSettings =
  NetworkInputSettings'
    { hlsInputSettings = Core.Nothing,
      serverValidation = Core.Nothing
    }

-- | Specifies HLS input settings when the uri is for a HLS manifest.
--
-- /Note:/ Consider using 'hlsInputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisHlsInputSettings :: Lens.Lens' NetworkInputSettings (Core.Maybe Types.HlsInputSettings)
nisHlsInputSettings = Lens.field @"hlsInputSettings"
{-# DEPRECATED nisHlsInputSettings "Use generic-lens or generic-optics with 'hlsInputSettings' instead." #-}

-- | Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
--
-- /Note:/ Consider using 'serverValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisServerValidation :: Lens.Lens' NetworkInputSettings (Core.Maybe Types.NetworkInputServerValidation)
nisServerValidation = Lens.field @"serverValidation"
{-# DEPRECATED nisServerValidation "Use generic-lens or generic-optics with 'serverValidation' instead." #-}

instance Core.FromJSON NetworkInputSettings where
  toJSON NetworkInputSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("hlsInputSettings" Core..=) Core.<$> hlsInputSettings,
            ("serverValidation" Core..=) Core.<$> serverValidation
          ]
      )

instance Core.FromJSON NetworkInputSettings where
  parseJSON =
    Core.withObject "NetworkInputSettings" Core.$
      \x ->
        NetworkInputSettings'
          Core.<$> (x Core..:? "hlsInputSettings")
          Core.<*> (x Core..:? "serverValidation")
