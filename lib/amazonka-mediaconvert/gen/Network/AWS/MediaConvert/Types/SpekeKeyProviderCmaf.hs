{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
  ( SpekeKeyProviderCmaf (..),

    -- * Smart constructor
    mkSpekeKeyProviderCmaf,

    -- * Lenses
    skpcCertificateArn,
    skpcDashSignaledSystemIds,
    skpcHlsSignaledSystemIds,
    skpcResourceId,
    skpcUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- /See:/ 'mkSpekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { -- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
    certificateArn :: Core.Maybe Core.Text,
    -- | Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
    dashSignaledSystemIds :: Core.Maybe [Core.Text],
    -- | Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
    hlsSignaledSystemIds :: Core.Maybe [Core.Text],
    -- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
    resourceId :: Core.Maybe Core.Text,
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
    url :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpekeKeyProviderCmaf' value with any optional fields omitted.
mkSpekeKeyProviderCmaf ::
  SpekeKeyProviderCmaf
mkSpekeKeyProviderCmaf =
  SpekeKeyProviderCmaf'
    { certificateArn = Core.Nothing,
      dashSignaledSystemIds = Core.Nothing,
      hlsSignaledSystemIds = Core.Nothing,
      resourceId = Core.Nothing,
      url = Core.Nothing
    }

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcCertificateArn :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
skpcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED skpcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
--
-- /Note:/ Consider using 'dashSignaledSystemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcDashSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe [Core.Text])
skpcDashSignaledSystemIds = Lens.field @"dashSignaledSystemIds"
{-# DEPRECATED skpcDashSignaledSystemIds "Use generic-lens or generic-optics with 'dashSignaledSystemIds' instead." #-}

-- | Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
--
-- /Note:/ Consider using 'hlsSignaledSystemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcHlsSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe [Core.Text])
skpcHlsSignaledSystemIds = Lens.field @"hlsSignaledSystemIds"
{-# DEPRECATED skpcHlsSignaledSystemIds "Use generic-lens or generic-optics with 'hlsSignaledSystemIds' instead." #-}

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcResourceId :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
skpcResourceId = Lens.field @"resourceId"
{-# DEPRECATED skpcResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcUrl :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
skpcUrl = Lens.field @"url"
{-# DEPRECATED skpcUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf {..} =
    Core.object
      ( Core.catMaybes
          [ ("certificateArn" Core..=) Core.<$> certificateArn,
            ("dashSignaledSystemIds" Core..=) Core.<$> dashSignaledSystemIds,
            ("hlsSignaledSystemIds" Core..=) Core.<$> hlsSignaledSystemIds,
            ("resourceId" Core..=) Core.<$> resourceId,
            ("url" Core..=) Core.<$> url
          ]
      )

instance Core.FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    Core.withObject "SpekeKeyProviderCmaf" Core.$
      \x ->
        SpekeKeyProviderCmaf'
          Core.<$> (x Core..:? "certificateArn")
          Core.<*> (x Core..:? "dashSignaledSystemIds")
          Core.<*> (x Core..:? "hlsSignaledSystemIds")
          Core.<*> (x Core..:? "resourceId")
          Core.<*> (x Core..:? "url")
