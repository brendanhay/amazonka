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
    skpcResourceId,
    skpcDashSignaledSystemIds,
    skpcCertificateARN,
    skpcURL,
    skpcHlsSignaledSystemIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- /See:/ 'mkSpekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { resourceId ::
      Lude.Maybe Lude.Text,
    dashSignaledSystemIds :: Lude.Maybe [Lude.Text],
    certificateARN :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    hlsSignaledSystemIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpekeKeyProviderCmaf' with the minimum fields required to make a request.
--
-- * 'certificateARN' - If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
-- * 'dashSignaledSystemIds' - Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
-- * 'hlsSignaledSystemIds' - Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
-- * 'resourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
-- * 'url' - Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
mkSpekeKeyProviderCmaf ::
  SpekeKeyProviderCmaf
mkSpekeKeyProviderCmaf =
  SpekeKeyProviderCmaf'
    { resourceId = Lude.Nothing,
      dashSignaledSystemIds = Lude.Nothing,
      certificateARN = Lude.Nothing,
      url = Lude.Nothing,
      hlsSignaledSystemIds = Lude.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcResourceId :: Lens.Lens' SpekeKeyProviderCmaf (Lude.Maybe Lude.Text)
skpcResourceId = Lens.lens (resourceId :: SpekeKeyProviderCmaf -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: SpekeKeyProviderCmaf)
{-# DEPRECATED skpcResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
--
-- /Note:/ Consider using 'dashSignaledSystemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcDashSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Lude.Maybe [Lude.Text])
skpcDashSignaledSystemIds = Lens.lens (dashSignaledSystemIds :: SpekeKeyProviderCmaf -> Lude.Maybe [Lude.Text]) (\s a -> s {dashSignaledSystemIds = a} :: SpekeKeyProviderCmaf)
{-# DEPRECATED skpcDashSignaledSystemIds "Use generic-lens or generic-optics with 'dashSignaledSystemIds' instead." #-}

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcCertificateARN :: Lens.Lens' SpekeKeyProviderCmaf (Lude.Maybe Lude.Text)
skpcCertificateARN = Lens.lens (certificateARN :: SpekeKeyProviderCmaf -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: SpekeKeyProviderCmaf)
{-# DEPRECATED skpcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcURL :: Lens.Lens' SpekeKeyProviderCmaf (Lude.Maybe Lude.Text)
skpcURL = Lens.lens (url :: SpekeKeyProviderCmaf -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: SpekeKeyProviderCmaf)
{-# DEPRECATED skpcURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
--
-- /Note:/ Consider using 'hlsSignaledSystemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpcHlsSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Lude.Maybe [Lude.Text])
skpcHlsSignaledSystemIds = Lens.lens (hlsSignaledSystemIds :: SpekeKeyProviderCmaf -> Lude.Maybe [Lude.Text]) (\s a -> s {hlsSignaledSystemIds = a} :: SpekeKeyProviderCmaf)
{-# DEPRECATED skpcHlsSignaledSystemIds "Use generic-lens or generic-optics with 'hlsSignaledSystemIds' instead." #-}

instance Lude.FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    Lude.withObject
      "SpekeKeyProviderCmaf"
      ( \x ->
          SpekeKeyProviderCmaf'
            Lude.<$> (x Lude..:? "resourceId")
            Lude.<*> (x Lude..:? "dashSignaledSystemIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "hlsSignaledSystemIds" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resourceId" Lude..=) Lude.<$> resourceId,
            ("dashSignaledSystemIds" Lude..=) Lude.<$> dashSignaledSystemIds,
            ("certificateArn" Lude..=) Lude.<$> certificateARN,
            ("url" Lude..=) Lude.<$> url,
            ("hlsSignaledSystemIds" Lude..=) Lude.<$> hlsSignaledSystemIds
          ]
      )
