{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProvider
  ( SpekeKeyProvider (..),

    -- * Smart constructor
    mkSpekeKeyProvider,

    -- * Lenses
    sResourceId,
    sCertificateARN,
    sURL,
    sSystemIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /See:/ 'mkSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { resourceId ::
      Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    systemIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- * 'certificateARN' - If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
-- * 'resourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
-- * 'systemIds' - Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See
--
--  https://dashif.org/identifiers/content_protection/ for more details.
-- * 'url' - Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
mkSpekeKeyProvider ::
  SpekeKeyProvider
mkSpekeKeyProvider =
  SpekeKeyProvider'
    { resourceId = Lude.Nothing,
      certificateARN = Lude.Nothing,
      url = Lude.Nothing,
      systemIds = Lude.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceId :: Lens.Lens' SpekeKeyProvider (Lude.Maybe Lude.Text)
sResourceId = Lens.lens (resourceId :: SpekeKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: SpekeKeyProvider)
{-# DEPRECATED sResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateARN :: Lens.Lens' SpekeKeyProvider (Lude.Maybe Lude.Text)
sCertificateARN = Lens.lens (certificateARN :: SpekeKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: SpekeKeyProvider)
{-# DEPRECATED sCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sURL :: Lens.Lens' SpekeKeyProvider (Lude.Maybe Lude.Text)
sURL = Lens.lens (url :: SpekeKeyProvider -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: SpekeKeyProvider)
{-# DEPRECATED sURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See
--
--  https://dashif.org/identifiers/content_protection/ for more details.
--
-- /Note:/ Consider using 'systemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSystemIds :: Lens.Lens' SpekeKeyProvider (Lude.Maybe [Lude.Text])
sSystemIds = Lens.lens (systemIds :: SpekeKeyProvider -> Lude.Maybe [Lude.Text]) (\s a -> s {systemIds = a} :: SpekeKeyProvider)
{-# DEPRECATED sSystemIds "Use generic-lens or generic-optics with 'systemIds' instead." #-}

instance Lude.FromJSON SpekeKeyProvider where
  parseJSON =
    Lude.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Lude.<$> (x Lude..:? "resourceId")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "systemIds" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resourceId" Lude..=) Lude.<$> resourceId,
            ("certificateArn" Lude..=) Lude.<$> certificateARN,
            ("url" Lude..=) Lude.<$> url,
            ("systemIds" Lude..=) Lude.<$> systemIds
          ]
      )
