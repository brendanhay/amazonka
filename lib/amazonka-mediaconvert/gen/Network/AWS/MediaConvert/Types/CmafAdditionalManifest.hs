{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafAdditionalManifest
  ( CmafAdditionalManifest (..),

    -- * Smart constructor
    mkCmafAdditionalManifest,

    -- * Lenses
    camManifestNameModifier,
    camSelectedOutputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specify the details for each pair of HLS and DASH additional manifests that you want the service to generate for this CMAF output group. Each pair of manifests can reference a different subset of outputs in the group.
--
-- /See:/ 'mkCmafAdditionalManifest' smart constructor.
data CmafAdditionalManifest = CmafAdditionalManifest'
  { -- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
    manifestNameModifier :: Lude.Maybe Lude.Text,
    -- | Specify the outputs that you want this additional top-level manifest to reference.
    selectedOutputs :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafAdditionalManifest' with the minimum fields required to make a request.
--
-- * 'manifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
-- * 'selectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
mkCmafAdditionalManifest ::
  CmafAdditionalManifest
mkCmafAdditionalManifest =
  CmafAdditionalManifest'
    { manifestNameModifier = Lude.Nothing,
      selectedOutputs = Lude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
--
-- /Note:/ Consider using 'manifestNameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camManifestNameModifier :: Lens.Lens' CmafAdditionalManifest (Lude.Maybe Lude.Text)
camManifestNameModifier = Lens.lens (manifestNameModifier :: CmafAdditionalManifest -> Lude.Maybe Lude.Text) (\s a -> s {manifestNameModifier = a} :: CmafAdditionalManifest)
{-# DEPRECATED camManifestNameModifier "Use generic-lens or generic-optics with 'manifestNameModifier' instead." #-}

-- | Specify the outputs that you want this additional top-level manifest to reference.
--
-- /Note:/ Consider using 'selectedOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camSelectedOutputs :: Lens.Lens' CmafAdditionalManifest (Lude.Maybe [Lude.Text])
camSelectedOutputs = Lens.lens (selectedOutputs :: CmafAdditionalManifest -> Lude.Maybe [Lude.Text]) (\s a -> s {selectedOutputs = a} :: CmafAdditionalManifest)
{-# DEPRECATED camSelectedOutputs "Use generic-lens or generic-optics with 'selectedOutputs' instead." #-}

instance Lude.FromJSON CmafAdditionalManifest where
  parseJSON =
    Lude.withObject
      "CmafAdditionalManifest"
      ( \x ->
          CmafAdditionalManifest'
            Lude.<$> (x Lude..:? "manifestNameModifier")
            Lude.<*> (x Lude..:? "selectedOutputs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CmafAdditionalManifest where
  toJSON CmafAdditionalManifest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("manifestNameModifier" Lude..=) Lude.<$> manifestNameModifier,
            ("selectedOutputs" Lude..=) Lude.<$> selectedOutputs
          ]
      )
