{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashAdditionalManifest
  ( DashAdditionalManifest (..),

    -- * Smart constructor
    mkDashAdditionalManifest,

    -- * Lenses
    damManifestNameModifier,
    damSelectedOutputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specify the details for each additional DASH manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'mkDashAdditionalManifest' smart constructor.
data DashAdditionalManifest = DashAdditionalManifest'
  { -- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your DASH group is film-name.mpd. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.mpd.
    manifestNameModifier :: Lude.Maybe Lude.Text,
    -- | Specify the outputs that you want this additional top-level manifest to reference.
    selectedOutputs :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashAdditionalManifest' with the minimum fields required to make a request.
--
-- * 'manifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your DASH group is film-name.mpd. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.mpd.
-- * 'selectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
mkDashAdditionalManifest ::
  DashAdditionalManifest
mkDashAdditionalManifest =
  DashAdditionalManifest'
    { manifestNameModifier = Lude.Nothing,
      selectedOutputs = Lude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your DASH group is film-name.mpd. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.mpd.
--
-- /Note:/ Consider using 'manifestNameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damManifestNameModifier :: Lens.Lens' DashAdditionalManifest (Lude.Maybe Lude.Text)
damManifestNameModifier = Lens.lens (manifestNameModifier :: DashAdditionalManifest -> Lude.Maybe Lude.Text) (\s a -> s {manifestNameModifier = a} :: DashAdditionalManifest)
{-# DEPRECATED damManifestNameModifier "Use generic-lens or generic-optics with 'manifestNameModifier' instead." #-}

-- | Specify the outputs that you want this additional top-level manifest to reference.
--
-- /Note:/ Consider using 'selectedOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damSelectedOutputs :: Lens.Lens' DashAdditionalManifest (Lude.Maybe [Lude.Text])
damSelectedOutputs = Lens.lens (selectedOutputs :: DashAdditionalManifest -> Lude.Maybe [Lude.Text]) (\s a -> s {selectedOutputs = a} :: DashAdditionalManifest)
{-# DEPRECATED damSelectedOutputs "Use generic-lens or generic-optics with 'selectedOutputs' instead." #-}

instance Lude.FromJSON DashAdditionalManifest where
  parseJSON =
    Lude.withObject
      "DashAdditionalManifest"
      ( \x ->
          DashAdditionalManifest'
            Lude.<$> (x Lude..:? "manifestNameModifier")
            Lude.<*> (x Lude..:? "selectedOutputs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DashAdditionalManifest where
  toJSON DashAdditionalManifest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("manifestNameModifier" Lude..=) Lude.<$> manifestNameModifier,
            ("selectedOutputs" Lude..=) Lude.<$> selectedOutputs
          ]
      )
