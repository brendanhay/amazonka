-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
  ( MsSmoothAdditionalManifest (..),

    -- * Smart constructor
    mkMsSmoothAdditionalManifest,

    -- * Lenses
    msamManifestNameModifier,
    msamSelectedOutputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specify the details for each additional Microsoft Smooth Streaming manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'mkMsSmoothAdditionalManifest' smart constructor.
data MsSmoothAdditionalManifest = MsSmoothAdditionalManifest'
  { manifestNameModifier ::
      Lude.Maybe Lude.Text,
    selectedOutputs ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MsSmoothAdditionalManifest' with the minimum fields required to make a request.
--
-- * 'manifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
-- * 'selectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
mkMsSmoothAdditionalManifest ::
  MsSmoothAdditionalManifest
mkMsSmoothAdditionalManifest =
  MsSmoothAdditionalManifest'
    { manifestNameModifier = Lude.Nothing,
      selectedOutputs = Lude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
--
-- /Note:/ Consider using 'manifestNameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msamManifestNameModifier :: Lens.Lens' MsSmoothAdditionalManifest (Lude.Maybe Lude.Text)
msamManifestNameModifier = Lens.lens (manifestNameModifier :: MsSmoothAdditionalManifest -> Lude.Maybe Lude.Text) (\s a -> s {manifestNameModifier = a} :: MsSmoothAdditionalManifest)
{-# DEPRECATED msamManifestNameModifier "Use generic-lens or generic-optics with 'manifestNameModifier' instead." #-}

-- | Specify the outputs that you want this additional top-level manifest to reference.
--
-- /Note:/ Consider using 'selectedOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msamSelectedOutputs :: Lens.Lens' MsSmoothAdditionalManifest (Lude.Maybe [Lude.Text])
msamSelectedOutputs = Lens.lens (selectedOutputs :: MsSmoothAdditionalManifest -> Lude.Maybe [Lude.Text]) (\s a -> s {selectedOutputs = a} :: MsSmoothAdditionalManifest)
{-# DEPRECATED msamSelectedOutputs "Use generic-lens or generic-optics with 'selectedOutputs' instead." #-}

instance Lude.FromJSON MsSmoothAdditionalManifest where
  parseJSON =
    Lude.withObject
      "MsSmoothAdditionalManifest"
      ( \x ->
          MsSmoothAdditionalManifest'
            Lude.<$> (x Lude..:? "manifestNameModifier")
            Lude.<*> (x Lude..:? "selectedOutputs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON MsSmoothAdditionalManifest where
  toJSON MsSmoothAdditionalManifest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("manifestNameModifier" Lude..=) Lude.<$> manifestNameModifier,
            ("selectedOutputs" Lude..=) Lude.<$> selectedOutputs
          ]
      )
