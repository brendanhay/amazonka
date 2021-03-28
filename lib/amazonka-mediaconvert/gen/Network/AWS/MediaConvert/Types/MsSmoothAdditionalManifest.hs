{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
  ( MsSmoothAdditionalManifest (..)
  -- * Smart constructor
  , mkMsSmoothAdditionalManifest
  -- * Lenses
  , msamManifestNameModifier
  , msamSelectedOutputs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specify the details for each additional Microsoft Smooth Streaming manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'mkMsSmoothAdditionalManifest' smart constructor.
data MsSmoothAdditionalManifest = MsSmoothAdditionalManifest'
  { manifestNameModifier :: Core.Maybe Core.Text
    -- ^ Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
  , selectedOutputs :: Core.Maybe [Core.Text]
    -- ^ Specify the outputs that you want this additional top-level manifest to reference.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MsSmoothAdditionalManifest' value with any optional fields omitted.
mkMsSmoothAdditionalManifest
    :: MsSmoothAdditionalManifest
mkMsSmoothAdditionalManifest
  = MsSmoothAdditionalManifest'{manifestNameModifier = Core.Nothing,
                                selectedOutputs = Core.Nothing}

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
--
-- /Note:/ Consider using 'manifestNameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msamManifestNameModifier :: Lens.Lens' MsSmoothAdditionalManifest (Core.Maybe Core.Text)
msamManifestNameModifier = Lens.field @"manifestNameModifier"
{-# INLINEABLE msamManifestNameModifier #-}
{-# DEPRECATED manifestNameModifier "Use generic-lens or generic-optics with 'manifestNameModifier' instead"  #-}

-- | Specify the outputs that you want this additional top-level manifest to reference.
--
-- /Note:/ Consider using 'selectedOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msamSelectedOutputs :: Lens.Lens' MsSmoothAdditionalManifest (Core.Maybe [Core.Text])
msamSelectedOutputs = Lens.field @"selectedOutputs"
{-# INLINEABLE msamSelectedOutputs #-}
{-# DEPRECATED selectedOutputs "Use generic-lens or generic-optics with 'selectedOutputs' instead"  #-}

instance Core.FromJSON MsSmoothAdditionalManifest where
        toJSON MsSmoothAdditionalManifest{..}
          = Core.object
              (Core.catMaybes
                 [("manifestNameModifier" Core..=) Core.<$> manifestNameModifier,
                  ("selectedOutputs" Core..=) Core.<$> selectedOutputs])

instance Core.FromJSON MsSmoothAdditionalManifest where
        parseJSON
          = Core.withObject "MsSmoothAdditionalManifest" Core.$
              \ x ->
                MsSmoothAdditionalManifest' Core.<$>
                  (x Core..:? "manifestNameModifier") Core.<*>
                    x Core..:? "selectedOutputs"
