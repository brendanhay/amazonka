{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsAdditionalManifest
  ( HlsAdditionalManifest (..)
  -- * Smart constructor
  , mkHlsAdditionalManifest
  -- * Lenses
  , hamManifestNameModifier
  , hamSelectedOutputs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specify the details for each additional HLS manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'mkHlsAdditionalManifest' smart constructor.
data HlsAdditionalManifest = HlsAdditionalManifest'
  { manifestNameModifier :: Core.Maybe Core.Text
    -- ^ Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
  , selectedOutputs :: Core.Maybe [Core.Text]
    -- ^ Specify the outputs that you want this additional top-level manifest to reference.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsAdditionalManifest' value with any optional fields omitted.
mkHlsAdditionalManifest
    :: HlsAdditionalManifest
mkHlsAdditionalManifest
  = HlsAdditionalManifest'{manifestNameModifier = Core.Nothing,
                           selectedOutputs = Core.Nothing}

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
--
-- /Note:/ Consider using 'manifestNameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hamManifestNameModifier :: Lens.Lens' HlsAdditionalManifest (Core.Maybe Core.Text)
hamManifestNameModifier = Lens.field @"manifestNameModifier"
{-# INLINEABLE hamManifestNameModifier #-}
{-# DEPRECATED manifestNameModifier "Use generic-lens or generic-optics with 'manifestNameModifier' instead"  #-}

-- | Specify the outputs that you want this additional top-level manifest to reference.
--
-- /Note:/ Consider using 'selectedOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hamSelectedOutputs :: Lens.Lens' HlsAdditionalManifest (Core.Maybe [Core.Text])
hamSelectedOutputs = Lens.field @"selectedOutputs"
{-# INLINEABLE hamSelectedOutputs #-}
{-# DEPRECATED selectedOutputs "Use generic-lens or generic-optics with 'selectedOutputs' instead"  #-}

instance Core.FromJSON HlsAdditionalManifest where
        toJSON HlsAdditionalManifest{..}
          = Core.object
              (Core.catMaybes
                 [("manifestNameModifier" Core..=) Core.<$> manifestNameModifier,
                  ("selectedOutputs" Core..=) Core.<$> selectedOutputs])

instance Core.FromJSON HlsAdditionalManifest where
        parseJSON
          = Core.withObject "HlsAdditionalManifest" Core.$
              \ x ->
                HlsAdditionalManifest' Core.<$>
                  (x Core..:? "manifestNameModifier") Core.<*>
                    x Core..:? "selectedOutputs"
