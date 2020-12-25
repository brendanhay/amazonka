{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Asset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Asset
  ( Asset (..),

    -- * Smart constructor
    mkAsset,

    -- * Lenses
    aGroundTruthManifest,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.GroundTruthManifest as Types

-- | Assets are the images that you use to train and evaluate a model version. Assets can also contain validation information that you use to debug a failed model training.
--
-- /See:/ 'mkAsset' smart constructor.
newtype Asset = Asset'
  { groundTruthManifest :: Core.Maybe Types.GroundTruthManifest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Asset' value with any optional fields omitted.
mkAsset ::
  Asset
mkAsset = Asset' {groundTruthManifest = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'groundTruthManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aGroundTruthManifest :: Lens.Lens' Asset (Core.Maybe Types.GroundTruthManifest)
aGroundTruthManifest = Lens.field @"groundTruthManifest"
{-# DEPRECATED aGroundTruthManifest "Use generic-lens or generic-optics with 'groundTruthManifest' instead." #-}

instance Core.FromJSON Asset where
  toJSON Asset {..} =
    Core.object
      ( Core.catMaybes
          [("GroundTruthManifest" Core..=) Core.<$> groundTruthManifest]
      )

instance Core.FromJSON Asset where
  parseJSON =
    Core.withObject "Asset" Core.$
      \x -> Asset' Core.<$> (x Core..:? "GroundTruthManifest")
