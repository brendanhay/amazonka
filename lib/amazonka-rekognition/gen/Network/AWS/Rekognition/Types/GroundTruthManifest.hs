{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GroundTruthManifest
  ( GroundTruthManifest (..),

    -- * Smart constructor
    mkGroundTruthManifest,

    -- * Lenses
    gtmS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.S3Object as Types

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format manifest file.
--
-- /See:/ 'mkGroundTruthManifest' smart constructor.
newtype GroundTruthManifest = GroundTruthManifest'
  { s3Object :: Core.Maybe Types.S3Object
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GroundTruthManifest' value with any optional fields omitted.
mkGroundTruthManifest ::
  GroundTruthManifest
mkGroundTruthManifest =
  GroundTruthManifest' {s3Object = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmS3Object :: Lens.Lens' GroundTruthManifest (Core.Maybe Types.S3Object)
gtmS3Object = Lens.field @"s3Object"
{-# DEPRECATED gtmS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Core.FromJSON GroundTruthManifest where
  toJSON GroundTruthManifest {..} =
    Core.object
      (Core.catMaybes [("S3Object" Core..=) Core.<$> s3Object])

instance Core.FromJSON GroundTruthManifest where
  parseJSON =
    Core.withObject "GroundTruthManifest" Core.$
      \x -> GroundTruthManifest' Core.<$> (x Core..:? "S3Object")
