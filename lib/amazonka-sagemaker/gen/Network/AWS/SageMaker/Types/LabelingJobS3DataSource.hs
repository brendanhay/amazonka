{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelingJobS3DataSource
  ( LabelingJobS3DataSource (..)
  -- * Smart constructor
  , mkLabelingJobS3DataSource
  -- * Lenses
  , ljsdsManifestS3Uri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ManifestS3Uri as Types

-- | The Amazon S3 location of the input data objects.
--
-- /See:/ 'mkLabelingJobS3DataSource' smart constructor.
newtype LabelingJobS3DataSource = LabelingJobS3DataSource'
  { manifestS3Uri :: Types.ManifestS3Uri
    -- ^ The Amazon S3 location of the manifest file that describes the input data objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobS3DataSource' value with any optional fields omitted.
mkLabelingJobS3DataSource
    :: Types.ManifestS3Uri -- ^ 'manifestS3Uri'
    -> LabelingJobS3DataSource
mkLabelingJobS3DataSource manifestS3Uri
  = LabelingJobS3DataSource'{manifestS3Uri}

-- | The Amazon S3 location of the manifest file that describes the input data objects.
--
-- /Note:/ Consider using 'manifestS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsdsManifestS3Uri :: Lens.Lens' LabelingJobS3DataSource Types.ManifestS3Uri
ljsdsManifestS3Uri = Lens.field @"manifestS3Uri"
{-# INLINEABLE ljsdsManifestS3Uri #-}
{-# DEPRECATED manifestS3Uri "Use generic-lens or generic-optics with 'manifestS3Uri' instead"  #-}

instance Core.FromJSON LabelingJobS3DataSource where
        toJSON LabelingJobS3DataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ManifestS3Uri" Core..= manifestS3Uri)])

instance Core.FromJSON LabelingJobS3DataSource where
        parseJSON
          = Core.withObject "LabelingJobS3DataSource" Core.$
              \ x ->
                LabelingJobS3DataSource' Core.<$> (x Core..: "ManifestS3Uri")
