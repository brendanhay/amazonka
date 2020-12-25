{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
  ( LabelingSetGenerationTaskRunProperties (..),

    -- * Smart constructor
    mkLabelingSetGenerationTaskRunProperties,

    -- * Lenses
    lsgtrpOutputS3Path,
  )
where

import qualified Network.AWS.Glue.Types.OutputS3Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies configuration properties for a labeling set generation task run.
--
-- /See:/ 'mkLabelingSetGenerationTaskRunProperties' smart constructor.
newtype LabelingSetGenerationTaskRunProperties = LabelingSetGenerationTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
    outputS3Path :: Core.Maybe Types.OutputS3Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingSetGenerationTaskRunProperties' value with any optional fields omitted.
mkLabelingSetGenerationTaskRunProperties ::
  LabelingSetGenerationTaskRunProperties
mkLabelingSetGenerationTaskRunProperties =
  LabelingSetGenerationTaskRunProperties'
    { outputS3Path =
        Core.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsgtrpOutputS3Path :: Lens.Lens' LabelingSetGenerationTaskRunProperties (Core.Maybe Types.OutputS3Path)
lsgtrpOutputS3Path = Lens.field @"outputS3Path"
{-# DEPRECATED lsgtrpOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Core.FromJSON LabelingSetGenerationTaskRunProperties where
  parseJSON =
    Core.withObject "LabelingSetGenerationTaskRunProperties" Core.$
      \x ->
        LabelingSetGenerationTaskRunProperties'
          Core.<$> (x Core..:? "OutputS3Path")
