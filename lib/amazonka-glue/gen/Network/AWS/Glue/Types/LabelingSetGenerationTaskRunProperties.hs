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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration properties for a labeling set generation task run.
--
-- /See:/ 'mkLabelingSetGenerationTaskRunProperties' smart constructor.
newtype LabelingSetGenerationTaskRunProperties = LabelingSetGenerationTaskRunProperties'
  { outputS3Path ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingSetGenerationTaskRunProperties' with the minimum fields required to make a request.
--
-- * 'outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
mkLabelingSetGenerationTaskRunProperties ::
  LabelingSetGenerationTaskRunProperties
mkLabelingSetGenerationTaskRunProperties =
  LabelingSetGenerationTaskRunProperties'
    { outputS3Path =
        Lude.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsgtrpOutputS3Path :: Lens.Lens' LabelingSetGenerationTaskRunProperties (Lude.Maybe Lude.Text)
lsgtrpOutputS3Path = Lens.lens (outputS3Path :: LabelingSetGenerationTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Path = a} :: LabelingSetGenerationTaskRunProperties)
{-# DEPRECATED lsgtrpOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Lude.FromJSON LabelingSetGenerationTaskRunProperties where
  parseJSON =
    Lude.withObject
      "LabelingSetGenerationTaskRunProperties"
      ( \x ->
          LabelingSetGenerationTaskRunProperties'
            Lude.<$> (x Lude..:? "OutputS3Path")
      )
