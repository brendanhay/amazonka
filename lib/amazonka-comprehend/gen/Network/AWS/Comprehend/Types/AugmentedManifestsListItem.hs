{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.AugmentedManifestsListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.AugmentedManifestsListItem
  ( AugmentedManifestsListItem (..),

    -- * Smart constructor
    mkAugmentedManifestsListItem,

    -- * Lenses
    amliS3Uri,
    amliAttributeNames,
  )
where

import qualified Network.AWS.Comprehend.Types.AttributeNamesListItem as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An augmented manifest file that provides training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- /See:/ 'mkAugmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { -- | The Amazon S3 location of the augmented manifest file.
    s3Uri :: Types.S3Uri,
    -- | The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job.
    --
    -- If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth.
    -- If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
    attributeNames :: [Types.AttributeNamesListItem]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AugmentedManifestsListItem' value with any optional fields omitted.
mkAugmentedManifestsListItem ::
  -- | 's3Uri'
  Types.S3Uri ->
  AugmentedManifestsListItem
mkAugmentedManifestsListItem s3Uri =
  AugmentedManifestsListItem' {s3Uri, attributeNames = Core.mempty}

-- | The Amazon S3 location of the augmented manifest file.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amliS3Uri :: Lens.Lens' AugmentedManifestsListItem Types.S3Uri
amliS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED amliS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

-- | The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth.
-- If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amliAttributeNames :: Lens.Lens' AugmentedManifestsListItem [Types.AttributeNamesListItem]
amliAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED amliAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Core.FromJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3Uri" Core..= s3Uri),
            Core.Just ("AttributeNames" Core..= attributeNames)
          ]
      )

instance Core.FromJSON AugmentedManifestsListItem where
  parseJSON =
    Core.withObject "AugmentedManifestsListItem" Core.$
      \x ->
        AugmentedManifestsListItem'
          Core.<$> (x Core..: "S3Uri")
          Core.<*> (x Core..:? "AttributeNames" Core..!= Core.mempty)
