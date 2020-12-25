{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataAttributes
  ( LabelingJobDataAttributes (..),

    -- * Smart constructor
    mkLabelingJobDataAttributes,

    -- * Lenses
    ljdaContentClassifiers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContentClassifier as Types

-- | Attributes of the data specified by the customer. Use these to describe the data to be labeled.
--
-- /See:/ 'mkLabelingJobDataAttributes' smart constructor.
newtype LabelingJobDataAttributes = LabelingJobDataAttributes'
  { -- | Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
    contentClassifiers :: Core.Maybe [Types.ContentClassifier]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobDataAttributes' value with any optional fields omitted.
mkLabelingJobDataAttributes ::
  LabelingJobDataAttributes
mkLabelingJobDataAttributes =
  LabelingJobDataAttributes' {contentClassifiers = Core.Nothing}

-- | Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
--
-- /Note:/ Consider using 'contentClassifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdaContentClassifiers :: Lens.Lens' LabelingJobDataAttributes (Core.Maybe [Types.ContentClassifier])
ljdaContentClassifiers = Lens.field @"contentClassifiers"
{-# DEPRECATED ljdaContentClassifiers "Use generic-lens or generic-optics with 'contentClassifiers' instead." #-}

instance Core.FromJSON LabelingJobDataAttributes where
  toJSON LabelingJobDataAttributes {..} =
    Core.object
      ( Core.catMaybes
          [("ContentClassifiers" Core..=) Core.<$> contentClassifiers]
      )

instance Core.FromJSON LabelingJobDataAttributes where
  parseJSON =
    Core.withObject "LabelingJobDataAttributes" Core.$
      \x ->
        LabelingJobDataAttributes'
          Core.<$> (x Core..:? "ContentClassifiers")
