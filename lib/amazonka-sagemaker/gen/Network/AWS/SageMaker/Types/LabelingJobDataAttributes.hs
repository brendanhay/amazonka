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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ContentClassifier

-- | Attributes of the data specified by the customer. Use these to describe the data to be labeled.
--
-- /See:/ 'mkLabelingJobDataAttributes' smart constructor.
newtype LabelingJobDataAttributes = LabelingJobDataAttributes'
  { contentClassifiers ::
      Lude.Maybe [ContentClassifier]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobDataAttributes' with the minimum fields required to make a request.
--
-- * 'contentClassifiers' - Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
mkLabelingJobDataAttributes ::
  LabelingJobDataAttributes
mkLabelingJobDataAttributes =
  LabelingJobDataAttributes' {contentClassifiers = Lude.Nothing}

-- | Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
--
-- /Note:/ Consider using 'contentClassifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdaContentClassifiers :: Lens.Lens' LabelingJobDataAttributes (Lude.Maybe [ContentClassifier])
ljdaContentClassifiers = Lens.lens (contentClassifiers :: LabelingJobDataAttributes -> Lude.Maybe [ContentClassifier]) (\s a -> s {contentClassifiers = a} :: LabelingJobDataAttributes)
{-# DEPRECATED ljdaContentClassifiers "Use generic-lens or generic-optics with 'contentClassifiers' instead." #-}

instance Lude.FromJSON LabelingJobDataAttributes where
  parseJSON =
    Lude.withObject
      "LabelingJobDataAttributes"
      ( \x ->
          LabelingJobDataAttributes'
            Lude.<$> (x Lude..:? "ContentClassifiers" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON LabelingJobDataAttributes where
  toJSON LabelingJobDataAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ContentClassifiers" Lude..=) Lude.<$> contentClassifiers]
      )
