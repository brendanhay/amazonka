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
    amliS3URI,
    amliAttributeNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An augmented manifest file that provides training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- /See:/ 'mkAugmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { s3URI ::
      Lude.Text,
    attributeNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AugmentedManifestsListItem' with the minimum fields required to make a request.
--
-- * 'attributeNames' - The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth.
-- If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
-- * 's3URI' - The Amazon S3 location of the augmented manifest file.
mkAugmentedManifestsListItem ::
  -- | 's3URI'
  Lude.Text ->
  AugmentedManifestsListItem
mkAugmentedManifestsListItem pS3URI_ =
  AugmentedManifestsListItem'
    { s3URI = pS3URI_,
      attributeNames = Lude.mempty
    }

-- | The Amazon S3 location of the augmented manifest file.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amliS3URI :: Lens.Lens' AugmentedManifestsListItem Lude.Text
amliS3URI = Lens.lens (s3URI :: AugmentedManifestsListItem -> Lude.Text) (\s a -> s {s3URI = a} :: AugmentedManifestsListItem)
{-# DEPRECATED amliS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

-- | The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth.
-- If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amliAttributeNames :: Lens.Lens' AugmentedManifestsListItem [Lude.Text]
amliAttributeNames = Lens.lens (attributeNames :: AugmentedManifestsListItem -> [Lude.Text]) (\s a -> s {attributeNames = a} :: AugmentedManifestsListItem)
{-# DEPRECATED amliAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.FromJSON AugmentedManifestsListItem where
  parseJSON =
    Lude.withObject
      "AugmentedManifestsListItem"
      ( \x ->
          AugmentedManifestsListItem'
            Lude.<$> (x Lude..: "S3Uri")
            Lude.<*> (x Lude..:? "AttributeNames" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3Uri" Lude..= s3URI),
            Lude.Just ("AttributeNames" Lude..= attributeNames)
          ]
      )
