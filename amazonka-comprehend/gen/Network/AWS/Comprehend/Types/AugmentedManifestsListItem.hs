{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.AugmentedManifestsListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.AugmentedManifestsListItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An augmented manifest file that provides training data for your custom
-- model. An augmented manifest file is a labeled dataset that is produced
-- by Amazon SageMaker Ground Truth.
--
-- /See:/ 'newAugmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { -- | The Amazon S3 location of the augmented manifest file.
    s3Uri :: Core.Text,
    -- | The JSON attribute that contains the annotations for your training
    -- documents. The number of attribute names that you specify depends on
    -- whether your augmented manifest file is the output of a single labeling
    -- job or a chained labeling job.
    --
    -- If your file is the output of a single labeling job, specify the
    -- LabelAttributeName key that was used when the job was created in Ground
    -- Truth.
    --
    -- If your file is the output of a chained labeling job, specify the
    -- LabelAttributeName key for one or more jobs in the chain. Each
    -- LabelAttributeName key provides the annotations from an individual job.
    attributeNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AugmentedManifestsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'augmentedManifestsListItem_s3Uri' - The Amazon S3 location of the augmented manifest file.
--
-- 'attributeNames', 'augmentedManifestsListItem_attributeNames' - The JSON attribute that contains the annotations for your training
-- documents. The number of attribute names that you specify depends on
-- whether your augmented manifest file is the output of a single labeling
-- job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the
-- LabelAttributeName key that was used when the job was created in Ground
-- Truth.
--
-- If your file is the output of a chained labeling job, specify the
-- LabelAttributeName key for one or more jobs in the chain. Each
-- LabelAttributeName key provides the annotations from an individual job.
newAugmentedManifestsListItem ::
  -- | 's3Uri'
  Core.Text ->
  AugmentedManifestsListItem
newAugmentedManifestsListItem pS3Uri_ =
  AugmentedManifestsListItem'
    { s3Uri = pS3Uri_,
      attributeNames = Core.mempty
    }

-- | The Amazon S3 location of the augmented manifest file.
augmentedManifestsListItem_s3Uri :: Lens.Lens' AugmentedManifestsListItem Core.Text
augmentedManifestsListItem_s3Uri = Lens.lens (\AugmentedManifestsListItem' {s3Uri} -> s3Uri) (\s@AugmentedManifestsListItem' {} a -> s {s3Uri = a} :: AugmentedManifestsListItem)

-- | The JSON attribute that contains the annotations for your training
-- documents. The number of attribute names that you specify depends on
-- whether your augmented manifest file is the output of a single labeling
-- job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the
-- LabelAttributeName key that was used when the job was created in Ground
-- Truth.
--
-- If your file is the output of a chained labeling job, specify the
-- LabelAttributeName key for one or more jobs in the chain. Each
-- LabelAttributeName key provides the annotations from an individual job.
augmentedManifestsListItem_attributeNames :: Lens.Lens' AugmentedManifestsListItem [Core.Text]
augmentedManifestsListItem_attributeNames = Lens.lens (\AugmentedManifestsListItem' {attributeNames} -> attributeNames) (\s@AugmentedManifestsListItem' {} a -> s {attributeNames = a} :: AugmentedManifestsListItem) Core.. Lens._Coerce

instance Core.FromJSON AugmentedManifestsListItem where
  parseJSON =
    Core.withObject
      "AugmentedManifestsListItem"
      ( \x ->
          AugmentedManifestsListItem'
            Core.<$> (x Core..: "S3Uri")
            Core.<*> (x Core..:? "AttributeNames" Core..!= Core.mempty)
      )

instance Core.Hashable AugmentedManifestsListItem

instance Core.NFData AugmentedManifestsListItem

instance Core.ToJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3Uri" Core..= s3Uri),
            Core.Just ("AttributeNames" Core..= attributeNames)
          ]
      )
