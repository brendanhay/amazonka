{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An augmented manifest file that provides training data for your custom
-- model. An augmented manifest file is a labeled dataset that is produced
-- by Amazon SageMaker Ground Truth.
--
-- /See:/ 'newAugmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { -- | The Amazon S3 location of the augmented manifest file.
    s3Uri :: Prelude.Text,
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
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AugmentedManifestsListItem
newAugmentedManifestsListItem pS3Uri_ =
  AugmentedManifestsListItem'
    { s3Uri = pS3Uri_,
      attributeNames = Prelude.mempty
    }

-- | The Amazon S3 location of the augmented manifest file.
augmentedManifestsListItem_s3Uri :: Lens.Lens' AugmentedManifestsListItem Prelude.Text
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
augmentedManifestsListItem_attributeNames :: Lens.Lens' AugmentedManifestsListItem [Prelude.Text]
augmentedManifestsListItem_attributeNames = Lens.lens (\AugmentedManifestsListItem' {attributeNames} -> attributeNames) (\s@AugmentedManifestsListItem' {} a -> s {attributeNames = a} :: AugmentedManifestsListItem) Prelude.. Prelude._Coerce

instance Prelude.FromJSON AugmentedManifestsListItem where
  parseJSON =
    Prelude.withObject
      "AugmentedManifestsListItem"
      ( \x ->
          AugmentedManifestsListItem'
            Prelude.<$> (x Prelude..: "S3Uri")
            Prelude.<*> ( x Prelude..:? "AttributeNames"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AugmentedManifestsListItem

instance Prelude.NFData AugmentedManifestsListItem

instance Prelude.ToJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just
              ("AttributeNames" Prelude..= attributeNames)
          ]
      )
