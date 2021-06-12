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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobS3DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobS3DataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon S3 location of the input data objects.
--
-- /See:/ 'newLabelingJobS3DataSource' smart constructor.
data LabelingJobS3DataSource = LabelingJobS3DataSource'
  { -- | The Amazon S3 location of the manifest file that describes the input
    -- data objects.
    --
    -- The input manifest file referenced in @ManifestS3Uri@ must contain one
    -- of the following keys: @source-ref@ or @source@. The value of the keys
    -- are interpreted as follows:
    --
    -- -   @source-ref@: The source of the object is the Amazon S3 object
    --     specified in the value. Use this value when the object is a binary
    --     object, such as an image.
    --
    -- -   @source@: The source of the object is the value. Use this value when
    --     the object is a text value.
    --
    -- If you are a new user of Ground Truth, it is recommended you review
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-input-data-input-manifest.html Use an Input Manifest File>
    -- in the Amazon SageMaker Developer Guide to learn how to create an input
    -- manifest file.
    manifestS3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LabelingJobS3DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestS3Uri', 'labelingJobS3DataSource_manifestS3Uri' - The Amazon S3 location of the manifest file that describes the input
-- data objects.
--
-- The input manifest file referenced in @ManifestS3Uri@ must contain one
-- of the following keys: @source-ref@ or @source@. The value of the keys
-- are interpreted as follows:
--
-- -   @source-ref@: The source of the object is the Amazon S3 object
--     specified in the value. Use this value when the object is a binary
--     object, such as an image.
--
-- -   @source@: The source of the object is the value. Use this value when
--     the object is a text value.
--
-- If you are a new user of Ground Truth, it is recommended you review
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-input-data-input-manifest.html Use an Input Manifest File>
-- in the Amazon SageMaker Developer Guide to learn how to create an input
-- manifest file.
newLabelingJobS3DataSource ::
  -- | 'manifestS3Uri'
  Core.Text ->
  LabelingJobS3DataSource
newLabelingJobS3DataSource pManifestS3Uri_ =
  LabelingJobS3DataSource'
    { manifestS3Uri =
        pManifestS3Uri_
    }

-- | The Amazon S3 location of the manifest file that describes the input
-- data objects.
--
-- The input manifest file referenced in @ManifestS3Uri@ must contain one
-- of the following keys: @source-ref@ or @source@. The value of the keys
-- are interpreted as follows:
--
-- -   @source-ref@: The source of the object is the Amazon S3 object
--     specified in the value. Use this value when the object is a binary
--     object, such as an image.
--
-- -   @source@: The source of the object is the value. Use this value when
--     the object is a text value.
--
-- If you are a new user of Ground Truth, it is recommended you review
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-input-data-input-manifest.html Use an Input Manifest File>
-- in the Amazon SageMaker Developer Guide to learn how to create an input
-- manifest file.
labelingJobS3DataSource_manifestS3Uri :: Lens.Lens' LabelingJobS3DataSource Core.Text
labelingJobS3DataSource_manifestS3Uri = Lens.lens (\LabelingJobS3DataSource' {manifestS3Uri} -> manifestS3Uri) (\s@LabelingJobS3DataSource' {} a -> s {manifestS3Uri = a} :: LabelingJobS3DataSource)

instance Core.FromJSON LabelingJobS3DataSource where
  parseJSON =
    Core.withObject
      "LabelingJobS3DataSource"
      ( \x ->
          LabelingJobS3DataSource'
            Core.<$> (x Core..: "ManifestS3Uri")
      )

instance Core.Hashable LabelingJobS3DataSource

instance Core.NFData LabelingJobS3DataSource

instance Core.ToJSON LabelingJobS3DataSource where
  toJSON LabelingJobS3DataSource' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ManifestS3Uri" Core..= manifestS3Uri)]
      )
