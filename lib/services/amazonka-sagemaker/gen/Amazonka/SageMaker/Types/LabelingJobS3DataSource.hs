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
-- Module      : Amazonka.SageMaker.Types.LabelingJobS3DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobS3DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    manifestS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
labelingJobS3DataSource_manifestS3Uri :: Lens.Lens' LabelingJobS3DataSource Prelude.Text
labelingJobS3DataSource_manifestS3Uri = Lens.lens (\LabelingJobS3DataSource' {manifestS3Uri} -> manifestS3Uri) (\s@LabelingJobS3DataSource' {} a -> s {manifestS3Uri = a} :: LabelingJobS3DataSource)

instance Core.FromJSON LabelingJobS3DataSource where
  parseJSON =
    Core.withObject
      "LabelingJobS3DataSource"
      ( \x ->
          LabelingJobS3DataSource'
            Prelude.<$> (x Core..: "ManifestS3Uri")
      )

instance Prelude.Hashable LabelingJobS3DataSource where
  hashWithSalt _salt LabelingJobS3DataSource' {..} =
    _salt `Prelude.hashWithSalt` manifestS3Uri

instance Prelude.NFData LabelingJobS3DataSource where
  rnf LabelingJobS3DataSource' {..} =
    Prelude.rnf manifestS3Uri

instance Core.ToJSON LabelingJobS3DataSource where
  toJSON LabelingJobS3DataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ManifestS3Uri" Core..= manifestS3Uri)
          ]
      )
