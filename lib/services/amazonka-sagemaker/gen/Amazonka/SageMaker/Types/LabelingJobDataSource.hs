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
-- Module      : Amazonka.SageMaker.Types.LabelingJobDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LabelingJobS3DataSource
import Amazonka.SageMaker.Types.LabelingJobSnsDataSource

-- | Provides information about the location of input data.
--
-- You must specify at least one of the following: @S3DataSource@ or
-- @SnsDataSource@.
--
-- Use @SnsDataSource@ to specify an SNS input topic for a streaming
-- labeling job. If you do not specify and SNS input topic ARN, Ground
-- Truth will create a one-time labeling job.
--
-- Use @S3DataSource@ to specify an input manifest file for both streaming
-- and one-time labeling jobs. Adding an @S3DataSource@ is optional if you
-- use @SnsDataSource@ to create a streaming labeling job.
--
-- /See:/ 'newLabelingJobDataSource' smart constructor.
data LabelingJobDataSource = LabelingJobDataSource'
  { -- | The Amazon S3 location of the input data objects.
    s3DataSource :: Prelude.Maybe LabelingJobS3DataSource,
    -- | An Amazon SNS data source used for streaming labeling jobs. To learn
    -- more, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-send-data Send Data to a Streaming Labeling Job>.
    snsDataSource :: Prelude.Maybe LabelingJobSnsDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'labelingJobDataSource_s3DataSource' - The Amazon S3 location of the input data objects.
--
-- 'snsDataSource', 'labelingJobDataSource_snsDataSource' - An Amazon SNS data source used for streaming labeling jobs. To learn
-- more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-send-data Send Data to a Streaming Labeling Job>.
newLabelingJobDataSource ::
  LabelingJobDataSource
newLabelingJobDataSource =
  LabelingJobDataSource'
    { s3DataSource =
        Prelude.Nothing,
      snsDataSource = Prelude.Nothing
    }

-- | The Amazon S3 location of the input data objects.
labelingJobDataSource_s3DataSource :: Lens.Lens' LabelingJobDataSource (Prelude.Maybe LabelingJobS3DataSource)
labelingJobDataSource_s3DataSource = Lens.lens (\LabelingJobDataSource' {s3DataSource} -> s3DataSource) (\s@LabelingJobDataSource' {} a -> s {s3DataSource = a} :: LabelingJobDataSource)

-- | An Amazon SNS data source used for streaming labeling jobs. To learn
-- more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-send-data Send Data to a Streaming Labeling Job>.
labelingJobDataSource_snsDataSource :: Lens.Lens' LabelingJobDataSource (Prelude.Maybe LabelingJobSnsDataSource)
labelingJobDataSource_snsDataSource = Lens.lens (\LabelingJobDataSource' {snsDataSource} -> snsDataSource) (\s@LabelingJobDataSource' {} a -> s {snsDataSource = a} :: LabelingJobDataSource)

instance Data.FromJSON LabelingJobDataSource where
  parseJSON =
    Data.withObject
      "LabelingJobDataSource"
      ( \x ->
          LabelingJobDataSource'
            Prelude.<$> (x Data..:? "S3DataSource")
            Prelude.<*> (x Data..:? "SnsDataSource")
      )

instance Prelude.Hashable LabelingJobDataSource where
  hashWithSalt _salt LabelingJobDataSource' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource
      `Prelude.hashWithSalt` snsDataSource

instance Prelude.NFData LabelingJobDataSource where
  rnf LabelingJobDataSource' {..} =
    Prelude.rnf s3DataSource
      `Prelude.seq` Prelude.rnf snsDataSource

instance Data.ToJSON LabelingJobDataSource where
  toJSON LabelingJobDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3DataSource" Data..=) Prelude.<$> s3DataSource,
            ("SnsDataSource" Data..=) Prelude.<$> snsDataSource
          ]
      )
