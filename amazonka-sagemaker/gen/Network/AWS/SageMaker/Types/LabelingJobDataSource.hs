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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
import Network.AWS.SageMaker.Types.LabelingJobSnsDataSource

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
  { -- | An Amazon SNS data source used for streaming labeling jobs.
    snsDataSource :: Prelude.Maybe LabelingJobSnsDataSource,
    -- | The Amazon S3 location of the input data objects.
    s3DataSource :: Prelude.Maybe LabelingJobS3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsDataSource', 'labelingJobDataSource_snsDataSource' - An Amazon SNS data source used for streaming labeling jobs.
--
-- 's3DataSource', 'labelingJobDataSource_s3DataSource' - The Amazon S3 location of the input data objects.
newLabelingJobDataSource ::
  LabelingJobDataSource
newLabelingJobDataSource =
  LabelingJobDataSource'
    { snsDataSource =
        Prelude.Nothing,
      s3DataSource = Prelude.Nothing
    }

-- | An Amazon SNS data source used for streaming labeling jobs.
labelingJobDataSource_snsDataSource :: Lens.Lens' LabelingJobDataSource (Prelude.Maybe LabelingJobSnsDataSource)
labelingJobDataSource_snsDataSource = Lens.lens (\LabelingJobDataSource' {snsDataSource} -> snsDataSource) (\s@LabelingJobDataSource' {} a -> s {snsDataSource = a} :: LabelingJobDataSource)

-- | The Amazon S3 location of the input data objects.
labelingJobDataSource_s3DataSource :: Lens.Lens' LabelingJobDataSource (Prelude.Maybe LabelingJobS3DataSource)
labelingJobDataSource_s3DataSource = Lens.lens (\LabelingJobDataSource' {s3DataSource} -> s3DataSource) (\s@LabelingJobDataSource' {} a -> s {s3DataSource = a} :: LabelingJobDataSource)

instance Prelude.FromJSON LabelingJobDataSource where
  parseJSON =
    Prelude.withObject
      "LabelingJobDataSource"
      ( \x ->
          LabelingJobDataSource'
            Prelude.<$> (x Prelude..:? "SnsDataSource")
            Prelude.<*> (x Prelude..:? "S3DataSource")
      )

instance Prelude.Hashable LabelingJobDataSource

instance Prelude.NFData LabelingJobDataSource

instance Prelude.ToJSON LabelingJobDataSource where
  toJSON LabelingJobDataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SnsDataSource" Prelude..=)
              Prelude.<$> snsDataSource,
            ("S3DataSource" Prelude..=)
              Prelude.<$> s3DataSource
          ]
      )
