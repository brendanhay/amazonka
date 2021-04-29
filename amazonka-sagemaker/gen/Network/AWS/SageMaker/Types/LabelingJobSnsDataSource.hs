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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSnsDataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /See:/ 'newLabelingJobSnsDataSource' smart constructor.
data LabelingJobSnsDataSource = LabelingJobSnsDataSource'
  { -- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN
    -- of the input topic you will use to send new data objects to a streaming
    -- labeling job.
    --
    -- If you specify an input topic for @SnsTopicArn@ in @InputConfig@, you
    -- must specify a value for @SnsTopicArn@ in @OutputConfig@.
    snsTopicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobSnsDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsTopicArn', 'labelingJobSnsDataSource_snsTopicArn' - The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN
-- of the input topic you will use to send new data objects to a streaming
-- labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@, you
-- must specify a value for @SnsTopicArn@ in @OutputConfig@.
newLabelingJobSnsDataSource ::
  -- | 'snsTopicArn'
  Prelude.Text ->
  LabelingJobSnsDataSource
newLabelingJobSnsDataSource pSnsTopicArn_ =
  LabelingJobSnsDataSource'
    { snsTopicArn =
        pSnsTopicArn_
    }

-- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN
-- of the input topic you will use to send new data objects to a streaming
-- labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@, you
-- must specify a value for @SnsTopicArn@ in @OutputConfig@.
labelingJobSnsDataSource_snsTopicArn :: Lens.Lens' LabelingJobSnsDataSource Prelude.Text
labelingJobSnsDataSource_snsTopicArn = Lens.lens (\LabelingJobSnsDataSource' {snsTopicArn} -> snsTopicArn) (\s@LabelingJobSnsDataSource' {} a -> s {snsTopicArn = a} :: LabelingJobSnsDataSource)

instance Prelude.FromJSON LabelingJobSnsDataSource where
  parseJSON =
    Prelude.withObject
      "LabelingJobSnsDataSource"
      ( \x ->
          LabelingJobSnsDataSource'
            Prelude.<$> (x Prelude..: "SnsTopicArn")
      )

instance Prelude.Hashable LabelingJobSnsDataSource

instance Prelude.NFData LabelingJobSnsDataSource

instance Prelude.ToJSON LabelingJobSnsDataSource where
  toJSON LabelingJobSnsDataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SnsTopicArn" Prelude..= snsTopicArn)
          ]
      )
