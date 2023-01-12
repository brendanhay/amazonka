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
-- Module      : Amazonka.SageMaker.Types.LabelingJobSnsDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobSnsDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /See:/ 'newLabelingJobSnsDataSource' smart constructor.
data LabelingJobSnsDataSource = LabelingJobSnsDataSource'
  { -- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN
    -- of the input topic you will use to send new data objects to a streaming
    -- labeling job.
    snsTopicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
labelingJobSnsDataSource_snsTopicArn :: Lens.Lens' LabelingJobSnsDataSource Prelude.Text
labelingJobSnsDataSource_snsTopicArn = Lens.lens (\LabelingJobSnsDataSource' {snsTopicArn} -> snsTopicArn) (\s@LabelingJobSnsDataSource' {} a -> s {snsTopicArn = a} :: LabelingJobSnsDataSource)

instance Data.FromJSON LabelingJobSnsDataSource where
  parseJSON =
    Data.withObject
      "LabelingJobSnsDataSource"
      ( \x ->
          LabelingJobSnsDataSource'
            Prelude.<$> (x Data..: "SnsTopicArn")
      )

instance Prelude.Hashable LabelingJobSnsDataSource where
  hashWithSalt _salt LabelingJobSnsDataSource' {..} =
    _salt `Prelude.hashWithSalt` snsTopicArn

instance Prelude.NFData LabelingJobSnsDataSource where
  rnf LabelingJobSnsDataSource' {..} =
    Prelude.rnf snsTopicArn

instance Data.ToJSON LabelingJobSnsDataSource where
  toJSON LabelingJobSnsDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SnsTopicArn" Data..= snsTopicArn)]
      )
