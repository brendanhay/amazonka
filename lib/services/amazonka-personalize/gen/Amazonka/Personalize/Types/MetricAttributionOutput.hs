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
-- Module      : Amazonka.Personalize.Types.MetricAttributionOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.MetricAttributionOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.S3DataConfig
import qualified Amazonka.Prelude as Prelude

-- | The output configuration details for a metric attribution.
--
-- /See:/ 'newMetricAttributionOutput' smart constructor.
data MetricAttributionOutput = MetricAttributionOutput'
  { s3DataDestination :: Prelude.Maybe S3DataConfig,
    -- | The Amazon Resource Name (ARN) of the IAM service role that has
    -- permissions to add data to your output Amazon S3 bucket and add metrics
    -- to Amazon CloudWatch. For more information, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAttributionOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataDestination', 'metricAttributionOutput_s3DataDestination' - Undocumented member.
--
-- 'roleArn', 'metricAttributionOutput_roleArn' - The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket and add metrics
-- to Amazon CloudWatch. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
newMetricAttributionOutput ::
  -- | 'roleArn'
  Prelude.Text ->
  MetricAttributionOutput
newMetricAttributionOutput pRoleArn_ =
  MetricAttributionOutput'
    { s3DataDestination =
        Prelude.Nothing,
      roleArn = pRoleArn_
    }

-- | Undocumented member.
metricAttributionOutput_s3DataDestination :: Lens.Lens' MetricAttributionOutput (Prelude.Maybe S3DataConfig)
metricAttributionOutput_s3DataDestination = Lens.lens (\MetricAttributionOutput' {s3DataDestination} -> s3DataDestination) (\s@MetricAttributionOutput' {} a -> s {s3DataDestination = a} :: MetricAttributionOutput)

-- | The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket and add metrics
-- to Amazon CloudWatch. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
metricAttributionOutput_roleArn :: Lens.Lens' MetricAttributionOutput Prelude.Text
metricAttributionOutput_roleArn = Lens.lens (\MetricAttributionOutput' {roleArn} -> roleArn) (\s@MetricAttributionOutput' {} a -> s {roleArn = a} :: MetricAttributionOutput)

instance Data.FromJSON MetricAttributionOutput where
  parseJSON =
    Data.withObject
      "MetricAttributionOutput"
      ( \x ->
          MetricAttributionOutput'
            Prelude.<$> (x Data..:? "s3DataDestination")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable MetricAttributionOutput where
  hashWithSalt _salt MetricAttributionOutput' {..} =
    _salt `Prelude.hashWithSalt` s3DataDestination
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData MetricAttributionOutput where
  rnf MetricAttributionOutput' {..} =
    Prelude.rnf s3DataDestination
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON MetricAttributionOutput where
  toJSON MetricAttributionOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3DataDestination" Data..=)
              Prelude.<$> s3DataDestination,
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
