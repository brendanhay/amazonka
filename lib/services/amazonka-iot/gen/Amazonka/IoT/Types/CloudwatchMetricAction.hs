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
-- Module      : Amazonka.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CloudwatchMetricAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action that captures a CloudWatch metric.
--
-- /See:/ 'newCloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
  { -- | An optional
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
    metricTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that allows access to the CloudWatch metric.
    roleArn :: Prelude.Text,
    -- | The CloudWatch metric namespace name.
    metricNamespace :: Prelude.Text,
    -- | The CloudWatch metric name.
    metricName :: Prelude.Text,
    -- | The CloudWatch metric value.
    metricValue :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
    -- supported by CloudWatch.
    metricUnit :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudwatchMetricAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricTimestamp', 'cloudwatchMetricAction_metricTimestamp' - An optional
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
--
-- 'roleArn', 'cloudwatchMetricAction_roleArn' - The IAM role that allows access to the CloudWatch metric.
--
-- 'metricNamespace', 'cloudwatchMetricAction_metricNamespace' - The CloudWatch metric namespace name.
--
-- 'metricName', 'cloudwatchMetricAction_metricName' - The CloudWatch metric name.
--
-- 'metricValue', 'cloudwatchMetricAction_metricValue' - The CloudWatch metric value.
--
-- 'metricUnit', 'cloudwatchMetricAction_metricUnit' - The
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
-- supported by CloudWatch.
newCloudwatchMetricAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'metricNamespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'metricValue'
  Prelude.Text ->
  -- | 'metricUnit'
  Prelude.Text ->
  CloudwatchMetricAction
newCloudwatchMetricAction
  pRoleArn_
  pMetricNamespace_
  pMetricName_
  pMetricValue_
  pMetricUnit_ =
    CloudwatchMetricAction'
      { metricTimestamp =
          Prelude.Nothing,
        roleArn = pRoleArn_,
        metricNamespace = pMetricNamespace_,
        metricName = pMetricName_,
        metricValue = pMetricValue_,
        metricUnit = pMetricUnit_
      }

-- | An optional
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
cloudwatchMetricAction_metricTimestamp :: Lens.Lens' CloudwatchMetricAction (Prelude.Maybe Prelude.Text)
cloudwatchMetricAction_metricTimestamp = Lens.lens (\CloudwatchMetricAction' {metricTimestamp} -> metricTimestamp) (\s@CloudwatchMetricAction' {} a -> s {metricTimestamp = a} :: CloudwatchMetricAction)

-- | The IAM role that allows access to the CloudWatch metric.
cloudwatchMetricAction_roleArn :: Lens.Lens' CloudwatchMetricAction Prelude.Text
cloudwatchMetricAction_roleArn = Lens.lens (\CloudwatchMetricAction' {roleArn} -> roleArn) (\s@CloudwatchMetricAction' {} a -> s {roleArn = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric namespace name.
cloudwatchMetricAction_metricNamespace :: Lens.Lens' CloudwatchMetricAction Prelude.Text
cloudwatchMetricAction_metricNamespace = Lens.lens (\CloudwatchMetricAction' {metricNamespace} -> metricNamespace) (\s@CloudwatchMetricAction' {} a -> s {metricNamespace = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric name.
cloudwatchMetricAction_metricName :: Lens.Lens' CloudwatchMetricAction Prelude.Text
cloudwatchMetricAction_metricName = Lens.lens (\CloudwatchMetricAction' {metricName} -> metricName) (\s@CloudwatchMetricAction' {} a -> s {metricName = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric value.
cloudwatchMetricAction_metricValue :: Lens.Lens' CloudwatchMetricAction Prelude.Text
cloudwatchMetricAction_metricValue = Lens.lens (\CloudwatchMetricAction' {metricValue} -> metricValue) (\s@CloudwatchMetricAction' {} a -> s {metricValue = a} :: CloudwatchMetricAction)

-- | The
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
-- supported by CloudWatch.
cloudwatchMetricAction_metricUnit :: Lens.Lens' CloudwatchMetricAction Prelude.Text
cloudwatchMetricAction_metricUnit = Lens.lens (\CloudwatchMetricAction' {metricUnit} -> metricUnit) (\s@CloudwatchMetricAction' {} a -> s {metricUnit = a} :: CloudwatchMetricAction)

instance Data.FromJSON CloudwatchMetricAction where
  parseJSON =
    Data.withObject
      "CloudwatchMetricAction"
      ( \x ->
          CloudwatchMetricAction'
            Prelude.<$> (x Data..:? "metricTimestamp")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "metricNamespace")
            Prelude.<*> (x Data..: "metricName")
            Prelude.<*> (x Data..: "metricValue")
            Prelude.<*> (x Data..: "metricUnit")
      )

instance Prelude.Hashable CloudwatchMetricAction where
  hashWithSalt _salt CloudwatchMetricAction' {..} =
    _salt `Prelude.hashWithSalt` metricTimestamp
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` metricNamespace
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metricValue
      `Prelude.hashWithSalt` metricUnit

instance Prelude.NFData CloudwatchMetricAction where
  rnf CloudwatchMetricAction' {..} =
    Prelude.rnf metricTimestamp
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf metricNamespace
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf metricValue
      `Prelude.seq` Prelude.rnf metricUnit

instance Data.ToJSON CloudwatchMetricAction where
  toJSON CloudwatchMetricAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metricTimestamp" Data..=)
              Prelude.<$> metricTimestamp,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just
              ("metricNamespace" Data..= metricNamespace),
            Prelude.Just ("metricName" Data..= metricName),
            Prelude.Just ("metricValue" Data..= metricValue),
            Prelude.Just ("metricUnit" Data..= metricUnit)
          ]
      )
