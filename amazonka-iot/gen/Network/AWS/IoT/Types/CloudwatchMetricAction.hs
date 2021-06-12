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
-- Module      : Network.AWS.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchMetricAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action that captures a CloudWatch metric.
--
-- /See:/ 'newCloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
  { -- | An optional
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
    metricTimestamp :: Core.Maybe Core.Text,
    -- | The IAM role that allows access to the CloudWatch metric.
    roleArn :: Core.Text,
    -- | The CloudWatch metric namespace name.
    metricNamespace :: Core.Text,
    -- | The CloudWatch metric name.
    metricName :: Core.Text,
    -- | The CloudWatch metric value.
    metricValue :: Core.Text,
    -- | The
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
    -- supported by CloudWatch.
    metricUnit :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'metricNamespace'
  Core.Text ->
  -- | 'metricName'
  Core.Text ->
  -- | 'metricValue'
  Core.Text ->
  -- | 'metricUnit'
  Core.Text ->
  CloudwatchMetricAction
newCloudwatchMetricAction
  pRoleArn_
  pMetricNamespace_
  pMetricName_
  pMetricValue_
  pMetricUnit_ =
    CloudwatchMetricAction'
      { metricTimestamp =
          Core.Nothing,
        roleArn = pRoleArn_,
        metricNamespace = pMetricNamespace_,
        metricName = pMetricName_,
        metricValue = pMetricValue_,
        metricUnit = pMetricUnit_
      }

-- | An optional
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp>.
cloudwatchMetricAction_metricTimestamp :: Lens.Lens' CloudwatchMetricAction (Core.Maybe Core.Text)
cloudwatchMetricAction_metricTimestamp = Lens.lens (\CloudwatchMetricAction' {metricTimestamp} -> metricTimestamp) (\s@CloudwatchMetricAction' {} a -> s {metricTimestamp = a} :: CloudwatchMetricAction)

-- | The IAM role that allows access to the CloudWatch metric.
cloudwatchMetricAction_roleArn :: Lens.Lens' CloudwatchMetricAction Core.Text
cloudwatchMetricAction_roleArn = Lens.lens (\CloudwatchMetricAction' {roleArn} -> roleArn) (\s@CloudwatchMetricAction' {} a -> s {roleArn = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric namespace name.
cloudwatchMetricAction_metricNamespace :: Lens.Lens' CloudwatchMetricAction Core.Text
cloudwatchMetricAction_metricNamespace = Lens.lens (\CloudwatchMetricAction' {metricNamespace} -> metricNamespace) (\s@CloudwatchMetricAction' {} a -> s {metricNamespace = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric name.
cloudwatchMetricAction_metricName :: Lens.Lens' CloudwatchMetricAction Core.Text
cloudwatchMetricAction_metricName = Lens.lens (\CloudwatchMetricAction' {metricName} -> metricName) (\s@CloudwatchMetricAction' {} a -> s {metricName = a} :: CloudwatchMetricAction)

-- | The CloudWatch metric value.
cloudwatchMetricAction_metricValue :: Lens.Lens' CloudwatchMetricAction Core.Text
cloudwatchMetricAction_metricValue = Lens.lens (\CloudwatchMetricAction' {metricValue} -> metricValue) (\s@CloudwatchMetricAction' {} a -> s {metricValue = a} :: CloudwatchMetricAction)

-- | The
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit>
-- supported by CloudWatch.
cloudwatchMetricAction_metricUnit :: Lens.Lens' CloudwatchMetricAction Core.Text
cloudwatchMetricAction_metricUnit = Lens.lens (\CloudwatchMetricAction' {metricUnit} -> metricUnit) (\s@CloudwatchMetricAction' {} a -> s {metricUnit = a} :: CloudwatchMetricAction)

instance Core.FromJSON CloudwatchMetricAction where
  parseJSON =
    Core.withObject
      "CloudwatchMetricAction"
      ( \x ->
          CloudwatchMetricAction'
            Core.<$> (x Core..:? "metricTimestamp")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "metricNamespace")
            Core.<*> (x Core..: "metricName")
            Core.<*> (x Core..: "metricValue")
            Core.<*> (x Core..: "metricUnit")
      )

instance Core.Hashable CloudwatchMetricAction

instance Core.NFData CloudwatchMetricAction

instance Core.ToJSON CloudwatchMetricAction where
  toJSON CloudwatchMetricAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("metricTimestamp" Core..=)
              Core.<$> metricTimestamp,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just
              ("metricNamespace" Core..= metricNamespace),
            Core.Just ("metricName" Core..= metricName),
            Core.Just ("metricValue" Core..= metricValue),
            Core.Just ("metricUnit" Core..= metricUnit)
          ]
      )
