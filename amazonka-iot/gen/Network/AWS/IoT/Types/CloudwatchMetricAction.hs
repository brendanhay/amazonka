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
-- Module      : Network.AWS.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchMetricAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CloudwatchMetricAction where
  parseJSON =
    Prelude.withObject
      "CloudwatchMetricAction"
      ( \x ->
          CloudwatchMetricAction'
            Prelude.<$> (x Prelude..:? "metricTimestamp")
            Prelude.<*> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "metricNamespace")
            Prelude.<*> (x Prelude..: "metricName")
            Prelude.<*> (x Prelude..: "metricValue")
            Prelude.<*> (x Prelude..: "metricUnit")
      )

instance Prelude.Hashable CloudwatchMetricAction

instance Prelude.NFData CloudwatchMetricAction

instance Prelude.ToJSON CloudwatchMetricAction where
  toJSON CloudwatchMetricAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("metricTimestamp" Prelude..=)
              Prelude.<$> metricTimestamp,
            Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just
              ("metricNamespace" Prelude..= metricNamespace),
            Prelude.Just ("metricName" Prelude..= metricName),
            Prelude.Just ("metricValue" Prelude..= metricValue),
            Prelude.Just ("metricUnit" Prelude..= metricUnit)
          ]
      )
