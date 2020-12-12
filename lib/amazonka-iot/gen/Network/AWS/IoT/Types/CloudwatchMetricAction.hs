{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchMetricAction
  ( CloudwatchMetricAction (..),

    -- * Smart constructor
    mkCloudwatchMetricAction,

    -- * Lenses
    cmaMetricTimestamp,
    cmaRoleARN,
    cmaMetricNamespace,
    cmaMetricName,
    cmaMetricValue,
    cmaMetricUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action that captures a CloudWatch metric.
--
-- /See:/ 'mkCloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
  { metricTimestamp ::
      Lude.Maybe Lude.Text,
    roleARN :: Lude.Text,
    metricNamespace :: Lude.Text,
    metricName :: Lude.Text,
    metricValue :: Lude.Text,
    metricUnit :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudwatchMetricAction' with the minimum fields required to make a request.
--
-- * 'metricName' - The CloudWatch metric name.
-- * 'metricNamespace' - The CloudWatch metric namespace name.
-- * 'metricTimestamp' - An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
-- * 'metricUnit' - The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
-- * 'metricValue' - The CloudWatch metric value.
-- * 'roleARN' - The IAM role that allows access to the CloudWatch metric.
mkCloudwatchMetricAction ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'metricNamespace'
  Lude.Text ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'metricValue'
  Lude.Text ->
  -- | 'metricUnit'
  Lude.Text ->
  CloudwatchMetricAction
mkCloudwatchMetricAction
  pRoleARN_
  pMetricNamespace_
  pMetricName_
  pMetricValue_
  pMetricUnit_ =
    CloudwatchMetricAction'
      { metricTimestamp = Lude.Nothing,
        roleARN = pRoleARN_,
        metricNamespace = pMetricNamespace_,
        metricName = pMetricName_,
        metricValue = pMetricValue_,
        metricUnit = pMetricUnit_
      }

-- | An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
--
-- /Note:/ Consider using 'metricTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricTimestamp :: Lens.Lens' CloudwatchMetricAction (Lude.Maybe Lude.Text)
cmaMetricTimestamp = Lens.lens (metricTimestamp :: CloudwatchMetricAction -> Lude.Maybe Lude.Text) (\s a -> s {metricTimestamp = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaMetricTimestamp "Use generic-lens or generic-optics with 'metricTimestamp' instead." #-}

-- | The IAM role that allows access to the CloudWatch metric.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaRoleARN :: Lens.Lens' CloudwatchMetricAction Lude.Text
cmaRoleARN = Lens.lens (roleARN :: CloudwatchMetricAction -> Lude.Text) (\s a -> s {roleARN = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The CloudWatch metric namespace name.
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricNamespace :: Lens.Lens' CloudwatchMetricAction Lude.Text
cmaMetricNamespace = Lens.lens (metricNamespace :: CloudwatchMetricAction -> Lude.Text) (\s a -> s {metricNamespace = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaMetricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead." #-}

-- | The CloudWatch metric name.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricName :: Lens.Lens' CloudwatchMetricAction Lude.Text
cmaMetricName = Lens.lens (metricName :: CloudwatchMetricAction -> Lude.Text) (\s a -> s {metricName = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The CloudWatch metric value.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricValue :: Lens.Lens' CloudwatchMetricAction Lude.Text
cmaMetricValue = Lens.lens (metricValue :: CloudwatchMetricAction -> Lude.Text) (\s a -> s {metricValue = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
--
-- /Note:/ Consider using 'metricUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricUnit :: Lens.Lens' CloudwatchMetricAction Lude.Text
cmaMetricUnit = Lens.lens (metricUnit :: CloudwatchMetricAction -> Lude.Text) (\s a -> s {metricUnit = a} :: CloudwatchMetricAction)
{-# DEPRECATED cmaMetricUnit "Use generic-lens or generic-optics with 'metricUnit' instead." #-}

instance Lude.FromJSON CloudwatchMetricAction where
  parseJSON =
    Lude.withObject
      "CloudwatchMetricAction"
      ( \x ->
          CloudwatchMetricAction'
            Lude.<$> (x Lude..:? "metricTimestamp")
            Lude.<*> (x Lude..: "roleArn")
            Lude.<*> (x Lude..: "metricNamespace")
            Lude.<*> (x Lude..: "metricName")
            Lude.<*> (x Lude..: "metricValue")
            Lude.<*> (x Lude..: "metricUnit")
      )

instance Lude.ToJSON CloudwatchMetricAction where
  toJSON CloudwatchMetricAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("metricTimestamp" Lude..=) Lude.<$> metricTimestamp,
            Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("metricNamespace" Lude..= metricNamespace),
            Lude.Just ("metricName" Lude..= metricName),
            Lude.Just ("metricValue" Lude..= metricValue),
            Lude.Just ("metricUnit" Lude..= metricUnit)
          ]
      )
