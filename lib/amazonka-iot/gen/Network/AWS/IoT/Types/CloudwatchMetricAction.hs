{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CloudwatchMetricAction
  ( CloudwatchMetricAction (..)
  -- * Smart constructor
  , mkCloudwatchMetricAction
  -- * Lenses
  , cmaRoleArn
  , cmaMetricNamespace
  , cmaMetricName
  , cmaMetricValue
  , cmaMetricUnit
  , cmaMetricTimestamp
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action that captures a CloudWatch metric.
--
-- /See:/ 'mkCloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
  { roleArn :: Types.AwsArn
    -- ^ The IAM role that allows access to the CloudWatch metric.
  , metricNamespace :: Core.Text
    -- ^ The CloudWatch metric namespace name.
  , metricName :: Core.Text
    -- ^ The CloudWatch metric name.
  , metricValue :: Core.Text
    -- ^ The CloudWatch metric value.
  , metricUnit :: Core.Text
    -- ^ The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
  , metricTimestamp :: Core.Maybe Core.Text
    -- ^ An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudwatchMetricAction' value with any optional fields omitted.
mkCloudwatchMetricAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Core.Text -- ^ 'metricNamespace'
    -> Core.Text -- ^ 'metricName'
    -> Core.Text -- ^ 'metricValue'
    -> Core.Text -- ^ 'metricUnit'
    -> CloudwatchMetricAction
mkCloudwatchMetricAction roleArn metricNamespace metricName
  metricValue metricUnit
  = CloudwatchMetricAction'{roleArn, metricNamespace, metricName,
                            metricValue, metricUnit, metricTimestamp = Core.Nothing}

-- | The IAM role that allows access to the CloudWatch metric.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaRoleArn :: Lens.Lens' CloudwatchMetricAction Types.AwsArn
cmaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cmaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The CloudWatch metric namespace name.
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricNamespace :: Lens.Lens' CloudwatchMetricAction Core.Text
cmaMetricNamespace = Lens.field @"metricNamespace"
{-# INLINEABLE cmaMetricNamespace #-}
{-# DEPRECATED metricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead"  #-}

-- | The CloudWatch metric name.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricName :: Lens.Lens' CloudwatchMetricAction Core.Text
cmaMetricName = Lens.field @"metricName"
{-# INLINEABLE cmaMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The CloudWatch metric value.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricValue :: Lens.Lens' CloudwatchMetricAction Core.Text
cmaMetricValue = Lens.field @"metricValue"
{-# INLINEABLE cmaMetricValue #-}
{-# DEPRECATED metricValue "Use generic-lens or generic-optics with 'metricValue' instead"  #-}

-- | The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
--
-- /Note:/ Consider using 'metricUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricUnit :: Lens.Lens' CloudwatchMetricAction Core.Text
cmaMetricUnit = Lens.field @"metricUnit"
{-# INLINEABLE cmaMetricUnit #-}
{-# DEPRECATED metricUnit "Use generic-lens or generic-optics with 'metricUnit' instead"  #-}

-- | An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
--
-- /Note:/ Consider using 'metricTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmaMetricTimestamp :: Lens.Lens' CloudwatchMetricAction (Core.Maybe Core.Text)
cmaMetricTimestamp = Lens.field @"metricTimestamp"
{-# INLINEABLE cmaMetricTimestamp #-}
{-# DEPRECATED metricTimestamp "Use generic-lens or generic-optics with 'metricTimestamp' instead"  #-}

instance Core.FromJSON CloudwatchMetricAction where
        toJSON CloudwatchMetricAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("metricNamespace" Core..= metricNamespace),
                  Core.Just ("metricName" Core..= metricName),
                  Core.Just ("metricValue" Core..= metricValue),
                  Core.Just ("metricUnit" Core..= metricUnit),
                  ("metricTimestamp" Core..=) Core.<$> metricTimestamp])

instance Core.FromJSON CloudwatchMetricAction where
        parseJSON
          = Core.withObject "CloudwatchMetricAction" Core.$
              \ x ->
                CloudwatchMetricAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "metricNamespace" Core.<*>
                    x Core..: "metricName"
                    Core.<*> x Core..: "metricValue"
                    Core.<*> x Core..: "metricUnit"
                    Core.<*> x Core..:? "metricTimestamp"
