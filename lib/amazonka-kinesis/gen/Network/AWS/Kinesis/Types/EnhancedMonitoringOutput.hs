-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
  ( EnhancedMonitoringOutput (..),

    -- * Smart constructor
    mkEnhancedMonitoringOutput,

    -- * Lenses
    emoDesiredShardLevelMetrics,
    emoCurrentShardLevelMetrics,
    emoStreamName,
  )
where

import Network.AWS.Kinesis.Types.MetricsName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output for 'EnableEnhancedMonitoring' and 'DisableEnhancedMonitoring' .
--
-- /See:/ 'mkEnhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { desiredShardLevelMetrics ::
      Lude.Maybe [MetricsName],
    currentShardLevelMetrics ::
      Lude.Maybe [MetricsName],
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnhancedMonitoringOutput' with the minimum fields required to make a request.
--
-- * 'currentShardLevelMetrics' - Represents the current state of the metrics that are in the enhanced state before the operation.
-- * 'desiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced state after the operation.
-- * 'streamName' - The name of the Kinesis data stream.
mkEnhancedMonitoringOutput ::
  EnhancedMonitoringOutput
mkEnhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { desiredShardLevelMetrics =
        Lude.Nothing,
      currentShardLevelMetrics = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | Represents the list of all the metrics that would be in the enhanced state after the operation.
--
-- /Note:/ Consider using 'desiredShardLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoDesiredShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Lude.Maybe [MetricsName])
emoDesiredShardLevelMetrics = Lens.lens (desiredShardLevelMetrics :: EnhancedMonitoringOutput -> Lude.Maybe [MetricsName]) (\s a -> s {desiredShardLevelMetrics = a} :: EnhancedMonitoringOutput)
{-# DEPRECATED emoDesiredShardLevelMetrics "Use generic-lens or generic-optics with 'desiredShardLevelMetrics' instead." #-}

-- | Represents the current state of the metrics that are in the enhanced state before the operation.
--
-- /Note:/ Consider using 'currentShardLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoCurrentShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Lude.Maybe [MetricsName])
emoCurrentShardLevelMetrics = Lens.lens (currentShardLevelMetrics :: EnhancedMonitoringOutput -> Lude.Maybe [MetricsName]) (\s a -> s {currentShardLevelMetrics = a} :: EnhancedMonitoringOutput)
{-# DEPRECATED emoCurrentShardLevelMetrics "Use generic-lens or generic-optics with 'currentShardLevelMetrics' instead." #-}

-- | The name of the Kinesis data stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoStreamName :: Lens.Lens' EnhancedMonitoringOutput (Lude.Maybe Lude.Text)
emoStreamName = Lens.lens (streamName :: EnhancedMonitoringOutput -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: EnhancedMonitoringOutput)
{-# DEPRECATED emoStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON EnhancedMonitoringOutput where
  parseJSON =
    Lude.withObject
      "EnhancedMonitoringOutput"
      ( \x ->
          EnhancedMonitoringOutput'
            Lude.<$> (x Lude..:? "DesiredShardLevelMetrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CurrentShardLevelMetrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StreamName")
      )
