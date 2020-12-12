{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyMonitor
  ( AnomalyMonitor (..),

    -- * Smart constructor
    mkAnomalyMonitor,

    -- * Lenses
    amDimensionalValueCount,
    amMonitorSpecification,
    amMonitorDimension,
    amCreationDate,
    amLastUpdatedDate,
    amLastEvaluatedDate,
    amMonitorARN,
    amMonitorName,
    amMonitorType,
  )
where

import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This object continuously inspects your account's cost data for anomalies, based on @MonitorType@ and @MonitorSpecification@ . The content consists of detailed metadata and the current status of the monitor object.
--
-- /See:/ 'mkAnomalyMonitor' smart constructor.
data AnomalyMonitor = AnomalyMonitor'
  { dimensionalValueCount ::
      Lude.Maybe Lude.Natural,
    monitorSpecification :: Lude.Maybe Expression,
    monitorDimension :: Lude.Maybe MonitorDimension,
    creationDate :: Lude.Maybe Lude.Text,
    lastUpdatedDate :: Lude.Maybe Lude.Text,
    lastEvaluatedDate :: Lude.Maybe Lude.Text,
    monitorARN :: Lude.Maybe Lude.Text,
    monitorName :: Lude.Text,
    monitorType :: MonitorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalyMonitor' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date when the monitor was created.
-- * 'dimensionalValueCount' - The value for evaluated dimensions.
-- * 'lastEvaluatedDate' - The date when the monitor last evaluated for anomalies.
-- * 'lastUpdatedDate' - The date when the monitor was last updated.
-- * 'monitorARN' - The Amazon Resource Name (ARN) value.
-- * 'monitorDimension' - The dimensions to evaluate.
-- * 'monitorName' - The name of the monitor.
-- * 'monitorSpecification' - Undocumented field.
-- * 'monitorType' - The possible type values.
mkAnomalyMonitor ::
  -- | 'monitorName'
  Lude.Text ->
  -- | 'monitorType'
  MonitorType ->
  AnomalyMonitor
mkAnomalyMonitor pMonitorName_ pMonitorType_ =
  AnomalyMonitor'
    { dimensionalValueCount = Lude.Nothing,
      monitorSpecification = Lude.Nothing,
      monitorDimension = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      lastEvaluatedDate = Lude.Nothing,
      monitorARN = Lude.Nothing,
      monitorName = pMonitorName_,
      monitorType = pMonitorType_
    }

-- | The value for evaluated dimensions.
--
-- /Note:/ Consider using 'dimensionalValueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDimensionalValueCount :: Lens.Lens' AnomalyMonitor (Lude.Maybe Lude.Natural)
amDimensionalValueCount = Lens.lens (dimensionalValueCount :: AnomalyMonitor -> Lude.Maybe Lude.Natural) (\s a -> s {dimensionalValueCount = a} :: AnomalyMonitor)
{-# DEPRECATED amDimensionalValueCount "Use generic-lens or generic-optics with 'dimensionalValueCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'monitorSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMonitorSpecification :: Lens.Lens' AnomalyMonitor (Lude.Maybe Expression)
amMonitorSpecification = Lens.lens (monitorSpecification :: AnomalyMonitor -> Lude.Maybe Expression) (\s a -> s {monitorSpecification = a} :: AnomalyMonitor)
{-# DEPRECATED amMonitorSpecification "Use generic-lens or generic-optics with 'monitorSpecification' instead." #-}

-- | The dimensions to evaluate.
--
-- /Note:/ Consider using 'monitorDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMonitorDimension :: Lens.Lens' AnomalyMonitor (Lude.Maybe MonitorDimension)
amMonitorDimension = Lens.lens (monitorDimension :: AnomalyMonitor -> Lude.Maybe MonitorDimension) (\s a -> s {monitorDimension = a} :: AnomalyMonitor)
{-# DEPRECATED amMonitorDimension "Use generic-lens or generic-optics with 'monitorDimension' instead." #-}

-- | The date when the monitor was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCreationDate :: Lens.Lens' AnomalyMonitor (Lude.Maybe Lude.Text)
amCreationDate = Lens.lens (creationDate :: AnomalyMonitor -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: AnomalyMonitor)
{-# DEPRECATED amCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The date when the monitor was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amLastUpdatedDate :: Lens.Lens' AnomalyMonitor (Lude.Maybe Lude.Text)
amLastUpdatedDate = Lens.lens (lastUpdatedDate :: AnomalyMonitor -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedDate = a} :: AnomalyMonitor)
{-# DEPRECATED amLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The date when the monitor last evaluated for anomalies.
--
-- /Note:/ Consider using 'lastEvaluatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amLastEvaluatedDate :: Lens.Lens' AnomalyMonitor (Lude.Maybe Lude.Text)
amLastEvaluatedDate = Lens.lens (lastEvaluatedDate :: AnomalyMonitor -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedDate = a} :: AnomalyMonitor)
{-# DEPRECATED amLastEvaluatedDate "Use generic-lens or generic-optics with 'lastEvaluatedDate' instead." #-}

-- | The Amazon Resource Name (ARN) value.
--
-- /Note:/ Consider using 'monitorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMonitorARN :: Lens.Lens' AnomalyMonitor (Lude.Maybe Lude.Text)
amMonitorARN = Lens.lens (monitorARN :: AnomalyMonitor -> Lude.Maybe Lude.Text) (\s a -> s {monitorARN = a} :: AnomalyMonitor)
{-# DEPRECATED amMonitorARN "Use generic-lens or generic-optics with 'monitorARN' instead." #-}

-- | The name of the monitor.
--
-- /Note:/ Consider using 'monitorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMonitorName :: Lens.Lens' AnomalyMonitor Lude.Text
amMonitorName = Lens.lens (monitorName :: AnomalyMonitor -> Lude.Text) (\s a -> s {monitorName = a} :: AnomalyMonitor)
{-# DEPRECATED amMonitorName "Use generic-lens or generic-optics with 'monitorName' instead." #-}

-- | The possible type values.
--
-- /Note:/ Consider using 'monitorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMonitorType :: Lens.Lens' AnomalyMonitor MonitorType
amMonitorType = Lens.lens (monitorType :: AnomalyMonitor -> MonitorType) (\s a -> s {monitorType = a} :: AnomalyMonitor)
{-# DEPRECATED amMonitorType "Use generic-lens or generic-optics with 'monitorType' instead." #-}

instance Lude.FromJSON AnomalyMonitor where
  parseJSON =
    Lude.withObject
      "AnomalyMonitor"
      ( \x ->
          AnomalyMonitor'
            Lude.<$> (x Lude..:? "DimensionalValueCount")
            Lude.<*> (x Lude..:? "MonitorSpecification")
            Lude.<*> (x Lude..:? "MonitorDimension")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastUpdatedDate")
            Lude.<*> (x Lude..:? "LastEvaluatedDate")
            Lude.<*> (x Lude..:? "MonitorArn")
            Lude.<*> (x Lude..: "MonitorName")
            Lude.<*> (x Lude..: "MonitorType")
      )

instance Lude.ToJSON AnomalyMonitor where
  toJSON AnomalyMonitor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DimensionalValueCount" Lude..=) Lude.<$> dimensionalValueCount,
            ("MonitorSpecification" Lude..=) Lude.<$> monitorSpecification,
            ("MonitorDimension" Lude..=) Lude.<$> monitorDimension,
            ("CreationDate" Lude..=) Lude.<$> creationDate,
            ("LastUpdatedDate" Lude..=) Lude.<$> lastUpdatedDate,
            ("LastEvaluatedDate" Lude..=) Lude.<$> lastEvaluatedDate,
            ("MonitorArn" Lude..=) Lude.<$> monitorARN,
            Lude.Just ("MonitorName" Lude..= monitorName),
            Lude.Just ("MonitorType" Lude..= monitorType)
          ]
      )
