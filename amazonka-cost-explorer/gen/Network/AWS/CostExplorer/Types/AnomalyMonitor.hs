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
-- Module      : Network.AWS.CostExplorer.Types.AnomalyMonitor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyMonitor where

import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This object continuously inspects your account\'s cost data for
-- anomalies, based on @MonitorType@ and @MonitorSpecification@. The
-- content consists of detailed metadata and the current status of the
-- monitor object.
--
-- /See:/ 'newAnomalyMonitor' smart constructor.
data AnomalyMonitor = AnomalyMonitor'
  { -- | The date when the monitor last evaluated for anomalies.
    lastEvaluatedDate :: Prelude.Maybe Prelude.Text,
    monitorSpecification :: Prelude.Maybe Expression,
    -- | The date when the monitor was last updated.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
    -- | The date when the monitor was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The value for evaluated dimensions.
    dimensionalValueCount :: Prelude.Maybe Prelude.Natural,
    -- | The dimensions to evaluate.
    monitorDimension :: Prelude.Maybe MonitorDimension,
    -- | The Amazon Resource Name (ARN) value.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitor.
    monitorName :: Prelude.Text,
    -- | The possible type values.
    monitorType :: MonitorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedDate', 'anomalyMonitor_lastEvaluatedDate' - The date when the monitor last evaluated for anomalies.
--
-- 'monitorSpecification', 'anomalyMonitor_monitorSpecification' - Undocumented member.
--
-- 'lastUpdatedDate', 'anomalyMonitor_lastUpdatedDate' - The date when the monitor was last updated.
--
-- 'creationDate', 'anomalyMonitor_creationDate' - The date when the monitor was created.
--
-- 'dimensionalValueCount', 'anomalyMonitor_dimensionalValueCount' - The value for evaluated dimensions.
--
-- 'monitorDimension', 'anomalyMonitor_monitorDimension' - The dimensions to evaluate.
--
-- 'monitorArn', 'anomalyMonitor_monitorArn' - The Amazon Resource Name (ARN) value.
--
-- 'monitorName', 'anomalyMonitor_monitorName' - The name of the monitor.
--
-- 'monitorType', 'anomalyMonitor_monitorType' - The possible type values.
newAnomalyMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  -- | 'monitorType'
  MonitorType ->
  AnomalyMonitor
newAnomalyMonitor pMonitorName_ pMonitorType_ =
  AnomalyMonitor'
    { lastEvaluatedDate =
        Prelude.Nothing,
      monitorSpecification = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      dimensionalValueCount = Prelude.Nothing,
      monitorDimension = Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      monitorName = pMonitorName_,
      monitorType = pMonitorType_
    }

-- | The date when the monitor last evaluated for anomalies.
anomalyMonitor_lastEvaluatedDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_lastEvaluatedDate = Lens.lens (\AnomalyMonitor' {lastEvaluatedDate} -> lastEvaluatedDate) (\s@AnomalyMonitor' {} a -> s {lastEvaluatedDate = a} :: AnomalyMonitor)

-- | Undocumented member.
anomalyMonitor_monitorSpecification :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Expression)
anomalyMonitor_monitorSpecification = Lens.lens (\AnomalyMonitor' {monitorSpecification} -> monitorSpecification) (\s@AnomalyMonitor' {} a -> s {monitorSpecification = a} :: AnomalyMonitor)

-- | The date when the monitor was last updated.
anomalyMonitor_lastUpdatedDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_lastUpdatedDate = Lens.lens (\AnomalyMonitor' {lastUpdatedDate} -> lastUpdatedDate) (\s@AnomalyMonitor' {} a -> s {lastUpdatedDate = a} :: AnomalyMonitor)

-- | The date when the monitor was created.
anomalyMonitor_creationDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_creationDate = Lens.lens (\AnomalyMonitor' {creationDate} -> creationDate) (\s@AnomalyMonitor' {} a -> s {creationDate = a} :: AnomalyMonitor)

-- | The value for evaluated dimensions.
anomalyMonitor_dimensionalValueCount :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Natural)
anomalyMonitor_dimensionalValueCount = Lens.lens (\AnomalyMonitor' {dimensionalValueCount} -> dimensionalValueCount) (\s@AnomalyMonitor' {} a -> s {dimensionalValueCount = a} :: AnomalyMonitor)

-- | The dimensions to evaluate.
anomalyMonitor_monitorDimension :: Lens.Lens' AnomalyMonitor (Prelude.Maybe MonitorDimension)
anomalyMonitor_monitorDimension = Lens.lens (\AnomalyMonitor' {monitorDimension} -> monitorDimension) (\s@AnomalyMonitor' {} a -> s {monitorDimension = a} :: AnomalyMonitor)

-- | The Amazon Resource Name (ARN) value.
anomalyMonitor_monitorArn :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_monitorArn = Lens.lens (\AnomalyMonitor' {monitorArn} -> monitorArn) (\s@AnomalyMonitor' {} a -> s {monitorArn = a} :: AnomalyMonitor)

-- | The name of the monitor.
anomalyMonitor_monitorName :: Lens.Lens' AnomalyMonitor Prelude.Text
anomalyMonitor_monitorName = Lens.lens (\AnomalyMonitor' {monitorName} -> monitorName) (\s@AnomalyMonitor' {} a -> s {monitorName = a} :: AnomalyMonitor)

-- | The possible type values.
anomalyMonitor_monitorType :: Lens.Lens' AnomalyMonitor MonitorType
anomalyMonitor_monitorType = Lens.lens (\AnomalyMonitor' {monitorType} -> monitorType) (\s@AnomalyMonitor' {} a -> s {monitorType = a} :: AnomalyMonitor)

instance Prelude.FromJSON AnomalyMonitor where
  parseJSON =
    Prelude.withObject
      "AnomalyMonitor"
      ( \x ->
          AnomalyMonitor'
            Prelude.<$> (x Prelude..:? "LastEvaluatedDate")
            Prelude.<*> (x Prelude..:? "MonitorSpecification")
            Prelude.<*> (x Prelude..:? "LastUpdatedDate")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "DimensionalValueCount")
            Prelude.<*> (x Prelude..:? "MonitorDimension")
            Prelude.<*> (x Prelude..:? "MonitorArn")
            Prelude.<*> (x Prelude..: "MonitorName")
            Prelude.<*> (x Prelude..: "MonitorType")
      )

instance Prelude.Hashable AnomalyMonitor

instance Prelude.NFData AnomalyMonitor

instance Prelude.ToJSON AnomalyMonitor where
  toJSON AnomalyMonitor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LastEvaluatedDate" Prelude..=)
              Prelude.<$> lastEvaluatedDate,
            ("MonitorSpecification" Prelude..=)
              Prelude.<$> monitorSpecification,
            ("LastUpdatedDate" Prelude..=)
              Prelude.<$> lastUpdatedDate,
            ("CreationDate" Prelude..=) Prelude.<$> creationDate,
            ("DimensionalValueCount" Prelude..=)
              Prelude.<$> dimensionalValueCount,
            ("MonitorDimension" Prelude..=)
              Prelude.<$> monitorDimension,
            ("MonitorArn" Prelude..=) Prelude.<$> monitorArn,
            Prelude.Just ("MonitorName" Prelude..= monitorName),
            Prelude.Just ("MonitorType" Prelude..= monitorType)
          ]
      )
