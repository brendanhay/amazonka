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
-- Module      : Amazonka.CostExplorer.Types.AnomalyMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalyMonitor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.Expression
import Amazonka.CostExplorer.Types.MonitorDimension
import Amazonka.CostExplorer.Types.MonitorType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This object continuously inspects your account\'s cost data for
-- anomalies. It\'s based on @MonitorType@ and @MonitorSpecification@. The
-- content consists of detailed metadata and the current status of the
-- monitor object.
--
-- /See:/ 'newAnomalyMonitor' smart constructor.
data AnomalyMonitor = AnomalyMonitor'
  { -- | The date when the monitor was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The value for evaluated dimensions.
    dimensionalValueCount :: Prelude.Maybe Prelude.Natural,
    -- | The date when the monitor last evaluated for anomalies.
    lastEvaluatedDate :: Prelude.Maybe Prelude.Text,
    -- | The date when the monitor was last updated.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) value.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The dimensions to evaluate.
    monitorDimension :: Prelude.Maybe MonitorDimension,
    monitorSpecification :: Prelude.Maybe Expression,
    -- | The name of the monitor.
    monitorName :: Prelude.Text,
    -- | The possible type values.
    monitorType :: MonitorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'anomalyMonitor_creationDate' - The date when the monitor was created.
--
-- 'dimensionalValueCount', 'anomalyMonitor_dimensionalValueCount' - The value for evaluated dimensions.
--
-- 'lastEvaluatedDate', 'anomalyMonitor_lastEvaluatedDate' - The date when the monitor last evaluated for anomalies.
--
-- 'lastUpdatedDate', 'anomalyMonitor_lastUpdatedDate' - The date when the monitor was last updated.
--
-- 'monitorArn', 'anomalyMonitor_monitorArn' - The Amazon Resource Name (ARN) value.
--
-- 'monitorDimension', 'anomalyMonitor_monitorDimension' - The dimensions to evaluate.
--
-- 'monitorSpecification', 'anomalyMonitor_monitorSpecification' - Undocumented member.
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
    { creationDate = Prelude.Nothing,
      dimensionalValueCount = Prelude.Nothing,
      lastEvaluatedDate = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      monitorDimension = Prelude.Nothing,
      monitorSpecification = Prelude.Nothing,
      monitorName = pMonitorName_,
      monitorType = pMonitorType_
    }

-- | The date when the monitor was created.
anomalyMonitor_creationDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_creationDate = Lens.lens (\AnomalyMonitor' {creationDate} -> creationDate) (\s@AnomalyMonitor' {} a -> s {creationDate = a} :: AnomalyMonitor)

-- | The value for evaluated dimensions.
anomalyMonitor_dimensionalValueCount :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Natural)
anomalyMonitor_dimensionalValueCount = Lens.lens (\AnomalyMonitor' {dimensionalValueCount} -> dimensionalValueCount) (\s@AnomalyMonitor' {} a -> s {dimensionalValueCount = a} :: AnomalyMonitor)

-- | The date when the monitor last evaluated for anomalies.
anomalyMonitor_lastEvaluatedDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_lastEvaluatedDate = Lens.lens (\AnomalyMonitor' {lastEvaluatedDate} -> lastEvaluatedDate) (\s@AnomalyMonitor' {} a -> s {lastEvaluatedDate = a} :: AnomalyMonitor)

-- | The date when the monitor was last updated.
anomalyMonitor_lastUpdatedDate :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_lastUpdatedDate = Lens.lens (\AnomalyMonitor' {lastUpdatedDate} -> lastUpdatedDate) (\s@AnomalyMonitor' {} a -> s {lastUpdatedDate = a} :: AnomalyMonitor)

-- | The Amazon Resource Name (ARN) value.
anomalyMonitor_monitorArn :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Prelude.Text)
anomalyMonitor_monitorArn = Lens.lens (\AnomalyMonitor' {monitorArn} -> monitorArn) (\s@AnomalyMonitor' {} a -> s {monitorArn = a} :: AnomalyMonitor)

-- | The dimensions to evaluate.
anomalyMonitor_monitorDimension :: Lens.Lens' AnomalyMonitor (Prelude.Maybe MonitorDimension)
anomalyMonitor_monitorDimension = Lens.lens (\AnomalyMonitor' {monitorDimension} -> monitorDimension) (\s@AnomalyMonitor' {} a -> s {monitorDimension = a} :: AnomalyMonitor)

-- | Undocumented member.
anomalyMonitor_monitorSpecification :: Lens.Lens' AnomalyMonitor (Prelude.Maybe Expression)
anomalyMonitor_monitorSpecification = Lens.lens (\AnomalyMonitor' {monitorSpecification} -> monitorSpecification) (\s@AnomalyMonitor' {} a -> s {monitorSpecification = a} :: AnomalyMonitor)

-- | The name of the monitor.
anomalyMonitor_monitorName :: Lens.Lens' AnomalyMonitor Prelude.Text
anomalyMonitor_monitorName = Lens.lens (\AnomalyMonitor' {monitorName} -> monitorName) (\s@AnomalyMonitor' {} a -> s {monitorName = a} :: AnomalyMonitor)

-- | The possible type values.
anomalyMonitor_monitorType :: Lens.Lens' AnomalyMonitor MonitorType
anomalyMonitor_monitorType = Lens.lens (\AnomalyMonitor' {monitorType} -> monitorType) (\s@AnomalyMonitor' {} a -> s {monitorType = a} :: AnomalyMonitor)

instance Data.FromJSON AnomalyMonitor where
  parseJSON =
    Data.withObject
      "AnomalyMonitor"
      ( \x ->
          AnomalyMonitor'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DimensionalValueCount")
            Prelude.<*> (x Data..:? "LastEvaluatedDate")
            Prelude.<*> (x Data..:? "LastUpdatedDate")
            Prelude.<*> (x Data..:? "MonitorArn")
            Prelude.<*> (x Data..:? "MonitorDimension")
            Prelude.<*> (x Data..:? "MonitorSpecification")
            Prelude.<*> (x Data..: "MonitorName")
            Prelude.<*> (x Data..: "MonitorType")
      )

instance Prelude.Hashable AnomalyMonitor where
  hashWithSalt _salt AnomalyMonitor' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` dimensionalValueCount
      `Prelude.hashWithSalt` lastEvaluatedDate
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` monitorDimension
      `Prelude.hashWithSalt` monitorSpecification
      `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` monitorType

instance Prelude.NFData AnomalyMonitor where
  rnf AnomalyMonitor' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf dimensionalValueCount
      `Prelude.seq` Prelude.rnf lastEvaluatedDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf monitorDimension
      `Prelude.seq` Prelude.rnf monitorSpecification
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf monitorType

instance Data.ToJSON AnomalyMonitor where
  toJSON AnomalyMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationDate" Data..=) Prelude.<$> creationDate,
            ("DimensionalValueCount" Data..=)
              Prelude.<$> dimensionalValueCount,
            ("LastEvaluatedDate" Data..=)
              Prelude.<$> lastEvaluatedDate,
            ("LastUpdatedDate" Data..=)
              Prelude.<$> lastUpdatedDate,
            ("MonitorArn" Data..=) Prelude.<$> monitorArn,
            ("MonitorDimension" Data..=)
              Prelude.<$> monitorDimension,
            ("MonitorSpecification" Data..=)
              Prelude.<$> monitorSpecification,
            Prelude.Just ("MonitorName" Data..= monitorName),
            Prelude.Just ("MonitorType" Data..= monitorType)
          ]
      )
