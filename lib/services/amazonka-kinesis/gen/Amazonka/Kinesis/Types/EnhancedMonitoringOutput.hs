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
-- Module      : Amazonka.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.EnhancedMonitoringOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.MetricsName
import qualified Amazonka.Prelude as Prelude

-- | Represents the output for EnableEnhancedMonitoring and
-- DisableEnhancedMonitoring.
--
-- /See:/ 'newEnhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { -- | Represents the current state of the metrics that are in the enhanced
    -- state before the operation.
    currentShardLevelMetrics :: Prelude.Maybe [MetricsName],
    -- | Represents the list of all the metrics that would be in the enhanced
    -- state after the operation.
    desiredShardLevelMetrics :: Prelude.Maybe [MetricsName],
    -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the Kinesis data stream.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnhancedMonitoringOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentShardLevelMetrics', 'enhancedMonitoringOutput_currentShardLevelMetrics' - Represents the current state of the metrics that are in the enhanced
-- state before the operation.
--
-- 'desiredShardLevelMetrics', 'enhancedMonitoringOutput_desiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
--
-- 'streamARN', 'enhancedMonitoringOutput_streamARN' - The ARN of the stream.
--
-- 'streamName', 'enhancedMonitoringOutput_streamName' - The name of the Kinesis data stream.
newEnhancedMonitoringOutput ::
  EnhancedMonitoringOutput
newEnhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { currentShardLevelMetrics =
        Prelude.Nothing,
      desiredShardLevelMetrics = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Represents the current state of the metrics that are in the enhanced
-- state before the operation.
enhancedMonitoringOutput_currentShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_currentShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {currentShardLevelMetrics} -> currentShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {currentShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Lens.coerced

-- | Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
enhancedMonitoringOutput_desiredShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_desiredShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {desiredShardLevelMetrics} -> desiredShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {desiredShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the stream.
enhancedMonitoringOutput_streamARN :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe Prelude.Text)
enhancedMonitoringOutput_streamARN = Lens.lens (\EnhancedMonitoringOutput' {streamARN} -> streamARN) (\s@EnhancedMonitoringOutput' {} a -> s {streamARN = a} :: EnhancedMonitoringOutput)

-- | The name of the Kinesis data stream.
enhancedMonitoringOutput_streamName :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe Prelude.Text)
enhancedMonitoringOutput_streamName = Lens.lens (\EnhancedMonitoringOutput' {streamName} -> streamName) (\s@EnhancedMonitoringOutput' {} a -> s {streamName = a} :: EnhancedMonitoringOutput)

instance Data.FromJSON EnhancedMonitoringOutput where
  parseJSON =
    Data.withObject
      "EnhancedMonitoringOutput"
      ( \x ->
          EnhancedMonitoringOutput'
            Prelude.<$> ( x
                            Data..:? "CurrentShardLevelMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "DesiredShardLevelMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StreamARN")
            Prelude.<*> (x Data..:? "StreamName")
      )

instance Prelude.Hashable EnhancedMonitoringOutput where
  hashWithSalt _salt EnhancedMonitoringOutput' {..} =
    _salt
      `Prelude.hashWithSalt` currentShardLevelMetrics
      `Prelude.hashWithSalt` desiredShardLevelMetrics
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData EnhancedMonitoringOutput where
  rnf EnhancedMonitoringOutput' {..} =
    Prelude.rnf currentShardLevelMetrics
      `Prelude.seq` Prelude.rnf desiredShardLevelMetrics
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
