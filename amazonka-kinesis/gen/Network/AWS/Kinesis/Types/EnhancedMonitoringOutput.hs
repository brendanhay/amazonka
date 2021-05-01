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
-- Module      : Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMonitoringOutput where

import Network.AWS.Kinesis.Types.MetricsName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output for EnableEnhancedMonitoring and
-- DisableEnhancedMonitoring.
--
-- /See:/ 'newEnhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { -- | Represents the current state of the metrics that are in the enhanced
    -- state before the operation.
    currentShardLevelMetrics :: Prelude.Maybe [MetricsName],
    -- | The name of the Kinesis data stream.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | Represents the list of all the metrics that would be in the enhanced
    -- state after the operation.
    desiredShardLevelMetrics :: Prelude.Maybe [MetricsName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'streamName', 'enhancedMonitoringOutput_streamName' - The name of the Kinesis data stream.
--
-- 'desiredShardLevelMetrics', 'enhancedMonitoringOutput_desiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
newEnhancedMonitoringOutput ::
  EnhancedMonitoringOutput
newEnhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { currentShardLevelMetrics =
        Prelude.Nothing,
      streamName = Prelude.Nothing,
      desiredShardLevelMetrics = Prelude.Nothing
    }

-- | Represents the current state of the metrics that are in the enhanced
-- state before the operation.
enhancedMonitoringOutput_currentShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_currentShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {currentShardLevelMetrics} -> currentShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {currentShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Kinesis data stream.
enhancedMonitoringOutput_streamName :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe Prelude.Text)
enhancedMonitoringOutput_streamName = Lens.lens (\EnhancedMonitoringOutput' {streamName} -> streamName) (\s@EnhancedMonitoringOutput' {} a -> s {streamName = a} :: EnhancedMonitoringOutput)

-- | Represents the list of all the metrics that would be in the enhanced
-- state after the operation.
enhancedMonitoringOutput_desiredShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Prelude.Maybe [MetricsName])
enhancedMonitoringOutput_desiredShardLevelMetrics = Lens.lens (\EnhancedMonitoringOutput' {desiredShardLevelMetrics} -> desiredShardLevelMetrics) (\s@EnhancedMonitoringOutput' {} a -> s {desiredShardLevelMetrics = a} :: EnhancedMonitoringOutput) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON EnhancedMonitoringOutput where
  parseJSON =
    Prelude.withObject
      "EnhancedMonitoringOutput"
      ( \x ->
          EnhancedMonitoringOutput'
            Prelude.<$> ( x Prelude..:? "CurrentShardLevelMetrics"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "StreamName")
            Prelude.<*> ( x Prelude..:? "DesiredShardLevelMetrics"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EnhancedMonitoringOutput

instance Prelude.NFData EnhancedMonitoringOutput
