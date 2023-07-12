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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.LogLevel
import qualified Amazonka.Prelude as Prelude

-- | Updates to the monitoring configuration for Apache Zeppelin within a
-- Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newZeppelinMonitoringConfigurationUpdate' smart constructor.
data ZeppelinMonitoringConfigurationUpdate = ZeppelinMonitoringConfigurationUpdate'
  { -- | Updates to the logging level for Apache Zeppelin within a Kinesis Data
    -- Analytics Studio notebook.
    logLevelUpdate :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZeppelinMonitoringConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevelUpdate', 'zeppelinMonitoringConfigurationUpdate_logLevelUpdate' - Updates to the logging level for Apache Zeppelin within a Kinesis Data
-- Analytics Studio notebook.
newZeppelinMonitoringConfigurationUpdate ::
  -- | 'logLevelUpdate'
  LogLevel ->
  ZeppelinMonitoringConfigurationUpdate
newZeppelinMonitoringConfigurationUpdate
  pLogLevelUpdate_ =
    ZeppelinMonitoringConfigurationUpdate'
      { logLevelUpdate =
          pLogLevelUpdate_
      }

-- | Updates to the logging level for Apache Zeppelin within a Kinesis Data
-- Analytics Studio notebook.
zeppelinMonitoringConfigurationUpdate_logLevelUpdate :: Lens.Lens' ZeppelinMonitoringConfigurationUpdate LogLevel
zeppelinMonitoringConfigurationUpdate_logLevelUpdate = Lens.lens (\ZeppelinMonitoringConfigurationUpdate' {logLevelUpdate} -> logLevelUpdate) (\s@ZeppelinMonitoringConfigurationUpdate' {} a -> s {logLevelUpdate = a} :: ZeppelinMonitoringConfigurationUpdate)

instance
  Prelude.Hashable
    ZeppelinMonitoringConfigurationUpdate
  where
  hashWithSalt
    _salt
    ZeppelinMonitoringConfigurationUpdate' {..} =
      _salt `Prelude.hashWithSalt` logLevelUpdate

instance
  Prelude.NFData
    ZeppelinMonitoringConfigurationUpdate
  where
  rnf ZeppelinMonitoringConfigurationUpdate' {..} =
    Prelude.rnf logLevelUpdate

instance
  Data.ToJSON
    ZeppelinMonitoringConfigurationUpdate
  where
  toJSON ZeppelinMonitoringConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LogLevelUpdate" Data..= logLevelUpdate)
          ]
      )
