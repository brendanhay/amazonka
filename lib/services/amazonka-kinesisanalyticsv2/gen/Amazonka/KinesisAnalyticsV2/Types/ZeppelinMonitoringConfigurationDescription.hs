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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.LogLevel
import qualified Amazonka.Prelude as Prelude

-- | The monitoring configuration for Apache Zeppelin within a Kinesis Data
-- Analytics Studio notebook.
--
-- /See:/ 'newZeppelinMonitoringConfigurationDescription' smart constructor.
data ZeppelinMonitoringConfigurationDescription = ZeppelinMonitoringConfigurationDescription'
  { -- | Describes the verbosity of the CloudWatch Logs for an application.
    logLevel :: Prelude.Maybe LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZeppelinMonitoringConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'zeppelinMonitoringConfigurationDescription_logLevel' - Describes the verbosity of the CloudWatch Logs for an application.
newZeppelinMonitoringConfigurationDescription ::
  ZeppelinMonitoringConfigurationDescription
newZeppelinMonitoringConfigurationDescription =
  ZeppelinMonitoringConfigurationDescription'
    { logLevel =
        Prelude.Nothing
    }

-- | Describes the verbosity of the CloudWatch Logs for an application.
zeppelinMonitoringConfigurationDescription_logLevel :: Lens.Lens' ZeppelinMonitoringConfigurationDescription (Prelude.Maybe LogLevel)
zeppelinMonitoringConfigurationDescription_logLevel = Lens.lens (\ZeppelinMonitoringConfigurationDescription' {logLevel} -> logLevel) (\s@ZeppelinMonitoringConfigurationDescription' {} a -> s {logLevel = a} :: ZeppelinMonitoringConfigurationDescription)

instance
  Data.FromJSON
    ZeppelinMonitoringConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "ZeppelinMonitoringConfigurationDescription"
      ( \x ->
          ZeppelinMonitoringConfigurationDescription'
            Prelude.<$> (x Data..:? "LogLevel")
      )

instance
  Prelude.Hashable
    ZeppelinMonitoringConfigurationDescription
  where
  hashWithSalt
    _salt
    ZeppelinMonitoringConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` logLevel

instance
  Prelude.NFData
    ZeppelinMonitoringConfigurationDescription
  where
  rnf ZeppelinMonitoringConfigurationDescription' {..} =
    Prelude.rnf logLevel
