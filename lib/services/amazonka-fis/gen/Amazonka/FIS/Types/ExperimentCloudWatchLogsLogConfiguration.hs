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
-- Module      : Amazonka.FIS.Types.ExperimentCloudWatchLogsLogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentCloudWatchLogsLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging to Amazon CloudWatch
-- Logs.
--
-- /See:/ 'newExperimentCloudWatchLogsLogConfiguration' smart constructor.
data ExperimentCloudWatchLogsLogConfiguration = ExperimentCloudWatchLogsLogConfiguration'
  { -- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
    -- log group.
    logGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentCloudWatchLogsLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'experimentCloudWatchLogsLogConfiguration_logGroupArn' - The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
newExperimentCloudWatchLogsLogConfiguration ::
  ExperimentCloudWatchLogsLogConfiguration
newExperimentCloudWatchLogsLogConfiguration =
  ExperimentCloudWatchLogsLogConfiguration'
    { logGroupArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
experimentCloudWatchLogsLogConfiguration_logGroupArn :: Lens.Lens' ExperimentCloudWatchLogsLogConfiguration (Prelude.Maybe Prelude.Text)
experimentCloudWatchLogsLogConfiguration_logGroupArn = Lens.lens (\ExperimentCloudWatchLogsLogConfiguration' {logGroupArn} -> logGroupArn) (\s@ExperimentCloudWatchLogsLogConfiguration' {} a -> s {logGroupArn = a} :: ExperimentCloudWatchLogsLogConfiguration)

instance
  Core.FromJSON
    ExperimentCloudWatchLogsLogConfiguration
  where
  parseJSON =
    Core.withObject
      "ExperimentCloudWatchLogsLogConfiguration"
      ( \x ->
          ExperimentCloudWatchLogsLogConfiguration'
            Prelude.<$> (x Core..:? "logGroupArn")
      )

instance
  Prelude.Hashable
    ExperimentCloudWatchLogsLogConfiguration
  where
  hashWithSalt
    _salt
    ExperimentCloudWatchLogsLogConfiguration' {..} =
      _salt `Prelude.hashWithSalt` logGroupArn

instance
  Prelude.NFData
    ExperimentCloudWatchLogsLogConfiguration
  where
  rnf ExperimentCloudWatchLogsLogConfiguration' {..} =
    Prelude.rnf logGroupArn
