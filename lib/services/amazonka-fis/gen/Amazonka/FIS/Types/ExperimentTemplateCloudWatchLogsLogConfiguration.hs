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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging to Amazon CloudWatch
-- Logs.
--
-- /See:/ 'newExperimentTemplateCloudWatchLogsLogConfiguration' smart constructor.
data ExperimentTemplateCloudWatchLogsLogConfiguration = ExperimentTemplateCloudWatchLogsLogConfiguration'
  { -- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
    -- log group.
    logGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateCloudWatchLogsLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'experimentTemplateCloudWatchLogsLogConfiguration_logGroupArn' - The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
newExperimentTemplateCloudWatchLogsLogConfiguration ::
  ExperimentTemplateCloudWatchLogsLogConfiguration
newExperimentTemplateCloudWatchLogsLogConfiguration =
  ExperimentTemplateCloudWatchLogsLogConfiguration'
    { logGroupArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
experimentTemplateCloudWatchLogsLogConfiguration_logGroupArn :: Lens.Lens' ExperimentTemplateCloudWatchLogsLogConfiguration (Prelude.Maybe Prelude.Text)
experimentTemplateCloudWatchLogsLogConfiguration_logGroupArn = Lens.lens (\ExperimentTemplateCloudWatchLogsLogConfiguration' {logGroupArn} -> logGroupArn) (\s@ExperimentTemplateCloudWatchLogsLogConfiguration' {} a -> s {logGroupArn = a} :: ExperimentTemplateCloudWatchLogsLogConfiguration)

instance
  Data.FromJSON
    ExperimentTemplateCloudWatchLogsLogConfiguration
  where
  parseJSON =
    Data.withObject
      "ExperimentTemplateCloudWatchLogsLogConfiguration"
      ( \x ->
          ExperimentTemplateCloudWatchLogsLogConfiguration'
            Prelude.<$> (x Data..:? "logGroupArn")
      )

instance
  Prelude.Hashable
    ExperimentTemplateCloudWatchLogsLogConfiguration
  where
  hashWithSalt
    _salt
    ExperimentTemplateCloudWatchLogsLogConfiguration' {..} =
      _salt `Prelude.hashWithSalt` logGroupArn

instance
  Prelude.NFData
    ExperimentTemplateCloudWatchLogsLogConfiguration
  where
  rnf
    ExperimentTemplateCloudWatchLogsLogConfiguration' {..} =
      Prelude.rnf logGroupArn
