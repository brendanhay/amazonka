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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfigurationInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for experiment logging to Amazon CloudWatch
-- Logs.
--
-- /See:/ 'newExperimentTemplateCloudWatchLogsLogConfigurationInput' smart constructor.
data ExperimentTemplateCloudWatchLogsLogConfigurationInput = ExperimentTemplateCloudWatchLogsLogConfigurationInput'
  { -- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
    -- log group.
    logGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateCloudWatchLogsLogConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'experimentTemplateCloudWatchLogsLogConfigurationInput_logGroupArn' - The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
newExperimentTemplateCloudWatchLogsLogConfigurationInput ::
  -- | 'logGroupArn'
  Prelude.Text ->
  ExperimentTemplateCloudWatchLogsLogConfigurationInput
newExperimentTemplateCloudWatchLogsLogConfigurationInput
  pLogGroupArn_ =
    ExperimentTemplateCloudWatchLogsLogConfigurationInput'
      { logGroupArn =
          pLogGroupArn_
      }

-- | The Amazon Resource Name (ARN) of the destination Amazon CloudWatch Logs
-- log group.
experimentTemplateCloudWatchLogsLogConfigurationInput_logGroupArn :: Lens.Lens' ExperimentTemplateCloudWatchLogsLogConfigurationInput Prelude.Text
experimentTemplateCloudWatchLogsLogConfigurationInput_logGroupArn = Lens.lens (\ExperimentTemplateCloudWatchLogsLogConfigurationInput' {logGroupArn} -> logGroupArn) (\s@ExperimentTemplateCloudWatchLogsLogConfigurationInput' {} a -> s {logGroupArn = a} :: ExperimentTemplateCloudWatchLogsLogConfigurationInput)

instance
  Prelude.Hashable
    ExperimentTemplateCloudWatchLogsLogConfigurationInput
  where
  hashWithSalt
    _salt
    ExperimentTemplateCloudWatchLogsLogConfigurationInput' {..} =
      _salt `Prelude.hashWithSalt` logGroupArn

instance
  Prelude.NFData
    ExperimentTemplateCloudWatchLogsLogConfigurationInput
  where
  rnf
    ExperimentTemplateCloudWatchLogsLogConfigurationInput' {..} =
      Prelude.rnf logGroupArn

instance
  Data.ToJSON
    ExperimentTemplateCloudWatchLogsLogConfigurationInput
  where
  toJSON
    ExperimentTemplateCloudWatchLogsLogConfigurationInput' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("logGroupArn" Data..= logGroupArn)]
        )
