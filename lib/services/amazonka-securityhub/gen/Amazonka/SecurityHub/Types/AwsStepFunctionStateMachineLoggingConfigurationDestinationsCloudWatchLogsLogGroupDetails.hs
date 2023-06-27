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
-- Module      : Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object describing a CloudWatch log group. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html Amazon Web Services::Logs::LogGroup>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newAwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' smart constructor.
data AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails = AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails'
  { -- | The ARN (ends with @:*@) of the CloudWatch Logs log group to which you
    -- want your logs emitted.
    logGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'awsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails_logGroupArn' - The ARN (ends with @:*@) of the CloudWatch Logs log group to which you
-- want your logs emitted.
newAwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails ::
  AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
newAwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails =
  AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails'
    { logGroupArn =
        Prelude.Nothing
    }

-- | The ARN (ends with @:*@) of the CloudWatch Logs log group to which you
-- want your logs emitted.
awsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails_logGroupArn :: Lens.Lens' AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails_logGroupArn = Lens.lens (\AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' {logGroupArn} -> logGroupArn) (\s@AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' {} a -> s {logGroupArn = a} :: AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails)

instance
  Data.FromJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
  where
  parseJSON =
    Data.withObject
      "AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails"
      ( \x ->
          AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails'
            Prelude.<$> (x Data..:? "LogGroupArn")
      )

instance
  Prelude.Hashable
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
  where
  hashWithSalt
    _salt
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' {..} =
      _salt `Prelude.hashWithSalt` logGroupArn

instance
  Prelude.NFData
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
  where
  rnf
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' {..} =
      Prelude.rnf logGroupArn

instance
  Data.ToJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
  where
  toJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("LogGroupArn" Data..=) Prelude.<$> logGroupArn]
        )
