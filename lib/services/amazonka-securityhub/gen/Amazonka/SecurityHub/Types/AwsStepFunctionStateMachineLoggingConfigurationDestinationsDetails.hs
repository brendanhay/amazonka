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
-- Module      : Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails

-- | An array of objects that describes where your execution history events
-- will be logged.
--
-- /See:/ 'newAwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' smart constructor.
data AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails = AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails'
  { -- | An object describing a CloudWatch Logs log group. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html Amazon Web Services::Logs::LogGroup>
    -- in the /CloudFormation User Guide/.
    cloudWatchLogsLogGroup :: Prelude.Maybe AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroup', 'awsStepFunctionStateMachineLoggingConfigurationDestinationsDetails_cloudWatchLogsLogGroup' - An object describing a CloudWatch Logs log group. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html Amazon Web Services::Logs::LogGroup>
-- in the /CloudFormation User Guide/.
newAwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails ::
  AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
newAwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails =
  AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails'
    { cloudWatchLogsLogGroup =
        Prelude.Nothing
    }

-- | An object describing a CloudWatch Logs log group. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html Amazon Web Services::Logs::LogGroup>
-- in the /CloudFormation User Guide/.
awsStepFunctionStateMachineLoggingConfigurationDestinationsDetails_cloudWatchLogsLogGroup :: Lens.Lens' AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails (Prelude.Maybe AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails)
awsStepFunctionStateMachineLoggingConfigurationDestinationsDetails_cloudWatchLogsLogGroup = Lens.lens (\AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' {cloudWatchLogsLogGroup} -> cloudWatchLogsLogGroup) (\s@AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' {} a -> s {cloudWatchLogsLogGroup = a} :: AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails)

instance
  Data.FromJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
  where
  parseJSON =
    Data.withObject
      "AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails"
      ( \x ->
          AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroup")
      )

instance
  Prelude.Hashable
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
  where
  hashWithSalt
    _salt
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' {..} =
      _salt `Prelude.hashWithSalt` cloudWatchLogsLogGroup

instance
  Prelude.NFData
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
  where
  rnf
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' {..} =
      Prelude.rnf cloudWatchLogsLogGroup

instance
  Data.ToJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
  where
  toJSON
    AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CloudWatchLogsLogGroup" Data..=)
                Prelude.<$> cloudWatchLogsLogGroup
            ]
        )
