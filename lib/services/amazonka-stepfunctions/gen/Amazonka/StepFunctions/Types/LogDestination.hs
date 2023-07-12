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
-- Module      : Amazonka.StepFunctions.Types.LogDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.CloudWatchLogsLogGroup

-- |
--
-- /See:/ 'newLogDestination' smart constructor.
data LogDestination = LogDestination'
  { -- | An object describing a CloudWatch log group. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup>
    -- in the CloudFormation User Guide.
    cloudWatchLogsLogGroup :: Prelude.Maybe CloudWatchLogsLogGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroup', 'logDestination_cloudWatchLogsLogGroup' - An object describing a CloudWatch log group. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup>
-- in the CloudFormation User Guide.
newLogDestination ::
  LogDestination
newLogDestination =
  LogDestination'
    { cloudWatchLogsLogGroup =
        Prelude.Nothing
    }

-- | An object describing a CloudWatch log group. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup>
-- in the CloudFormation User Guide.
logDestination_cloudWatchLogsLogGroup :: Lens.Lens' LogDestination (Prelude.Maybe CloudWatchLogsLogGroup)
logDestination_cloudWatchLogsLogGroup = Lens.lens (\LogDestination' {cloudWatchLogsLogGroup} -> cloudWatchLogsLogGroup) (\s@LogDestination' {} a -> s {cloudWatchLogsLogGroup = a} :: LogDestination)

instance Data.FromJSON LogDestination where
  parseJSON =
    Data.withObject
      "LogDestination"
      ( \x ->
          LogDestination'
            Prelude.<$> (x Data..:? "cloudWatchLogsLogGroup")
      )

instance Prelude.Hashable LogDestination where
  hashWithSalt _salt LogDestination' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogsLogGroup

instance Prelude.NFData LogDestination where
  rnf LogDestination' {..} =
    Prelude.rnf cloudWatchLogsLogGroup

instance Data.ToJSON LogDestination where
  toJSON LogDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogsLogGroup" Data..=)
              Prelude.<$> cloudWatchLogsLogGroup
          ]
      )
