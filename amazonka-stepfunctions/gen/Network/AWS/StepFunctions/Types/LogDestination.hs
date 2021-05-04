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
-- Module      : Network.AWS.StepFunctions.Types.LogDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup

-- |
--
-- /See:/ 'newLogDestination' smart constructor.
data LogDestination = LogDestination'
  { -- | An object describing a CloudWatch log group. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup>
    -- in the AWS CloudFormation User Guide.
    cloudWatchLogsLogGroup :: Prelude.Maybe CloudWatchLogsLogGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- in the AWS CloudFormation User Guide.
newLogDestination ::
  LogDestination
newLogDestination =
  LogDestination'
    { cloudWatchLogsLogGroup =
        Prelude.Nothing
    }

-- | An object describing a CloudWatch log group. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup>
-- in the AWS CloudFormation User Guide.
logDestination_cloudWatchLogsLogGroup :: Lens.Lens' LogDestination (Prelude.Maybe CloudWatchLogsLogGroup)
logDestination_cloudWatchLogsLogGroup = Lens.lens (\LogDestination' {cloudWatchLogsLogGroup} -> cloudWatchLogsLogGroup) (\s@LogDestination' {} a -> s {cloudWatchLogsLogGroup = a} :: LogDestination)

instance Prelude.FromJSON LogDestination where
  parseJSON =
    Prelude.withObject
      "LogDestination"
      ( \x ->
          LogDestination'
            Prelude.<$> (x Prelude..:? "cloudWatchLogsLogGroup")
      )

instance Prelude.Hashable LogDestination

instance Prelude.NFData LogDestination

instance Prelude.ToJSON LogDestination where
  toJSON LogDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogsLogGroup" Prelude..=)
              Prelude.<$> cloudWatchLogsLogGroup
          ]
      )
