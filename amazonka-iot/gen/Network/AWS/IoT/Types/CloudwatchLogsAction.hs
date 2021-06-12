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
-- Module      : Network.AWS.IoT.Types.CloudwatchLogsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchLogsAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action that sends data to CloudWatch Logs.
--
-- /See:/ 'newCloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { -- | The IAM role that allows access to the CloudWatch log.
    roleArn :: Core.Text,
    -- | The CloudWatch log group to which the action sends data.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudwatchLogsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'cloudwatchLogsAction_roleArn' - The IAM role that allows access to the CloudWatch log.
--
-- 'logGroupName', 'cloudwatchLogsAction_logGroupName' - The CloudWatch log group to which the action sends data.
newCloudwatchLogsAction ::
  -- | 'roleArn'
  Core.Text ->
  -- | 'logGroupName'
  Core.Text ->
  CloudwatchLogsAction
newCloudwatchLogsAction pRoleArn_ pLogGroupName_ =
  CloudwatchLogsAction'
    { roleArn = pRoleArn_,
      logGroupName = pLogGroupName_
    }

-- | The IAM role that allows access to the CloudWatch log.
cloudwatchLogsAction_roleArn :: Lens.Lens' CloudwatchLogsAction Core.Text
cloudwatchLogsAction_roleArn = Lens.lens (\CloudwatchLogsAction' {roleArn} -> roleArn) (\s@CloudwatchLogsAction' {} a -> s {roleArn = a} :: CloudwatchLogsAction)

-- | The CloudWatch log group to which the action sends data.
cloudwatchLogsAction_logGroupName :: Lens.Lens' CloudwatchLogsAction Core.Text
cloudwatchLogsAction_logGroupName = Lens.lens (\CloudwatchLogsAction' {logGroupName} -> logGroupName) (\s@CloudwatchLogsAction' {} a -> s {logGroupName = a} :: CloudwatchLogsAction)

instance Core.FromJSON CloudwatchLogsAction where
  parseJSON =
    Core.withObject
      "CloudwatchLogsAction"
      ( \x ->
          CloudwatchLogsAction'
            Core.<$> (x Core..: "roleArn")
            Core.<*> (x Core..: "logGroupName")
      )

instance Core.Hashable CloudwatchLogsAction

instance Core.NFData CloudwatchLogsAction

instance Core.ToJSON CloudwatchLogsAction where
  toJSON CloudwatchLogsAction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("logGroupName" Core..= logGroupName)
          ]
      )
