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
-- Module      : Network.AWS.IoT.Types.CloudwatchLogsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchLogsAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action that sends data to CloudWatch Logs.
--
-- /See:/ 'newCloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { -- | The IAM role that allows access to the CloudWatch log.
    roleArn :: Prelude.Text,
    -- | The CloudWatch log group to which the action sends data.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'logGroupName'
  Prelude.Text ->
  CloudwatchLogsAction
newCloudwatchLogsAction pRoleArn_ pLogGroupName_ =
  CloudwatchLogsAction'
    { roleArn = pRoleArn_,
      logGroupName = pLogGroupName_
    }

-- | The IAM role that allows access to the CloudWatch log.
cloudwatchLogsAction_roleArn :: Lens.Lens' CloudwatchLogsAction Prelude.Text
cloudwatchLogsAction_roleArn = Lens.lens (\CloudwatchLogsAction' {roleArn} -> roleArn) (\s@CloudwatchLogsAction' {} a -> s {roleArn = a} :: CloudwatchLogsAction)

-- | The CloudWatch log group to which the action sends data.
cloudwatchLogsAction_logGroupName :: Lens.Lens' CloudwatchLogsAction Prelude.Text
cloudwatchLogsAction_logGroupName = Lens.lens (\CloudwatchLogsAction' {logGroupName} -> logGroupName) (\s@CloudwatchLogsAction' {} a -> s {logGroupName = a} :: CloudwatchLogsAction)

instance Prelude.FromJSON CloudwatchLogsAction where
  parseJSON =
    Prelude.withObject
      "CloudwatchLogsAction"
      ( \x ->
          CloudwatchLogsAction'
            Prelude.<$> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "logGroupName")
      )

instance Prelude.Hashable CloudwatchLogsAction

instance Prelude.NFData CloudwatchLogsAction

instance Prelude.ToJSON CloudwatchLogsAction where
  toJSON CloudwatchLogsAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just
              ("logGroupName" Prelude..= logGroupName)
          ]
      )
