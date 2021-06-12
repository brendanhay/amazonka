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
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newCloudWatchLogsLogGroup' smart constructor.
data CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { -- | The ARN of the the CloudWatch log group to which you want your logs
    -- emitted to. The ARN must end with @:*@
    logGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'cloudWatchLogsLogGroup_logGroupArn' - The ARN of the the CloudWatch log group to which you want your logs
-- emitted to. The ARN must end with @:*@
newCloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
newCloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup' {logGroupArn = Core.Nothing}

-- | The ARN of the the CloudWatch log group to which you want your logs
-- emitted to. The ARN must end with @:*@
cloudWatchLogsLogGroup_logGroupArn :: Lens.Lens' CloudWatchLogsLogGroup (Core.Maybe Core.Text)
cloudWatchLogsLogGroup_logGroupArn = Lens.lens (\CloudWatchLogsLogGroup' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogsLogGroup' {} a -> s {logGroupArn = a} :: CloudWatchLogsLogGroup)

instance Core.FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    Core.withObject
      "CloudWatchLogsLogGroup"
      ( \x ->
          CloudWatchLogsLogGroup'
            Core.<$> (x Core..:? "logGroupArn")
      )

instance Core.Hashable CloudWatchLogsLogGroup

instance Core.NFData CloudWatchLogsLogGroup

instance Core.ToJSON CloudWatchLogsLogGroup where
  toJSON CloudWatchLogsLogGroup' {..} =
    Core.object
      ( Core.catMaybes
          [("logGroupArn" Core..=) Core.<$> logGroupArn]
      )
