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
-- Module      : Network.AWS.SSM.Types.CloudWatchOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CloudWatchOutputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration options for sending command output to CloudWatch Logs.
--
-- /See:/ 'newCloudWatchOutputConfig' smart constructor.
data CloudWatchOutputConfig = CloudWatchOutputConfig'
  { -- | The name of the CloudWatch log group where you want to send command
    -- output. If you don\'t specify a group name, Systems Manager
    -- automatically creates a log group for you. The log group uses the
    -- following naming format: aws\/ssm\//SystemsManagerDocumentName/.
    cloudWatchLogGroupName :: Core.Maybe Core.Text,
    -- | Enables Systems Manager to send command output to CloudWatch Logs.
    cloudWatchOutputEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudWatchOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupName', 'cloudWatchOutputConfig_cloudWatchLogGroupName' - The name of the CloudWatch log group where you want to send command
-- output. If you don\'t specify a group name, Systems Manager
-- automatically creates a log group for you. The log group uses the
-- following naming format: aws\/ssm\//SystemsManagerDocumentName/.
--
-- 'cloudWatchOutputEnabled', 'cloudWatchOutputConfig_cloudWatchOutputEnabled' - Enables Systems Manager to send command output to CloudWatch Logs.
newCloudWatchOutputConfig ::
  CloudWatchOutputConfig
newCloudWatchOutputConfig =
  CloudWatchOutputConfig'
    { cloudWatchLogGroupName =
        Core.Nothing,
      cloudWatchOutputEnabled = Core.Nothing
    }

-- | The name of the CloudWatch log group where you want to send command
-- output. If you don\'t specify a group name, Systems Manager
-- automatically creates a log group for you. The log group uses the
-- following naming format: aws\/ssm\//SystemsManagerDocumentName/.
cloudWatchOutputConfig_cloudWatchLogGroupName :: Lens.Lens' CloudWatchOutputConfig (Core.Maybe Core.Text)
cloudWatchOutputConfig_cloudWatchLogGroupName = Lens.lens (\CloudWatchOutputConfig' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@CloudWatchOutputConfig' {} a -> s {cloudWatchLogGroupName = a} :: CloudWatchOutputConfig)

-- | Enables Systems Manager to send command output to CloudWatch Logs.
cloudWatchOutputConfig_cloudWatchOutputEnabled :: Lens.Lens' CloudWatchOutputConfig (Core.Maybe Core.Bool)
cloudWatchOutputConfig_cloudWatchOutputEnabled = Lens.lens (\CloudWatchOutputConfig' {cloudWatchOutputEnabled} -> cloudWatchOutputEnabled) (\s@CloudWatchOutputConfig' {} a -> s {cloudWatchOutputEnabled = a} :: CloudWatchOutputConfig)

instance Core.FromJSON CloudWatchOutputConfig where
  parseJSON =
    Core.withObject
      "CloudWatchOutputConfig"
      ( \x ->
          CloudWatchOutputConfig'
            Core.<$> (x Core..:? "CloudWatchLogGroupName")
            Core.<*> (x Core..:? "CloudWatchOutputEnabled")
      )

instance Core.Hashable CloudWatchOutputConfig

instance Core.NFData CloudWatchOutputConfig

instance Core.ToJSON CloudWatchOutputConfig where
  toJSON CloudWatchOutputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchLogGroupName" Core..=)
              Core.<$> cloudWatchLogGroupName,
            ("CloudWatchOutputEnabled" Core..=)
              Core.<$> cloudWatchOutputEnabled
          ]
      )
