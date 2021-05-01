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
-- Module      : Network.AWS.SSM.Types.CloudWatchOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CloudWatchOutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration options for sending command output to CloudWatch Logs.
--
-- /See:/ 'newCloudWatchOutputConfig' smart constructor.
data CloudWatchOutputConfig = CloudWatchOutputConfig'
  { -- | The name of the CloudWatch log group where you want to send command
    -- output. If you don\'t specify a group name, Systems Manager
    -- automatically creates a log group for you. The log group uses the
    -- following naming format: aws\/ssm\//SystemsManagerDocumentName/.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text,
    -- | Enables Systems Manager to send command output to CloudWatch Logs.
    cloudWatchOutputEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      cloudWatchOutputEnabled = Prelude.Nothing
    }

-- | The name of the CloudWatch log group where you want to send command
-- output. If you don\'t specify a group name, Systems Manager
-- automatically creates a log group for you. The log group uses the
-- following naming format: aws\/ssm\//SystemsManagerDocumentName/.
cloudWatchOutputConfig_cloudWatchLogGroupName :: Lens.Lens' CloudWatchOutputConfig (Prelude.Maybe Prelude.Text)
cloudWatchOutputConfig_cloudWatchLogGroupName = Lens.lens (\CloudWatchOutputConfig' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@CloudWatchOutputConfig' {} a -> s {cloudWatchLogGroupName = a} :: CloudWatchOutputConfig)

-- | Enables Systems Manager to send command output to CloudWatch Logs.
cloudWatchOutputConfig_cloudWatchOutputEnabled :: Lens.Lens' CloudWatchOutputConfig (Prelude.Maybe Prelude.Bool)
cloudWatchOutputConfig_cloudWatchOutputEnabled = Lens.lens (\CloudWatchOutputConfig' {cloudWatchOutputEnabled} -> cloudWatchOutputEnabled) (\s@CloudWatchOutputConfig' {} a -> s {cloudWatchOutputEnabled = a} :: CloudWatchOutputConfig)

instance Prelude.FromJSON CloudWatchOutputConfig where
  parseJSON =
    Prelude.withObject
      "CloudWatchOutputConfig"
      ( \x ->
          CloudWatchOutputConfig'
            Prelude.<$> (x Prelude..:? "CloudWatchLogGroupName")
            Prelude.<*> (x Prelude..:? "CloudWatchOutputEnabled")
      )

instance Prelude.Hashable CloudWatchOutputConfig

instance Prelude.NFData CloudWatchOutputConfig

instance Prelude.ToJSON CloudWatchOutputConfig where
  toJSON CloudWatchOutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogGroupName" Prelude..=)
              Prelude.<$> cloudWatchLogGroupName,
            ("CloudWatchOutputEnabled" Prelude..=)
              Prelude.<$> cloudWatchOutputEnabled
          ]
      )
