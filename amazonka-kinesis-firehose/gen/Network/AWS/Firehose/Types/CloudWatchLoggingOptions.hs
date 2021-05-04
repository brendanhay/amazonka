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
-- Module      : Network.AWS.Firehose.Types.CloudWatchLoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CloudWatchLoggingOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon CloudWatch logging options for your delivery
-- stream.
--
-- /See:/ 'newCloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { -- | The CloudWatch log stream name for logging. This value is required if
    -- CloudWatch logging is enabled.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables CloudWatch logging.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The CloudWatch group name for logging. This value is required if
    -- CloudWatch logging is enabled.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'cloudWatchLoggingOptions_logStreamName' - The CloudWatch log stream name for logging. This value is required if
-- CloudWatch logging is enabled.
--
-- 'enabled', 'cloudWatchLoggingOptions_enabled' - Enables or disables CloudWatch logging.
--
-- 'logGroupName', 'cloudWatchLoggingOptions_logGroupName' - The CloudWatch group name for logging. This value is required if
-- CloudWatch logging is enabled.
newCloudWatchLoggingOptions ::
  CloudWatchLoggingOptions
newCloudWatchLoggingOptions =
  CloudWatchLoggingOptions'
    { logStreamName =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      logGroupName = Prelude.Nothing
    }

-- | The CloudWatch log stream name for logging. This value is required if
-- CloudWatch logging is enabled.
cloudWatchLoggingOptions_logStreamName :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptions_logStreamName = Lens.lens (\CloudWatchLoggingOptions' {logStreamName} -> logStreamName) (\s@CloudWatchLoggingOptions' {} a -> s {logStreamName = a} :: CloudWatchLoggingOptions)

-- | Enables or disables CloudWatch logging.
cloudWatchLoggingOptions_enabled :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Bool)
cloudWatchLoggingOptions_enabled = Lens.lens (\CloudWatchLoggingOptions' {enabled} -> enabled) (\s@CloudWatchLoggingOptions' {} a -> s {enabled = a} :: CloudWatchLoggingOptions)

-- | The CloudWatch group name for logging. This value is required if
-- CloudWatch logging is enabled.
cloudWatchLoggingOptions_logGroupName :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptions_logGroupName = Lens.lens (\CloudWatchLoggingOptions' {logGroupName} -> logGroupName) (\s@CloudWatchLoggingOptions' {} a -> s {logGroupName = a} :: CloudWatchLoggingOptions)

instance Prelude.FromJSON CloudWatchLoggingOptions where
  parseJSON =
    Prelude.withObject
      "CloudWatchLoggingOptions"
      ( \x ->
          CloudWatchLoggingOptions'
            Prelude.<$> (x Prelude..:? "LogStreamName")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "LogGroupName")
      )

instance Prelude.Hashable CloudWatchLoggingOptions

instance Prelude.NFData CloudWatchLoggingOptions

instance Prelude.ToJSON CloudWatchLoggingOptions where
  toJSON CloudWatchLoggingOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LogStreamName" Prelude..=)
              Prelude.<$> logStreamName,
            ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("LogGroupName" Prelude..=)
              Prelude.<$> logGroupName
          ]
      )
