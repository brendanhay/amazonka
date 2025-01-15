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
-- Module      : Amazonka.Firehose.Types.CloudWatchLoggingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.CloudWatchLoggingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon CloudWatch logging options for your delivery
-- stream.
--
-- /See:/ 'newCloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { -- | Enables or disables CloudWatch logging.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The CloudWatch group name for logging. This value is required if
    -- CloudWatch logging is enabled.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch log stream name for logging. This value is required if
    -- CloudWatch logging is enabled.
    logStreamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'cloudWatchLoggingOptions_enabled' - Enables or disables CloudWatch logging.
--
-- 'logGroupName', 'cloudWatchLoggingOptions_logGroupName' - The CloudWatch group name for logging. This value is required if
-- CloudWatch logging is enabled.
--
-- 'logStreamName', 'cloudWatchLoggingOptions_logStreamName' - The CloudWatch log stream name for logging. This value is required if
-- CloudWatch logging is enabled.
newCloudWatchLoggingOptions ::
  CloudWatchLoggingOptions
newCloudWatchLoggingOptions =
  CloudWatchLoggingOptions'
    { enabled =
        Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      logStreamName = Prelude.Nothing
    }

-- | Enables or disables CloudWatch logging.
cloudWatchLoggingOptions_enabled :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Bool)
cloudWatchLoggingOptions_enabled = Lens.lens (\CloudWatchLoggingOptions' {enabled} -> enabled) (\s@CloudWatchLoggingOptions' {} a -> s {enabled = a} :: CloudWatchLoggingOptions)

-- | The CloudWatch group name for logging. This value is required if
-- CloudWatch logging is enabled.
cloudWatchLoggingOptions_logGroupName :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptions_logGroupName = Lens.lens (\CloudWatchLoggingOptions' {logGroupName} -> logGroupName) (\s@CloudWatchLoggingOptions' {} a -> s {logGroupName = a} :: CloudWatchLoggingOptions)

-- | The CloudWatch log stream name for logging. This value is required if
-- CloudWatch logging is enabled.
cloudWatchLoggingOptions_logStreamName :: Lens.Lens' CloudWatchLoggingOptions (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptions_logStreamName = Lens.lens (\CloudWatchLoggingOptions' {logStreamName} -> logStreamName) (\s@CloudWatchLoggingOptions' {} a -> s {logStreamName = a} :: CloudWatchLoggingOptions)

instance Data.FromJSON CloudWatchLoggingOptions where
  parseJSON =
    Data.withObject
      "CloudWatchLoggingOptions"
      ( \x ->
          CloudWatchLoggingOptions'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "LogStreamName")
      )

instance Prelude.Hashable CloudWatchLoggingOptions where
  hashWithSalt _salt CloudWatchLoggingOptions' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logStreamName

instance Prelude.NFData CloudWatchLoggingOptions where
  rnf CloudWatchLoggingOptions' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf logGroupName `Prelude.seq`
        Prelude.rnf logStreamName

instance Data.ToJSON CloudWatchLoggingOptions where
  toJSON CloudWatchLoggingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("LogGroupName" Data..=) Prelude.<$> logGroupName,
            ("LogStreamName" Data..=) Prelude.<$> logStreamName
          ]
      )
