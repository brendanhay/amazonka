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
-- Module      : Amazonka.MediaTailor.Types.LogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.LogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns Amazon CloudWatch log settings for a playback configuration.
--
-- /See:/ 'newLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { -- | The percentage of session logs that MediaTailor sends to your Cloudwatch
    -- Logs account. For example, if your playback configuration has 1000
    -- sessions and @percentEnabled@ is set to @60@, MediaTailor sends logs for
    -- 600 of the sessions to CloudWatch Logs. MediaTailor decides at random
    -- which of the playback configuration sessions to send logs for. If you
    -- want to view logs for a specific session, you can use the
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/debug-log-mode.html debug log mode>.
    --
    -- Valid values: @0@ - @100@
    percentEnabled :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentEnabled', 'logConfiguration_percentEnabled' - The percentage of session logs that MediaTailor sends to your Cloudwatch
-- Logs account. For example, if your playback configuration has 1000
-- sessions and @percentEnabled@ is set to @60@, MediaTailor sends logs for
-- 600 of the sessions to CloudWatch Logs. MediaTailor decides at random
-- which of the playback configuration sessions to send logs for. If you
-- want to view logs for a specific session, you can use the
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/debug-log-mode.html debug log mode>.
--
-- Valid values: @0@ - @100@
newLogConfiguration ::
  -- | 'percentEnabled'
  Prelude.Int ->
  LogConfiguration
newLogConfiguration pPercentEnabled_ =
  LogConfiguration'
    { percentEnabled =
        pPercentEnabled_
    }

-- | The percentage of session logs that MediaTailor sends to your Cloudwatch
-- Logs account. For example, if your playback configuration has 1000
-- sessions and @percentEnabled@ is set to @60@, MediaTailor sends logs for
-- 600 of the sessions to CloudWatch Logs. MediaTailor decides at random
-- which of the playback configuration sessions to send logs for. If you
-- want to view logs for a specific session, you can use the
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/debug-log-mode.html debug log mode>.
--
-- Valid values: @0@ - @100@
logConfiguration_percentEnabled :: Lens.Lens' LogConfiguration Prelude.Int
logConfiguration_percentEnabled = Lens.lens (\LogConfiguration' {percentEnabled} -> percentEnabled) (\s@LogConfiguration' {} a -> s {percentEnabled = a} :: LogConfiguration)

instance Data.FromJSON LogConfiguration where
  parseJSON =
    Data.withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration'
            Prelude.<$> (x Data..: "PercentEnabled")
      )

instance Prelude.Hashable LogConfiguration where
  hashWithSalt _salt LogConfiguration' {..} =
    _salt `Prelude.hashWithSalt` percentEnabled

instance Prelude.NFData LogConfiguration where
  rnf LogConfiguration' {..} =
    Prelude.rnf percentEnabled
