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
-- Module      : Amazonka.Pipes.Types.PipeTargetCloudWatchLogsParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetCloudWatchLogsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an CloudWatch Logs log stream as a target.
--
-- /See:/ 'newPipeTargetCloudWatchLogsParameters' smart constructor.
data PipeTargetCloudWatchLogsParameters = PipeTargetCloudWatchLogsParameters'
  { -- | The name of the log stream.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | The time the event occurred, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetCloudWatchLogsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'pipeTargetCloudWatchLogsParameters_logStreamName' - The name of the log stream.
--
-- 'timestamp', 'pipeTargetCloudWatchLogsParameters_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
newPipeTargetCloudWatchLogsParameters ::
  PipeTargetCloudWatchLogsParameters
newPipeTargetCloudWatchLogsParameters =
  PipeTargetCloudWatchLogsParameters'
    { logStreamName =
        Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The name of the log stream.
pipeTargetCloudWatchLogsParameters_logStreamName :: Lens.Lens' PipeTargetCloudWatchLogsParameters (Prelude.Maybe Prelude.Text)
pipeTargetCloudWatchLogsParameters_logStreamName = Lens.lens (\PipeTargetCloudWatchLogsParameters' {logStreamName} -> logStreamName) (\s@PipeTargetCloudWatchLogsParameters' {} a -> s {logStreamName = a} :: PipeTargetCloudWatchLogsParameters)

-- | The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
pipeTargetCloudWatchLogsParameters_timestamp :: Lens.Lens' PipeTargetCloudWatchLogsParameters (Prelude.Maybe Prelude.Text)
pipeTargetCloudWatchLogsParameters_timestamp = Lens.lens (\PipeTargetCloudWatchLogsParameters' {timestamp} -> timestamp) (\s@PipeTargetCloudWatchLogsParameters' {} a -> s {timestamp = a} :: PipeTargetCloudWatchLogsParameters)

instance
  Data.FromJSON
    PipeTargetCloudWatchLogsParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetCloudWatchLogsParameters"
      ( \x ->
          PipeTargetCloudWatchLogsParameters'
            Prelude.<$> (x Data..:? "LogStreamName")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance
  Prelude.Hashable
    PipeTargetCloudWatchLogsParameters
  where
  hashWithSalt
    _salt
    PipeTargetCloudWatchLogsParameters' {..} =
      _salt `Prelude.hashWithSalt` logStreamName
        `Prelude.hashWithSalt` timestamp

instance
  Prelude.NFData
    PipeTargetCloudWatchLogsParameters
  where
  rnf PipeTargetCloudWatchLogsParameters' {..} =
    Prelude.rnf logStreamName
      `Prelude.seq` Prelude.rnf timestamp

instance
  Data.ToJSON
    PipeTargetCloudWatchLogsParameters
  where
  toJSON PipeTargetCloudWatchLogsParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogStreamName" Data..=) Prelude.<$> logStreamName,
            ("Timestamp" Data..=) Prelude.<$> timestamp
          ]
      )
