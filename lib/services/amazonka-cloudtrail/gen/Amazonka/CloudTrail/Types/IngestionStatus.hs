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
-- Module      : Amazonka.CloudTrail.Types.IngestionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.IngestionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A table showing information about the most recent successful and failed
-- attempts to ingest events.
--
-- /See:/ 'newIngestionStatus' smart constructor.
data IngestionStatus = IngestionStatus'
  { -- | The event ID of the most recent attempt to ingest events.
    latestIngestionAttemptEventID :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the most recent attempt to ingest events on the
    -- channel.
    latestIngestionAttemptTime :: Prelude.Maybe Data.POSIX,
    -- | The error code for the most recent failure to ingest events.
    latestIngestionErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The event ID of the most recent successful ingestion of events.
    latestIngestionSuccessEventID :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the most recent successful ingestion of events for the
    -- channel.
    latestIngestionSuccessTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestIngestionAttemptEventID', 'ingestionStatus_latestIngestionAttemptEventID' - The event ID of the most recent attempt to ingest events.
--
-- 'latestIngestionAttemptTime', 'ingestionStatus_latestIngestionAttemptTime' - The time stamp of the most recent attempt to ingest events on the
-- channel.
--
-- 'latestIngestionErrorCode', 'ingestionStatus_latestIngestionErrorCode' - The error code for the most recent failure to ingest events.
--
-- 'latestIngestionSuccessEventID', 'ingestionStatus_latestIngestionSuccessEventID' - The event ID of the most recent successful ingestion of events.
--
-- 'latestIngestionSuccessTime', 'ingestionStatus_latestIngestionSuccessTime' - The time stamp of the most recent successful ingestion of events for the
-- channel.
newIngestionStatus ::
  IngestionStatus
newIngestionStatus =
  IngestionStatus'
    { latestIngestionAttemptEventID =
        Prelude.Nothing,
      latestIngestionAttemptTime = Prelude.Nothing,
      latestIngestionErrorCode = Prelude.Nothing,
      latestIngestionSuccessEventID = Prelude.Nothing,
      latestIngestionSuccessTime = Prelude.Nothing
    }

-- | The event ID of the most recent attempt to ingest events.
ingestionStatus_latestIngestionAttemptEventID :: Lens.Lens' IngestionStatus (Prelude.Maybe Prelude.Text)
ingestionStatus_latestIngestionAttemptEventID = Lens.lens (\IngestionStatus' {latestIngestionAttemptEventID} -> latestIngestionAttemptEventID) (\s@IngestionStatus' {} a -> s {latestIngestionAttemptEventID = a} :: IngestionStatus)

-- | The time stamp of the most recent attempt to ingest events on the
-- channel.
ingestionStatus_latestIngestionAttemptTime :: Lens.Lens' IngestionStatus (Prelude.Maybe Prelude.UTCTime)
ingestionStatus_latestIngestionAttemptTime = Lens.lens (\IngestionStatus' {latestIngestionAttemptTime} -> latestIngestionAttemptTime) (\s@IngestionStatus' {} a -> s {latestIngestionAttemptTime = a} :: IngestionStatus) Prelude.. Lens.mapping Data._Time

-- | The error code for the most recent failure to ingest events.
ingestionStatus_latestIngestionErrorCode :: Lens.Lens' IngestionStatus (Prelude.Maybe Prelude.Text)
ingestionStatus_latestIngestionErrorCode = Lens.lens (\IngestionStatus' {latestIngestionErrorCode} -> latestIngestionErrorCode) (\s@IngestionStatus' {} a -> s {latestIngestionErrorCode = a} :: IngestionStatus)

-- | The event ID of the most recent successful ingestion of events.
ingestionStatus_latestIngestionSuccessEventID :: Lens.Lens' IngestionStatus (Prelude.Maybe Prelude.Text)
ingestionStatus_latestIngestionSuccessEventID = Lens.lens (\IngestionStatus' {latestIngestionSuccessEventID} -> latestIngestionSuccessEventID) (\s@IngestionStatus' {} a -> s {latestIngestionSuccessEventID = a} :: IngestionStatus)

-- | The time stamp of the most recent successful ingestion of events for the
-- channel.
ingestionStatus_latestIngestionSuccessTime :: Lens.Lens' IngestionStatus (Prelude.Maybe Prelude.UTCTime)
ingestionStatus_latestIngestionSuccessTime = Lens.lens (\IngestionStatus' {latestIngestionSuccessTime} -> latestIngestionSuccessTime) (\s@IngestionStatus' {} a -> s {latestIngestionSuccessTime = a} :: IngestionStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON IngestionStatus where
  parseJSON =
    Data.withObject
      "IngestionStatus"
      ( \x ->
          IngestionStatus'
            Prelude.<$> (x Data..:? "LatestIngestionAttemptEventID")
            Prelude.<*> (x Data..:? "LatestIngestionAttemptTime")
            Prelude.<*> (x Data..:? "LatestIngestionErrorCode")
            Prelude.<*> (x Data..:? "LatestIngestionSuccessEventID")
            Prelude.<*> (x Data..:? "LatestIngestionSuccessTime")
      )

instance Prelude.Hashable IngestionStatus where
  hashWithSalt _salt IngestionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` latestIngestionAttemptEventID
      `Prelude.hashWithSalt` latestIngestionAttemptTime
      `Prelude.hashWithSalt` latestIngestionErrorCode
      `Prelude.hashWithSalt` latestIngestionSuccessEventID
      `Prelude.hashWithSalt` latestIngestionSuccessTime

instance Prelude.NFData IngestionStatus where
  rnf IngestionStatus' {..} =
    Prelude.rnf latestIngestionAttemptEventID
      `Prelude.seq` Prelude.rnf latestIngestionAttemptTime
      `Prelude.seq` Prelude.rnf latestIngestionErrorCode
      `Prelude.seq` Prelude.rnf latestIngestionSuccessEventID
      `Prelude.seq` Prelude.rnf latestIngestionSuccessTime
