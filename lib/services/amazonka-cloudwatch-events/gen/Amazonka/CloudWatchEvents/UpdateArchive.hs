{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.UpdateArchive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified archive.
module Amazonka.CloudWatchEvents.UpdateArchive
  ( -- * Creating a Request
    UpdateArchive (..),
    newUpdateArchive,

    -- * Request Lenses
    updateArchive_description,
    updateArchive_eventPattern,
    updateArchive_retentionDays,
    updateArchive_archiveName,

    -- * Destructuring the Response
    UpdateArchiveResponse (..),
    newUpdateArchiveResponse,

    -- * Response Lenses
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_creationTime,
    updateArchiveResponse_state,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateArchive' smart constructor.
data UpdateArchive = UpdateArchive'
  { -- | The description for the archive.
    description :: Prelude.Maybe Prelude.Text,
    -- | The event pattern to use to filter events sent to the archive.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain events in the archive.
    retentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The name of the archive to update.
    archiveName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateArchive_description' - The description for the archive.
--
-- 'eventPattern', 'updateArchive_eventPattern' - The event pattern to use to filter events sent to the archive.
--
-- 'retentionDays', 'updateArchive_retentionDays' - The number of days to retain events in the archive.
--
-- 'archiveName', 'updateArchive_archiveName' - The name of the archive to update.
newUpdateArchive ::
  -- | 'archiveName'
  Prelude.Text ->
  UpdateArchive
newUpdateArchive pArchiveName_ =
  UpdateArchive'
    { description = Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      retentionDays = Prelude.Nothing,
      archiveName = pArchiveName_
    }

-- | The description for the archive.
updateArchive_description :: Lens.Lens' UpdateArchive (Prelude.Maybe Prelude.Text)
updateArchive_description = Lens.lens (\UpdateArchive' {description} -> description) (\s@UpdateArchive' {} a -> s {description = a} :: UpdateArchive)

-- | The event pattern to use to filter events sent to the archive.
updateArchive_eventPattern :: Lens.Lens' UpdateArchive (Prelude.Maybe Prelude.Text)
updateArchive_eventPattern = Lens.lens (\UpdateArchive' {eventPattern} -> eventPattern) (\s@UpdateArchive' {} a -> s {eventPattern = a} :: UpdateArchive)

-- | The number of days to retain events in the archive.
updateArchive_retentionDays :: Lens.Lens' UpdateArchive (Prelude.Maybe Prelude.Natural)
updateArchive_retentionDays = Lens.lens (\UpdateArchive' {retentionDays} -> retentionDays) (\s@UpdateArchive' {} a -> s {retentionDays = a} :: UpdateArchive)

-- | The name of the archive to update.
updateArchive_archiveName :: Lens.Lens' UpdateArchive Prelude.Text
updateArchive_archiveName = Lens.lens (\UpdateArchive' {archiveName} -> archiveName) (\s@UpdateArchive' {} a -> s {archiveName = a} :: UpdateArchive)

instance Core.AWSRequest UpdateArchive where
  type
    AWSResponse UpdateArchive =
      UpdateArchiveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateArchiveResponse'
            Prelude.<$> (x Data..?> "ArchiveArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateArchive where
  hashWithSalt _salt UpdateArchive' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` retentionDays
      `Prelude.hashWithSalt` archiveName

instance Prelude.NFData UpdateArchive where
  rnf UpdateArchive' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf eventPattern `Prelude.seq`
        Prelude.rnf retentionDays `Prelude.seq`
          Prelude.rnf archiveName

instance Data.ToHeaders UpdateArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.UpdateArchive" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateArchive where
  toJSON UpdateArchive' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EventPattern" Data..=) Prelude.<$> eventPattern,
            ("RetentionDays" Data..=) Prelude.<$> retentionDays,
            Prelude.Just ("ArchiveName" Data..= archiveName)
          ]
      )

instance Data.ToPath UpdateArchive where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateArchive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateArchiveResponse' smart constructor.
data UpdateArchiveResponse = UpdateArchiveResponse'
  { -- | The ARN of the archive.
    archiveArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the archive was updated.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the archive.
    state :: Prelude.Maybe ArchiveState,
    -- | The reason that the archive is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveArn', 'updateArchiveResponse_archiveArn' - The ARN of the archive.
--
-- 'creationTime', 'updateArchiveResponse_creationTime' - The time at which the archive was updated.
--
-- 'state', 'updateArchiveResponse_state' - The state of the archive.
--
-- 'stateReason', 'updateArchiveResponse_stateReason' - The reason that the archive is in the current state.
--
-- 'httpStatus', 'updateArchiveResponse_httpStatus' - The response's http status code.
newUpdateArchiveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateArchiveResponse
newUpdateArchiveResponse pHttpStatus_ =
  UpdateArchiveResponse'
    { archiveArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the archive.
updateArchiveResponse_archiveArn :: Lens.Lens' UpdateArchiveResponse (Prelude.Maybe Prelude.Text)
updateArchiveResponse_archiveArn = Lens.lens (\UpdateArchiveResponse' {archiveArn} -> archiveArn) (\s@UpdateArchiveResponse' {} a -> s {archiveArn = a} :: UpdateArchiveResponse)

-- | The time at which the archive was updated.
updateArchiveResponse_creationTime :: Lens.Lens' UpdateArchiveResponse (Prelude.Maybe Prelude.UTCTime)
updateArchiveResponse_creationTime = Lens.lens (\UpdateArchiveResponse' {creationTime} -> creationTime) (\s@UpdateArchiveResponse' {} a -> s {creationTime = a} :: UpdateArchiveResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the archive.
updateArchiveResponse_state :: Lens.Lens' UpdateArchiveResponse (Prelude.Maybe ArchiveState)
updateArchiveResponse_state = Lens.lens (\UpdateArchiveResponse' {state} -> state) (\s@UpdateArchiveResponse' {} a -> s {state = a} :: UpdateArchiveResponse)

-- | The reason that the archive is in the current state.
updateArchiveResponse_stateReason :: Lens.Lens' UpdateArchiveResponse (Prelude.Maybe Prelude.Text)
updateArchiveResponse_stateReason = Lens.lens (\UpdateArchiveResponse' {stateReason} -> stateReason) (\s@UpdateArchiveResponse' {} a -> s {stateReason = a} :: UpdateArchiveResponse)

-- | The response's http status code.
updateArchiveResponse_httpStatus :: Lens.Lens' UpdateArchiveResponse Prelude.Int
updateArchiveResponse_httpStatus = Lens.lens (\UpdateArchiveResponse' {httpStatus} -> httpStatus) (\s@UpdateArchiveResponse' {} a -> s {httpStatus = a} :: UpdateArchiveResponse)

instance Prelude.NFData UpdateArchiveResponse where
  rnf UpdateArchiveResponse' {..} =
    Prelude.rnf archiveArn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf state `Prelude.seq`
          Prelude.rnf stateReason `Prelude.seq`
            Prelude.rnf httpStatus
