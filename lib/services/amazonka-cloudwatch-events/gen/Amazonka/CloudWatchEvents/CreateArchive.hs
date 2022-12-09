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
-- Module      : Amazonka.CloudWatchEvents.CreateArchive
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an archive of events with the specified settings. When you
-- create an archive, incoming events might not immediately start being
-- sent to the archive. Allow a short period of time for changes to take
-- effect. If you do not specify a pattern to filter events sent to the
-- archive, all events are sent to the archive except replayed events.
-- Replayed events are not sent to an archive.
module Amazonka.CloudWatchEvents.CreateArchive
  ( -- * Creating a Request
    CreateArchive (..),
    newCreateArchive,

    -- * Request Lenses
    createArchive_description,
    createArchive_eventPattern,
    createArchive_retentionDays,
    createArchive_archiveName,
    createArchive_eventSourceArn,

    -- * Destructuring the Response
    CreateArchiveResponse (..),
    newCreateArchiveResponse,

    -- * Response Lenses
    createArchiveResponse_archiveArn,
    createArchiveResponse_creationTime,
    createArchiveResponse_state,
    createArchiveResponse_stateReason,
    createArchiveResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateArchive' smart constructor.
data CreateArchive = CreateArchive'
  { -- | A description for the archive.
    description :: Prelude.Maybe Prelude.Text,
    -- | An event pattern to use to filter events sent to the archive.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain events for. Default value is 0. If set to
    -- 0, events are retained indefinitely
    retentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The name for the archive to create.
    archiveName :: Prelude.Text,
    -- | The ARN of the event bus that sends events to the archive.
    eventSourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createArchive_description' - A description for the archive.
--
-- 'eventPattern', 'createArchive_eventPattern' - An event pattern to use to filter events sent to the archive.
--
-- 'retentionDays', 'createArchive_retentionDays' - The number of days to retain events for. Default value is 0. If set to
-- 0, events are retained indefinitely
--
-- 'archiveName', 'createArchive_archiveName' - The name for the archive to create.
--
-- 'eventSourceArn', 'createArchive_eventSourceArn' - The ARN of the event bus that sends events to the archive.
newCreateArchive ::
  -- | 'archiveName'
  Prelude.Text ->
  -- | 'eventSourceArn'
  Prelude.Text ->
  CreateArchive
newCreateArchive pArchiveName_ pEventSourceArn_ =
  CreateArchive'
    { description = Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      retentionDays = Prelude.Nothing,
      archiveName = pArchiveName_,
      eventSourceArn = pEventSourceArn_
    }

-- | A description for the archive.
createArchive_description :: Lens.Lens' CreateArchive (Prelude.Maybe Prelude.Text)
createArchive_description = Lens.lens (\CreateArchive' {description} -> description) (\s@CreateArchive' {} a -> s {description = a} :: CreateArchive)

-- | An event pattern to use to filter events sent to the archive.
createArchive_eventPattern :: Lens.Lens' CreateArchive (Prelude.Maybe Prelude.Text)
createArchive_eventPattern = Lens.lens (\CreateArchive' {eventPattern} -> eventPattern) (\s@CreateArchive' {} a -> s {eventPattern = a} :: CreateArchive)

-- | The number of days to retain events for. Default value is 0. If set to
-- 0, events are retained indefinitely
createArchive_retentionDays :: Lens.Lens' CreateArchive (Prelude.Maybe Prelude.Natural)
createArchive_retentionDays = Lens.lens (\CreateArchive' {retentionDays} -> retentionDays) (\s@CreateArchive' {} a -> s {retentionDays = a} :: CreateArchive)

-- | The name for the archive to create.
createArchive_archiveName :: Lens.Lens' CreateArchive Prelude.Text
createArchive_archiveName = Lens.lens (\CreateArchive' {archiveName} -> archiveName) (\s@CreateArchive' {} a -> s {archiveName = a} :: CreateArchive)

-- | The ARN of the event bus that sends events to the archive.
createArchive_eventSourceArn :: Lens.Lens' CreateArchive Prelude.Text
createArchive_eventSourceArn = Lens.lens (\CreateArchive' {eventSourceArn} -> eventSourceArn) (\s@CreateArchive' {} a -> s {eventSourceArn = a} :: CreateArchive)

instance Core.AWSRequest CreateArchive where
  type
    AWSResponse CreateArchive =
      CreateArchiveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateArchiveResponse'
            Prelude.<$> (x Data..?> "ArchiveArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateArchive where
  hashWithSalt _salt CreateArchive' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` retentionDays
      `Prelude.hashWithSalt` archiveName
      `Prelude.hashWithSalt` eventSourceArn

instance Prelude.NFData CreateArchive where
  rnf CreateArchive' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventPattern
      `Prelude.seq` Prelude.rnf retentionDays
      `Prelude.seq` Prelude.rnf archiveName
      `Prelude.seq` Prelude.rnf eventSourceArn

instance Data.ToHeaders CreateArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.CreateArchive" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateArchive where
  toJSON CreateArchive' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EventPattern" Data..=) Prelude.<$> eventPattern,
            ("RetentionDays" Data..=) Prelude.<$> retentionDays,
            Prelude.Just ("ArchiveName" Data..= archiveName),
            Prelude.Just
              ("EventSourceArn" Data..= eventSourceArn)
          ]
      )

instance Data.ToPath CreateArchive where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateArchive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { -- | The ARN of the archive that was created.
    archiveArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the archive was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the archive that was created.
    state :: Prelude.Maybe ArchiveState,
    -- | The reason that the archive is in the state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveArn', 'createArchiveResponse_archiveArn' - The ARN of the archive that was created.
--
-- 'creationTime', 'createArchiveResponse_creationTime' - The time at which the archive was created.
--
-- 'state', 'createArchiveResponse_state' - The state of the archive that was created.
--
-- 'stateReason', 'createArchiveResponse_stateReason' - The reason that the archive is in the state.
--
-- 'httpStatus', 'createArchiveResponse_httpStatus' - The response's http status code.
newCreateArchiveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateArchiveResponse
newCreateArchiveResponse pHttpStatus_ =
  CreateArchiveResponse'
    { archiveArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the archive that was created.
createArchiveResponse_archiveArn :: Lens.Lens' CreateArchiveResponse (Prelude.Maybe Prelude.Text)
createArchiveResponse_archiveArn = Lens.lens (\CreateArchiveResponse' {archiveArn} -> archiveArn) (\s@CreateArchiveResponse' {} a -> s {archiveArn = a} :: CreateArchiveResponse)

-- | The time at which the archive was created.
createArchiveResponse_creationTime :: Lens.Lens' CreateArchiveResponse (Prelude.Maybe Prelude.UTCTime)
createArchiveResponse_creationTime = Lens.lens (\CreateArchiveResponse' {creationTime} -> creationTime) (\s@CreateArchiveResponse' {} a -> s {creationTime = a} :: CreateArchiveResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the archive that was created.
createArchiveResponse_state :: Lens.Lens' CreateArchiveResponse (Prelude.Maybe ArchiveState)
createArchiveResponse_state = Lens.lens (\CreateArchiveResponse' {state} -> state) (\s@CreateArchiveResponse' {} a -> s {state = a} :: CreateArchiveResponse)

-- | The reason that the archive is in the state.
createArchiveResponse_stateReason :: Lens.Lens' CreateArchiveResponse (Prelude.Maybe Prelude.Text)
createArchiveResponse_stateReason = Lens.lens (\CreateArchiveResponse' {stateReason} -> stateReason) (\s@CreateArchiveResponse' {} a -> s {stateReason = a} :: CreateArchiveResponse)

-- | The response's http status code.
createArchiveResponse_httpStatus :: Lens.Lens' CreateArchiveResponse Prelude.Int
createArchiveResponse_httpStatus = Lens.lens (\CreateArchiveResponse' {httpStatus} -> httpStatus) (\s@CreateArchiveResponse' {} a -> s {httpStatus = a} :: CreateArchiveResponse)

instance Prelude.NFData CreateArchiveResponse where
  rnf CreateArchiveResponse' {..} =
    Prelude.rnf archiveArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf httpStatus
