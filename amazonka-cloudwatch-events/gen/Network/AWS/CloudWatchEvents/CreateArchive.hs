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
-- Module      : Network.AWS.CloudWatchEvents.CreateArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudWatchEvents.CreateArchive
  ( -- * Creating a Request
    CreateArchive (..),
    newCreateArchive,

    -- * Request Lenses
    createArchive_eventPattern,
    createArchive_description,
    createArchive_retentionDays,
    createArchive_archiveName,
    createArchive_eventSourceArn,

    -- * Destructuring the Response
    CreateArchiveResponse (..),
    newCreateArchiveResponse,

    -- * Response Lenses
    createArchiveResponse_creationTime,
    createArchiveResponse_stateReason,
    createArchiveResponse_archiveArn,
    createArchiveResponse_state,
    createArchiveResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateArchive' smart constructor.
data CreateArchive = CreateArchive'
  { -- | An event pattern to use to filter events sent to the archive.
    eventPattern :: Core.Maybe Core.Text,
    -- | A description for the archive.
    description :: Core.Maybe Core.Text,
    -- | The number of days to retain events for. Default value is 0. If set to
    -- 0, events are retained indefinitely
    retentionDays :: Core.Maybe Core.Natural,
    -- | The name for the archive to create.
    archiveName :: Core.Text,
    -- | The ARN of the event source associated with the archive.
    eventSourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'createArchive_eventPattern' - An event pattern to use to filter events sent to the archive.
--
-- 'description', 'createArchive_description' - A description for the archive.
--
-- 'retentionDays', 'createArchive_retentionDays' - The number of days to retain events for. Default value is 0. If set to
-- 0, events are retained indefinitely
--
-- 'archiveName', 'createArchive_archiveName' - The name for the archive to create.
--
-- 'eventSourceArn', 'createArchive_eventSourceArn' - The ARN of the event source associated with the archive.
newCreateArchive ::
  -- | 'archiveName'
  Core.Text ->
  -- | 'eventSourceArn'
  Core.Text ->
  CreateArchive
newCreateArchive pArchiveName_ pEventSourceArn_ =
  CreateArchive'
    { eventPattern = Core.Nothing,
      description = Core.Nothing,
      retentionDays = Core.Nothing,
      archiveName = pArchiveName_,
      eventSourceArn = pEventSourceArn_
    }

-- | An event pattern to use to filter events sent to the archive.
createArchive_eventPattern :: Lens.Lens' CreateArchive (Core.Maybe Core.Text)
createArchive_eventPattern = Lens.lens (\CreateArchive' {eventPattern} -> eventPattern) (\s@CreateArchive' {} a -> s {eventPattern = a} :: CreateArchive)

-- | A description for the archive.
createArchive_description :: Lens.Lens' CreateArchive (Core.Maybe Core.Text)
createArchive_description = Lens.lens (\CreateArchive' {description} -> description) (\s@CreateArchive' {} a -> s {description = a} :: CreateArchive)

-- | The number of days to retain events for. Default value is 0. If set to
-- 0, events are retained indefinitely
createArchive_retentionDays :: Lens.Lens' CreateArchive (Core.Maybe Core.Natural)
createArchive_retentionDays = Lens.lens (\CreateArchive' {retentionDays} -> retentionDays) (\s@CreateArchive' {} a -> s {retentionDays = a} :: CreateArchive)

-- | The name for the archive to create.
createArchive_archiveName :: Lens.Lens' CreateArchive Core.Text
createArchive_archiveName = Lens.lens (\CreateArchive' {archiveName} -> archiveName) (\s@CreateArchive' {} a -> s {archiveName = a} :: CreateArchive)

-- | The ARN of the event source associated with the archive.
createArchive_eventSourceArn :: Lens.Lens' CreateArchive Core.Text
createArchive_eventSourceArn = Lens.lens (\CreateArchive' {eventSourceArn} -> eventSourceArn) (\s@CreateArchive' {} a -> s {eventSourceArn = a} :: CreateArchive)

instance Core.AWSRequest CreateArchive where
  type
    AWSResponse CreateArchive =
      CreateArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateArchiveResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "ArchiveArn")
            Core.<*> (x Core..?> "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateArchive

instance Core.NFData CreateArchive

instance Core.ToHeaders CreateArchive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.CreateArchive" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateArchive where
  toJSON CreateArchive' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventPattern" Core..=) Core.<$> eventPattern,
            ("Description" Core..=) Core.<$> description,
            ("RetentionDays" Core..=) Core.<$> retentionDays,
            Core.Just ("ArchiveName" Core..= archiveName),
            Core.Just ("EventSourceArn" Core..= eventSourceArn)
          ]
      )

instance Core.ToPath CreateArchive where
  toPath = Core.const "/"

instance Core.ToQuery CreateArchive where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { -- | The time at which the archive was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The reason that the archive is in the state.
    stateReason :: Core.Maybe Core.Text,
    -- | The ARN of the archive that was created.
    archiveArn :: Core.Maybe Core.Text,
    -- | The state of the archive that was created.
    state :: Core.Maybe ArchiveState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createArchiveResponse_creationTime' - The time at which the archive was created.
--
-- 'stateReason', 'createArchiveResponse_stateReason' - The reason that the archive is in the state.
--
-- 'archiveArn', 'createArchiveResponse_archiveArn' - The ARN of the archive that was created.
--
-- 'state', 'createArchiveResponse_state' - The state of the archive that was created.
--
-- 'httpStatus', 'createArchiveResponse_httpStatus' - The response's http status code.
newCreateArchiveResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateArchiveResponse
newCreateArchiveResponse pHttpStatus_ =
  CreateArchiveResponse'
    { creationTime = Core.Nothing,
      stateReason = Core.Nothing,
      archiveArn = Core.Nothing,
      state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the archive was created.
createArchiveResponse_creationTime :: Lens.Lens' CreateArchiveResponse (Core.Maybe Core.UTCTime)
createArchiveResponse_creationTime = Lens.lens (\CreateArchiveResponse' {creationTime} -> creationTime) (\s@CreateArchiveResponse' {} a -> s {creationTime = a} :: CreateArchiveResponse) Core.. Lens.mapping Core._Time

-- | The reason that the archive is in the state.
createArchiveResponse_stateReason :: Lens.Lens' CreateArchiveResponse (Core.Maybe Core.Text)
createArchiveResponse_stateReason = Lens.lens (\CreateArchiveResponse' {stateReason} -> stateReason) (\s@CreateArchiveResponse' {} a -> s {stateReason = a} :: CreateArchiveResponse)

-- | The ARN of the archive that was created.
createArchiveResponse_archiveArn :: Lens.Lens' CreateArchiveResponse (Core.Maybe Core.Text)
createArchiveResponse_archiveArn = Lens.lens (\CreateArchiveResponse' {archiveArn} -> archiveArn) (\s@CreateArchiveResponse' {} a -> s {archiveArn = a} :: CreateArchiveResponse)

-- | The state of the archive that was created.
createArchiveResponse_state :: Lens.Lens' CreateArchiveResponse (Core.Maybe ArchiveState)
createArchiveResponse_state = Lens.lens (\CreateArchiveResponse' {state} -> state) (\s@CreateArchiveResponse' {} a -> s {state = a} :: CreateArchiveResponse)

-- | The response's http status code.
createArchiveResponse_httpStatus :: Lens.Lens' CreateArchiveResponse Core.Int
createArchiveResponse_httpStatus = Lens.lens (\CreateArchiveResponse' {httpStatus} -> httpStatus) (\s@CreateArchiveResponse' {} a -> s {httpStatus = a} :: CreateArchiveResponse)

instance Core.NFData CreateArchiveResponse
