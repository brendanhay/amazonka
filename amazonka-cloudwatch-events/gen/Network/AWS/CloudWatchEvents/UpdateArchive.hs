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
-- Module      : Network.AWS.CloudWatchEvents.UpdateArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified archive.
module Network.AWS.CloudWatchEvents.UpdateArchive
  ( -- * Creating a Request
    UpdateArchive (..),
    newUpdateArchive,

    -- * Request Lenses
    updateArchive_eventPattern,
    updateArchive_description,
    updateArchive_retentionDays,
    updateArchive_archiveName,

    -- * Destructuring the Response
    UpdateArchiveResponse (..),
    newUpdateArchiveResponse,

    -- * Response Lenses
    updateArchiveResponse_creationTime,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_state,
    updateArchiveResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateArchive' smart constructor.
data UpdateArchive = UpdateArchive'
  { -- | The event pattern to use to filter events sent to the archive.
    eventPattern :: Core.Maybe Core.Text,
    -- | The description for the archive.
    description :: Core.Maybe Core.Text,
    -- | The number of days to retain events in the archive.
    retentionDays :: Core.Maybe Core.Natural,
    -- | The name of the archive to update.
    archiveName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'updateArchive_eventPattern' - The event pattern to use to filter events sent to the archive.
--
-- 'description', 'updateArchive_description' - The description for the archive.
--
-- 'retentionDays', 'updateArchive_retentionDays' - The number of days to retain events in the archive.
--
-- 'archiveName', 'updateArchive_archiveName' - The name of the archive to update.
newUpdateArchive ::
  -- | 'archiveName'
  Core.Text ->
  UpdateArchive
newUpdateArchive pArchiveName_ =
  UpdateArchive'
    { eventPattern = Core.Nothing,
      description = Core.Nothing,
      retentionDays = Core.Nothing,
      archiveName = pArchiveName_
    }

-- | The event pattern to use to filter events sent to the archive.
updateArchive_eventPattern :: Lens.Lens' UpdateArchive (Core.Maybe Core.Text)
updateArchive_eventPattern = Lens.lens (\UpdateArchive' {eventPattern} -> eventPattern) (\s@UpdateArchive' {} a -> s {eventPattern = a} :: UpdateArchive)

-- | The description for the archive.
updateArchive_description :: Lens.Lens' UpdateArchive (Core.Maybe Core.Text)
updateArchive_description = Lens.lens (\UpdateArchive' {description} -> description) (\s@UpdateArchive' {} a -> s {description = a} :: UpdateArchive)

-- | The number of days to retain events in the archive.
updateArchive_retentionDays :: Lens.Lens' UpdateArchive (Core.Maybe Core.Natural)
updateArchive_retentionDays = Lens.lens (\UpdateArchive' {retentionDays} -> retentionDays) (\s@UpdateArchive' {} a -> s {retentionDays = a} :: UpdateArchive)

-- | The name of the archive to update.
updateArchive_archiveName :: Lens.Lens' UpdateArchive Core.Text
updateArchive_archiveName = Lens.lens (\UpdateArchive' {archiveName} -> archiveName) (\s@UpdateArchive' {} a -> s {archiveName = a} :: UpdateArchive)

instance Core.AWSRequest UpdateArchive where
  type
    AWSResponse UpdateArchive =
      UpdateArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateArchiveResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "ArchiveArn")
            Core.<*> (x Core..?> "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateArchive

instance Core.NFData UpdateArchive

instance Core.ToHeaders UpdateArchive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.UpdateArchive" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateArchive where
  toJSON UpdateArchive' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventPattern" Core..=) Core.<$> eventPattern,
            ("Description" Core..=) Core.<$> description,
            ("RetentionDays" Core..=) Core.<$> retentionDays,
            Core.Just ("ArchiveName" Core..= archiveName)
          ]
      )

instance Core.ToPath UpdateArchive where
  toPath = Core.const "/"

instance Core.ToQuery UpdateArchive where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateArchiveResponse' smart constructor.
data UpdateArchiveResponse = UpdateArchiveResponse'
  { -- | The time at which the archive was updated.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The reason that the archive is in the current state.
    stateReason :: Core.Maybe Core.Text,
    -- | The ARN of the archive.
    archiveArn :: Core.Maybe Core.Text,
    -- | The state of the archive.
    state :: Core.Maybe ArchiveState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'updateArchiveResponse_creationTime' - The time at which the archive was updated.
--
-- 'stateReason', 'updateArchiveResponse_stateReason' - The reason that the archive is in the current state.
--
-- 'archiveArn', 'updateArchiveResponse_archiveArn' - The ARN of the archive.
--
-- 'state', 'updateArchiveResponse_state' - The state of the archive.
--
-- 'httpStatus', 'updateArchiveResponse_httpStatus' - The response's http status code.
newUpdateArchiveResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateArchiveResponse
newUpdateArchiveResponse pHttpStatus_ =
  UpdateArchiveResponse'
    { creationTime = Core.Nothing,
      stateReason = Core.Nothing,
      archiveArn = Core.Nothing,
      state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the archive was updated.
updateArchiveResponse_creationTime :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Core.UTCTime)
updateArchiveResponse_creationTime = Lens.lens (\UpdateArchiveResponse' {creationTime} -> creationTime) (\s@UpdateArchiveResponse' {} a -> s {creationTime = a} :: UpdateArchiveResponse) Core.. Lens.mapping Core._Time

-- | The reason that the archive is in the current state.
updateArchiveResponse_stateReason :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Core.Text)
updateArchiveResponse_stateReason = Lens.lens (\UpdateArchiveResponse' {stateReason} -> stateReason) (\s@UpdateArchiveResponse' {} a -> s {stateReason = a} :: UpdateArchiveResponse)

-- | The ARN of the archive.
updateArchiveResponse_archiveArn :: Lens.Lens' UpdateArchiveResponse (Core.Maybe Core.Text)
updateArchiveResponse_archiveArn = Lens.lens (\UpdateArchiveResponse' {archiveArn} -> archiveArn) (\s@UpdateArchiveResponse' {} a -> s {archiveArn = a} :: UpdateArchiveResponse)

-- | The state of the archive.
updateArchiveResponse_state :: Lens.Lens' UpdateArchiveResponse (Core.Maybe ArchiveState)
updateArchiveResponse_state = Lens.lens (\UpdateArchiveResponse' {state} -> state) (\s@UpdateArchiveResponse' {} a -> s {state = a} :: UpdateArchiveResponse)

-- | The response's http status code.
updateArchiveResponse_httpStatus :: Lens.Lens' UpdateArchiveResponse Core.Int
updateArchiveResponse_httpStatus = Lens.lens (\UpdateArchiveResponse' {httpStatus} -> httpStatus) (\s@UpdateArchiveResponse' {} a -> s {httpStatus = a} :: UpdateArchiveResponse)

instance Core.NFData UpdateArchiveResponse
