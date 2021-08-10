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
-- Module      : Network.AWS.CloudWatchEvents.DescribeArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an archive.
module Network.AWS.CloudWatchEvents.DescribeArchive
  ( -- * Creating a Request
    DescribeArchive (..),
    newDescribeArchive,

    -- * Request Lenses
    describeArchive_archiveName,

    -- * Destructuring the Response
    DescribeArchiveResponse (..),
    newDescribeArchiveResponse,

    -- * Response Lenses
    describeArchiveResponse_eventCount,
    describeArchiveResponse_eventPattern,
    describeArchiveResponse_eventSourceArn,
    describeArchiveResponse_creationTime,
    describeArchiveResponse_stateReason,
    describeArchiveResponse_archiveName,
    describeArchiveResponse_archiveArn,
    describeArchiveResponse_state,
    describeArchiveResponse_sizeBytes,
    describeArchiveResponse_description,
    describeArchiveResponse_retentionDays,
    describeArchiveResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeArchive' smart constructor.
data DescribeArchive = DescribeArchive'
  { -- | The name of the archive to retrieve.
    archiveName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveName', 'describeArchive_archiveName' - The name of the archive to retrieve.
newDescribeArchive ::
  -- | 'archiveName'
  Prelude.Text ->
  DescribeArchive
newDescribeArchive pArchiveName_ =
  DescribeArchive' {archiveName = pArchiveName_}

-- | The name of the archive to retrieve.
describeArchive_archiveName :: Lens.Lens' DescribeArchive Prelude.Text
describeArchive_archiveName = Lens.lens (\DescribeArchive' {archiveName} -> archiveName) (\s@DescribeArchive' {} a -> s {archiveName = a} :: DescribeArchive)

instance Core.AWSRequest DescribeArchive where
  type
    AWSResponse DescribeArchive =
      DescribeArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeArchiveResponse'
            Prelude.<$> (x Core..?> "EventCount")
            Prelude.<*> (x Core..?> "EventPattern")
            Prelude.<*> (x Core..?> "EventSourceArn")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "StateReason")
            Prelude.<*> (x Core..?> "ArchiveName")
            Prelude.<*> (x Core..?> "ArchiveArn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "SizeBytes")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RetentionDays")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeArchive

instance Prelude.NFData DescribeArchive

instance Core.ToHeaders DescribeArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DescribeArchive" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeArchive where
  toJSON DescribeArchive' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ArchiveName" Core..= archiveName)]
      )

instance Core.ToPath DescribeArchive where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeArchive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeArchiveResponse' smart constructor.
data DescribeArchiveResponse = DescribeArchiveResponse'
  { -- | The number of events in the archive.
    eventCount :: Prelude.Maybe Prelude.Integer,
    -- | The event pattern used to filter events sent to the archive.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event source associated with the archive.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the archive was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The reason that the archive is in the state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the archive.
    archiveName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the archive.
    archiveArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the archive.
    state :: Prelude.Maybe ArchiveState,
    -- | The size of the archive in bytes.
    sizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The description of the archive.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain events for in the archive.
    retentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCount', 'describeArchiveResponse_eventCount' - The number of events in the archive.
--
-- 'eventPattern', 'describeArchiveResponse_eventPattern' - The event pattern used to filter events sent to the archive.
--
-- 'eventSourceArn', 'describeArchiveResponse_eventSourceArn' - The ARN of the event source associated with the archive.
--
-- 'creationTime', 'describeArchiveResponse_creationTime' - The time at which the archive was created.
--
-- 'stateReason', 'describeArchiveResponse_stateReason' - The reason that the archive is in the state.
--
-- 'archiveName', 'describeArchiveResponse_archiveName' - The name of the archive.
--
-- 'archiveArn', 'describeArchiveResponse_archiveArn' - The ARN of the archive.
--
-- 'state', 'describeArchiveResponse_state' - The state of the archive.
--
-- 'sizeBytes', 'describeArchiveResponse_sizeBytes' - The size of the archive in bytes.
--
-- 'description', 'describeArchiveResponse_description' - The description of the archive.
--
-- 'retentionDays', 'describeArchiveResponse_retentionDays' - The number of days to retain events for in the archive.
--
-- 'httpStatus', 'describeArchiveResponse_httpStatus' - The response's http status code.
newDescribeArchiveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeArchiveResponse
newDescribeArchiveResponse pHttpStatus_ =
  DescribeArchiveResponse'
    { eventCount =
        Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      eventSourceArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      archiveName = Prelude.Nothing,
      archiveArn = Prelude.Nothing,
      state = Prelude.Nothing,
      sizeBytes = Prelude.Nothing,
      description = Prelude.Nothing,
      retentionDays = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of events in the archive.
describeArchiveResponse_eventCount :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Integer)
describeArchiveResponse_eventCount = Lens.lens (\DescribeArchiveResponse' {eventCount} -> eventCount) (\s@DescribeArchiveResponse' {} a -> s {eventCount = a} :: DescribeArchiveResponse)

-- | The event pattern used to filter events sent to the archive.
describeArchiveResponse_eventPattern :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_eventPattern = Lens.lens (\DescribeArchiveResponse' {eventPattern} -> eventPattern) (\s@DescribeArchiveResponse' {} a -> s {eventPattern = a} :: DescribeArchiveResponse)

-- | The ARN of the event source associated with the archive.
describeArchiveResponse_eventSourceArn :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_eventSourceArn = Lens.lens (\DescribeArchiveResponse' {eventSourceArn} -> eventSourceArn) (\s@DescribeArchiveResponse' {} a -> s {eventSourceArn = a} :: DescribeArchiveResponse)

-- | The time at which the archive was created.
describeArchiveResponse_creationTime :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.UTCTime)
describeArchiveResponse_creationTime = Lens.lens (\DescribeArchiveResponse' {creationTime} -> creationTime) (\s@DescribeArchiveResponse' {} a -> s {creationTime = a} :: DescribeArchiveResponse) Prelude.. Lens.mapping Core._Time

-- | The reason that the archive is in the state.
describeArchiveResponse_stateReason :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_stateReason = Lens.lens (\DescribeArchiveResponse' {stateReason} -> stateReason) (\s@DescribeArchiveResponse' {} a -> s {stateReason = a} :: DescribeArchiveResponse)

-- | The name of the archive.
describeArchiveResponse_archiveName :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_archiveName = Lens.lens (\DescribeArchiveResponse' {archiveName} -> archiveName) (\s@DescribeArchiveResponse' {} a -> s {archiveName = a} :: DescribeArchiveResponse)

-- | The ARN of the archive.
describeArchiveResponse_archiveArn :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_archiveArn = Lens.lens (\DescribeArchiveResponse' {archiveArn} -> archiveArn) (\s@DescribeArchiveResponse' {} a -> s {archiveArn = a} :: DescribeArchiveResponse)

-- | The state of the archive.
describeArchiveResponse_state :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe ArchiveState)
describeArchiveResponse_state = Lens.lens (\DescribeArchiveResponse' {state} -> state) (\s@DescribeArchiveResponse' {} a -> s {state = a} :: DescribeArchiveResponse)

-- | The size of the archive in bytes.
describeArchiveResponse_sizeBytes :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Integer)
describeArchiveResponse_sizeBytes = Lens.lens (\DescribeArchiveResponse' {sizeBytes} -> sizeBytes) (\s@DescribeArchiveResponse' {} a -> s {sizeBytes = a} :: DescribeArchiveResponse)

-- | The description of the archive.
describeArchiveResponse_description :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Text)
describeArchiveResponse_description = Lens.lens (\DescribeArchiveResponse' {description} -> description) (\s@DescribeArchiveResponse' {} a -> s {description = a} :: DescribeArchiveResponse)

-- | The number of days to retain events for in the archive.
describeArchiveResponse_retentionDays :: Lens.Lens' DescribeArchiveResponse (Prelude.Maybe Prelude.Natural)
describeArchiveResponse_retentionDays = Lens.lens (\DescribeArchiveResponse' {retentionDays} -> retentionDays) (\s@DescribeArchiveResponse' {} a -> s {retentionDays = a} :: DescribeArchiveResponse)

-- | The response's http status code.
describeArchiveResponse_httpStatus :: Lens.Lens' DescribeArchiveResponse Prelude.Int
describeArchiveResponse_httpStatus = Lens.lens (\DescribeArchiveResponse' {httpStatus} -> httpStatus) (\s@DescribeArchiveResponse' {} a -> s {httpStatus = a} :: DescribeArchiveResponse)

instance Prelude.NFData DescribeArchiveResponse
