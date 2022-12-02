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
-- Module      : Amazonka.ChimeSdkMeetings.BatchUpdateAttendeeCapabilitiesExcept
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates @AttendeeCapabilities@ except the capabilities listed in an
-- @ExcludedAttendeeIds@ table.
--
-- You use the capabilities with a set of values that control what the
-- capabilities can do, such as @SendReceive@ data. For more information
-- about those values, see .
--
-- When using capabilities, be aware of these corner cases:
--
-- -   You can\'t set @content@ capabilities to @SendReceive@ or @Receive@
--     unless you also set @video@ capabilities to @SendReceive@ or
--     @Receive@. If you don\'t set the @video@ capability to receive, the
--     response will contain an HTTP 400 Bad Request status code. However,
--     you can set your @video@ capability to receive and you set your
--     @content@ capability to not receive.
--
-- -   When you change an @audio@ capability from @None@ or @Receive@ to
--     @Send@ or @SendReceive@ , and if the attendee left their microphone
--     unmuted, audio will flow from the attendee to the other meeting
--     participants.
--
-- -   When you change a @video@ or @content@ capability from @None@ or
--     @Receive@ to @Send@ or @SendReceive@ , and if the attendee turned on
--     their video or content streams, remote attendess can receive those
--     streams, but only after media renegotiation between the client and
--     the Amazon Chime back-end server.
module Amazonka.ChimeSdkMeetings.BatchUpdateAttendeeCapabilitiesExcept
  ( -- * Creating a Request
    BatchUpdateAttendeeCapabilitiesExcept (..),
    newBatchUpdateAttendeeCapabilitiesExcept,

    -- * Request Lenses
    batchUpdateAttendeeCapabilitiesExcept_meetingId,
    batchUpdateAttendeeCapabilitiesExcept_excludedAttendeeIds,
    batchUpdateAttendeeCapabilitiesExcept_capabilities,

    -- * Destructuring the Response
    BatchUpdateAttendeeCapabilitiesExceptResponse (..),
    newBatchUpdateAttendeeCapabilitiesExceptResponse,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateAttendeeCapabilitiesExcept' smart constructor.
data BatchUpdateAttendeeCapabilitiesExcept = BatchUpdateAttendeeCapabilitiesExcept'
  { -- | The ID of the meeting associated with the update request.
    meetingId :: Prelude.Text,
    -- | The @AttendeeIDs@ that you want to exclude from one or more
    -- capabilities.
    excludedAttendeeIds :: Prelude.NonEmpty AttendeeIdItem,
    -- | The capabilities (@audio@, @video@, or @content@) that you want to
    -- update.
    capabilities :: AttendeeCapabilities
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateAttendeeCapabilitiesExcept' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'batchUpdateAttendeeCapabilitiesExcept_meetingId' - The ID of the meeting associated with the update request.
--
-- 'excludedAttendeeIds', 'batchUpdateAttendeeCapabilitiesExcept_excludedAttendeeIds' - The @AttendeeIDs@ that you want to exclude from one or more
-- capabilities.
--
-- 'capabilities', 'batchUpdateAttendeeCapabilitiesExcept_capabilities' - The capabilities (@audio@, @video@, or @content@) that you want to
-- update.
newBatchUpdateAttendeeCapabilitiesExcept ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'excludedAttendeeIds'
  Prelude.NonEmpty AttendeeIdItem ->
  -- | 'capabilities'
  AttendeeCapabilities ->
  BatchUpdateAttendeeCapabilitiesExcept
newBatchUpdateAttendeeCapabilitiesExcept
  pMeetingId_
  pExcludedAttendeeIds_
  pCapabilities_ =
    BatchUpdateAttendeeCapabilitiesExcept'
      { meetingId =
          pMeetingId_,
        excludedAttendeeIds =
          Lens.coerced
            Lens.# pExcludedAttendeeIds_,
        capabilities = pCapabilities_
      }

-- | The ID of the meeting associated with the update request.
batchUpdateAttendeeCapabilitiesExcept_meetingId :: Lens.Lens' BatchUpdateAttendeeCapabilitiesExcept Prelude.Text
batchUpdateAttendeeCapabilitiesExcept_meetingId = Lens.lens (\BatchUpdateAttendeeCapabilitiesExcept' {meetingId} -> meetingId) (\s@BatchUpdateAttendeeCapabilitiesExcept' {} a -> s {meetingId = a} :: BatchUpdateAttendeeCapabilitiesExcept)

-- | The @AttendeeIDs@ that you want to exclude from one or more
-- capabilities.
batchUpdateAttendeeCapabilitiesExcept_excludedAttendeeIds :: Lens.Lens' BatchUpdateAttendeeCapabilitiesExcept (Prelude.NonEmpty AttendeeIdItem)
batchUpdateAttendeeCapabilitiesExcept_excludedAttendeeIds = Lens.lens (\BatchUpdateAttendeeCapabilitiesExcept' {excludedAttendeeIds} -> excludedAttendeeIds) (\s@BatchUpdateAttendeeCapabilitiesExcept' {} a -> s {excludedAttendeeIds = a} :: BatchUpdateAttendeeCapabilitiesExcept) Prelude.. Lens.coerced

-- | The capabilities (@audio@, @video@, or @content@) that you want to
-- update.
batchUpdateAttendeeCapabilitiesExcept_capabilities :: Lens.Lens' BatchUpdateAttendeeCapabilitiesExcept AttendeeCapabilities
batchUpdateAttendeeCapabilitiesExcept_capabilities = Lens.lens (\BatchUpdateAttendeeCapabilitiesExcept' {capabilities} -> capabilities) (\s@BatchUpdateAttendeeCapabilitiesExcept' {} a -> s {capabilities = a} :: BatchUpdateAttendeeCapabilitiesExcept)

instance
  Core.AWSRequest
    BatchUpdateAttendeeCapabilitiesExcept
  where
  type
    AWSResponse
      BatchUpdateAttendeeCapabilitiesExcept =
      BatchUpdateAttendeeCapabilitiesExceptResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      BatchUpdateAttendeeCapabilitiesExceptResponse'

instance
  Prelude.Hashable
    BatchUpdateAttendeeCapabilitiesExcept
  where
  hashWithSalt
    _salt
    BatchUpdateAttendeeCapabilitiesExcept' {..} =
      _salt `Prelude.hashWithSalt` meetingId
        `Prelude.hashWithSalt` excludedAttendeeIds
        `Prelude.hashWithSalt` capabilities

instance
  Prelude.NFData
    BatchUpdateAttendeeCapabilitiesExcept
  where
  rnf BatchUpdateAttendeeCapabilitiesExcept' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf excludedAttendeeIds
      `Prelude.seq` Prelude.rnf capabilities

instance
  Data.ToHeaders
    BatchUpdateAttendeeCapabilitiesExcept
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    BatchUpdateAttendeeCapabilitiesExcept
  where
  toJSON BatchUpdateAttendeeCapabilitiesExcept' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExcludedAttendeeIds" Data..= excludedAttendeeIds),
            Prelude.Just ("Capabilities" Data..= capabilities)
          ]
      )

instance
  Data.ToPath
    BatchUpdateAttendeeCapabilitiesExcept
  where
  toPath BatchUpdateAttendeeCapabilitiesExcept' {..} =
    Prelude.mconcat
      [ "/meetings/",
        Data.toBS meetingId,
        "/attendees/capabilities"
      ]

instance
  Data.ToQuery
    BatchUpdateAttendeeCapabilitiesExcept
  where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=batch-update-except"])

-- | /See:/ 'newBatchUpdateAttendeeCapabilitiesExceptResponse' smart constructor.
data BatchUpdateAttendeeCapabilitiesExceptResponse = BatchUpdateAttendeeCapabilitiesExceptResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateAttendeeCapabilitiesExceptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchUpdateAttendeeCapabilitiesExceptResponse ::
  BatchUpdateAttendeeCapabilitiesExceptResponse
newBatchUpdateAttendeeCapabilitiesExceptResponse =
  BatchUpdateAttendeeCapabilitiesExceptResponse'

instance
  Prelude.NFData
    BatchUpdateAttendeeCapabilitiesExceptResponse
  where
  rnf _ = ()
