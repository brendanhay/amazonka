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
-- Module      : Amazonka.ChimeSdkMeetings.BatchCreateAttendee
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates up to 100 attendees for an active Amazon Chime SDK meeting. For
-- more information about the Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.ChimeSdkMeetings.BatchCreateAttendee
  ( -- * Creating a Request
    BatchCreateAttendee (..),
    newBatchCreateAttendee,

    -- * Request Lenses
    batchCreateAttendee_meetingId,
    batchCreateAttendee_attendees,

    -- * Destructuring the Response
    BatchCreateAttendeeResponse (..),
    newBatchCreateAttendeeResponse,

    -- * Response Lenses
    batchCreateAttendeeResponse_attendees,
    batchCreateAttendeeResponse_errors,
    batchCreateAttendeeResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateAttendee' smart constructor.
data BatchCreateAttendee = BatchCreateAttendee'
  { -- | The Amazon Chime SDK ID of the meeting to which you\'re adding
    -- attendees.
    meetingId :: Prelude.Text,
    -- | The attendee information, including attendees\' IDs and join tokens.
    attendees :: Prelude.NonEmpty CreateAttendeeRequestItem
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateAttendee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'batchCreateAttendee_meetingId' - The Amazon Chime SDK ID of the meeting to which you\'re adding
-- attendees.
--
-- 'attendees', 'batchCreateAttendee_attendees' - The attendee information, including attendees\' IDs and join tokens.
newBatchCreateAttendee ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'attendees'
  Prelude.NonEmpty CreateAttendeeRequestItem ->
  BatchCreateAttendee
newBatchCreateAttendee pMeetingId_ pAttendees_ =
  BatchCreateAttendee'
    { meetingId = pMeetingId_,
      attendees = Lens.coerced Lens.# pAttendees_
    }

-- | The Amazon Chime SDK ID of the meeting to which you\'re adding
-- attendees.
batchCreateAttendee_meetingId :: Lens.Lens' BatchCreateAttendee Prelude.Text
batchCreateAttendee_meetingId = Lens.lens (\BatchCreateAttendee' {meetingId} -> meetingId) (\s@BatchCreateAttendee' {} a -> s {meetingId = a} :: BatchCreateAttendee)

-- | The attendee information, including attendees\' IDs and join tokens.
batchCreateAttendee_attendees :: Lens.Lens' BatchCreateAttendee (Prelude.NonEmpty CreateAttendeeRequestItem)
batchCreateAttendee_attendees = Lens.lens (\BatchCreateAttendee' {attendees} -> attendees) (\s@BatchCreateAttendee' {} a -> s {attendees = a} :: BatchCreateAttendee) Prelude.. Lens.coerced

instance Core.AWSRequest BatchCreateAttendee where
  type
    AWSResponse BatchCreateAttendee =
      BatchCreateAttendeeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateAttendeeResponse'
            Prelude.<$> (x Data..?> "Attendees" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchCreateAttendee where
  hashWithSalt _salt BatchCreateAttendee' {..} =
    _salt `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` attendees

instance Prelude.NFData BatchCreateAttendee where
  rnf BatchCreateAttendee' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf attendees

instance Data.ToHeaders BatchCreateAttendee where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchCreateAttendee where
  toJSON BatchCreateAttendee' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Attendees" Data..= attendees)]
      )

instance Data.ToPath BatchCreateAttendee where
  toPath BatchCreateAttendee' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/attendees"]

instance Data.ToQuery BatchCreateAttendee where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=batch-create"])

-- | /See:/ 'newBatchCreateAttendeeResponse' smart constructor.
data BatchCreateAttendeeResponse = BatchCreateAttendeeResponse'
  { -- | The attendee information, including attendees\' IDs and join tokens.
    attendees :: Prelude.Maybe [Attendee],
    -- | If the action fails for one or more of the attendees in the request, a
    -- list of the attendees is returned, along with error codes and error
    -- messages.
    errors :: Prelude.Maybe [CreateAttendeeError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateAttendeeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendees', 'batchCreateAttendeeResponse_attendees' - The attendee information, including attendees\' IDs and join tokens.
--
-- 'errors', 'batchCreateAttendeeResponse_errors' - If the action fails for one or more of the attendees in the request, a
-- list of the attendees is returned, along with error codes and error
-- messages.
--
-- 'httpStatus', 'batchCreateAttendeeResponse_httpStatus' - The response's http status code.
newBatchCreateAttendeeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateAttendeeResponse
newBatchCreateAttendeeResponse pHttpStatus_ =
  BatchCreateAttendeeResponse'
    { attendees =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attendee information, including attendees\' IDs and join tokens.
batchCreateAttendeeResponse_attendees :: Lens.Lens' BatchCreateAttendeeResponse (Prelude.Maybe [Attendee])
batchCreateAttendeeResponse_attendees = Lens.lens (\BatchCreateAttendeeResponse' {attendees} -> attendees) (\s@BatchCreateAttendeeResponse' {} a -> s {attendees = a} :: BatchCreateAttendeeResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the action fails for one or more of the attendees in the request, a
-- list of the attendees is returned, along with error codes and error
-- messages.
batchCreateAttendeeResponse_errors :: Lens.Lens' BatchCreateAttendeeResponse (Prelude.Maybe [CreateAttendeeError])
batchCreateAttendeeResponse_errors = Lens.lens (\BatchCreateAttendeeResponse' {errors} -> errors) (\s@BatchCreateAttendeeResponse' {} a -> s {errors = a} :: BatchCreateAttendeeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateAttendeeResponse_httpStatus :: Lens.Lens' BatchCreateAttendeeResponse Prelude.Int
batchCreateAttendeeResponse_httpStatus = Lens.lens (\BatchCreateAttendeeResponse' {httpStatus} -> httpStatus) (\s@BatchCreateAttendeeResponse' {} a -> s {httpStatus = a} :: BatchCreateAttendeeResponse)

instance Prelude.NFData BatchCreateAttendeeResponse where
  rnf BatchCreateAttendeeResponse' {..} =
    Prelude.rnf attendees
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
