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
-- Module      : Amazonka.Chime.ListAttendeeTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags applied to an Amazon Chime SDK attendee resource.
module Amazonka.Chime.ListAttendeeTags
  ( -- * Creating a Request
    ListAttendeeTags (..),
    newListAttendeeTags,

    -- * Request Lenses
    listAttendeeTags_meetingId,
    listAttendeeTags_attendeeId,

    -- * Destructuring the Response
    ListAttendeeTagsResponse (..),
    newListAttendeeTagsResponse,

    -- * Response Lenses
    listAttendeeTagsResponse_tags,
    listAttendeeTagsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttendeeTags' smart constructor.
data ListAttendeeTags = ListAttendeeTags'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text,
    -- | The Amazon Chime SDK attendee ID.
    attendeeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttendeeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'listAttendeeTags_meetingId' - The Amazon Chime SDK meeting ID.
--
-- 'attendeeId', 'listAttendeeTags_attendeeId' - The Amazon Chime SDK attendee ID.
newListAttendeeTags ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'attendeeId'
  Prelude.Text ->
  ListAttendeeTags
newListAttendeeTags pMeetingId_ pAttendeeId_ =
  ListAttendeeTags'
    { meetingId = pMeetingId_,
      attendeeId = pAttendeeId_
    }

-- | The Amazon Chime SDK meeting ID.
listAttendeeTags_meetingId :: Lens.Lens' ListAttendeeTags Prelude.Text
listAttendeeTags_meetingId = Lens.lens (\ListAttendeeTags' {meetingId} -> meetingId) (\s@ListAttendeeTags' {} a -> s {meetingId = a} :: ListAttendeeTags)

-- | The Amazon Chime SDK attendee ID.
listAttendeeTags_attendeeId :: Lens.Lens' ListAttendeeTags Prelude.Text
listAttendeeTags_attendeeId = Lens.lens (\ListAttendeeTags' {attendeeId} -> attendeeId) (\s@ListAttendeeTags' {} a -> s {attendeeId = a} :: ListAttendeeTags)

instance Core.AWSRequest ListAttendeeTags where
  type
    AWSResponse ListAttendeeTags =
      ListAttendeeTagsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttendeeTagsResponse'
            Prelude.<$> (x Data..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttendeeTags where
  hashWithSalt _salt ListAttendeeTags' {..} =
    _salt
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` attendeeId

instance Prelude.NFData ListAttendeeTags where
  rnf ListAttendeeTags' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf attendeeId

instance Data.ToHeaders ListAttendeeTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAttendeeTags where
  toPath ListAttendeeTags' {..} =
    Prelude.mconcat
      [ "/meetings/",
        Data.toBS meetingId,
        "/attendees/",
        Data.toBS attendeeId,
        "/tags"
      ]

instance Data.ToQuery ListAttendeeTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttendeeTagsResponse' smart constructor.
data ListAttendeeTagsResponse = ListAttendeeTagsResponse'
  { -- | A list of tag key-value pairs.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttendeeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listAttendeeTagsResponse_tags' - A list of tag key-value pairs.
--
-- 'httpStatus', 'listAttendeeTagsResponse_httpStatus' - The response's http status code.
newListAttendeeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttendeeTagsResponse
newListAttendeeTagsResponse pHttpStatus_ =
  ListAttendeeTagsResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tag key-value pairs.
listAttendeeTagsResponse_tags :: Lens.Lens' ListAttendeeTagsResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listAttendeeTagsResponse_tags = Lens.lens (\ListAttendeeTagsResponse' {tags} -> tags) (\s@ListAttendeeTagsResponse' {} a -> s {tags = a} :: ListAttendeeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAttendeeTagsResponse_httpStatus :: Lens.Lens' ListAttendeeTagsResponse Prelude.Int
listAttendeeTagsResponse_httpStatus = Lens.lens (\ListAttendeeTagsResponse' {httpStatus} -> httpStatus) (\s@ListAttendeeTagsResponse' {} a -> s {httpStatus = a} :: ListAttendeeTagsResponse)

instance Prelude.NFData ListAttendeeTagsResponse where
  rnf ListAttendeeTagsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
