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
-- Module      : Amazonka.Chime.TagAttendee
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies the specified tags to the specified Amazon Chime SDK attendee.
module Amazonka.Chime.TagAttendee
  ( -- * Creating a Request
    TagAttendee (..),
    newTagAttendee,

    -- * Request Lenses
    tagAttendee_meetingId,
    tagAttendee_attendeeId,
    tagAttendee_tags,

    -- * Destructuring the Response
    TagAttendeeResponse (..),
    newTagAttendeeResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagAttendee' smart constructor.
data TagAttendee = TagAttendee'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text,
    -- | The Amazon Chime SDK attendee ID.
    attendeeId :: Prelude.Text,
    -- | The tag key-value pairs.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagAttendee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'tagAttendee_meetingId' - The Amazon Chime SDK meeting ID.
--
-- 'attendeeId', 'tagAttendee_attendeeId' - The Amazon Chime SDK attendee ID.
--
-- 'tags', 'tagAttendee_tags' - The tag key-value pairs.
newTagAttendee ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'attendeeId'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  TagAttendee
newTagAttendee pMeetingId_ pAttendeeId_ pTags_ =
  TagAttendee'
    { meetingId = pMeetingId_,
      attendeeId = pAttendeeId_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The Amazon Chime SDK meeting ID.
tagAttendee_meetingId :: Lens.Lens' TagAttendee Prelude.Text
tagAttendee_meetingId = Lens.lens (\TagAttendee' {meetingId} -> meetingId) (\s@TagAttendee' {} a -> s {meetingId = a} :: TagAttendee)

-- | The Amazon Chime SDK attendee ID.
tagAttendee_attendeeId :: Lens.Lens' TagAttendee Prelude.Text
tagAttendee_attendeeId = Lens.lens (\TagAttendee' {attendeeId} -> attendeeId) (\s@TagAttendee' {} a -> s {attendeeId = a} :: TagAttendee)

-- | The tag key-value pairs.
tagAttendee_tags :: Lens.Lens' TagAttendee (Prelude.NonEmpty Tag)
tagAttendee_tags = Lens.lens (\TagAttendee' {tags} -> tags) (\s@TagAttendee' {} a -> s {tags = a} :: TagAttendee) Prelude.. Lens.coerced

instance Core.AWSRequest TagAttendee where
  type AWSResponse TagAttendee = TagAttendeeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull TagAttendeeResponse'

instance Prelude.Hashable TagAttendee where
  hashWithSalt _salt TagAttendee' {..} =
    _salt
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` attendeeId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagAttendee where
  rnf TagAttendee' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf attendeeId
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagAttendee where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON TagAttendee where
  toJSON TagAttendee' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Tags" Data..= tags)]
      )

instance Data.ToPath TagAttendee where
  toPath TagAttendee' {..} =
    Prelude.mconcat
      [ "/meetings/",
        Data.toBS meetingId,
        "/attendees/",
        Data.toBS attendeeId,
        "/tags"
      ]

instance Data.ToQuery TagAttendee where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=add"])

-- | /See:/ 'newTagAttendeeResponse' smart constructor.
data TagAttendeeResponse = TagAttendeeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagAttendeeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagAttendeeResponse ::
  TagAttendeeResponse
newTagAttendeeResponse = TagAttendeeResponse'

instance Prelude.NFData TagAttendeeResponse where
  rnf _ = ()
