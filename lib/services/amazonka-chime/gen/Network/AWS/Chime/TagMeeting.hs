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
-- Module      : Network.AWS.Chime.TagMeeting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies the specified tags to the specified Amazon Chime SDK meeting.
module Network.AWS.Chime.TagMeeting
  ( -- * Creating a Request
    TagMeeting (..),
    newTagMeeting,

    -- * Request Lenses
    tagMeeting_meetingId,
    tagMeeting_tags,

    -- * Destructuring the Response
    TagMeetingResponse (..),
    newTagMeetingResponse,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagMeeting' smart constructor.
data TagMeeting = TagMeeting'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text,
    -- | The tag key-value pairs.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagMeeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'tagMeeting_meetingId' - The Amazon Chime SDK meeting ID.
--
-- 'tags', 'tagMeeting_tags' - The tag key-value pairs.
newTagMeeting ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  TagMeeting
newTagMeeting pMeetingId_ pTags_ =
  TagMeeting'
    { meetingId = pMeetingId_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The Amazon Chime SDK meeting ID.
tagMeeting_meetingId :: Lens.Lens' TagMeeting Prelude.Text
tagMeeting_meetingId = Lens.lens (\TagMeeting' {meetingId} -> meetingId) (\s@TagMeeting' {} a -> s {meetingId = a} :: TagMeeting)

-- | The tag key-value pairs.
tagMeeting_tags :: Lens.Lens' TagMeeting (Prelude.NonEmpty Tag)
tagMeeting_tags = Lens.lens (\TagMeeting' {tags} -> tags) (\s@TagMeeting' {} a -> s {tags = a} :: TagMeeting) Prelude.. Lens.coerced

instance Core.AWSRequest TagMeeting where
  type AWSResponse TagMeeting = TagMeetingResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagMeetingResponse'

instance Prelude.Hashable TagMeeting

instance Prelude.NFData TagMeeting

instance Core.ToHeaders TagMeeting where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON TagMeeting where
  toJSON TagMeeting' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Tags" Core..= tags)]
      )

instance Core.ToPath TagMeeting where
  toPath TagMeeting' {..} =
    Prelude.mconcat
      ["/meetings/", Core.toBS meetingId, "/tags"]

instance Core.ToQuery TagMeeting where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=add"])

-- | /See:/ 'newTagMeetingResponse' smart constructor.
data TagMeetingResponse = TagMeetingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagMeetingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagMeetingResponse ::
  TagMeetingResponse
newTagMeetingResponse = TagMeetingResponse'

instance Prelude.NFData TagMeetingResponse
