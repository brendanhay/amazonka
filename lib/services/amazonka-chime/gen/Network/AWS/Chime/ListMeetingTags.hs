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
-- Module      : Network.AWS.Chime.ListMeetingTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags applied to an Amazon Chime SDK meeting resource.
module Network.AWS.Chime.ListMeetingTags
  ( -- * Creating a Request
    ListMeetingTags (..),
    newListMeetingTags,

    -- * Request Lenses
    listMeetingTags_meetingId,

    -- * Destructuring the Response
    ListMeetingTagsResponse (..),
    newListMeetingTagsResponse,

    -- * Response Lenses
    listMeetingTagsResponse_tags,
    listMeetingTagsResponse_httpStatus,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMeetingTags' smart constructor.
data ListMeetingTags = ListMeetingTags'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMeetingTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'listMeetingTags_meetingId' - The Amazon Chime SDK meeting ID.
newListMeetingTags ::
  -- | 'meetingId'
  Prelude.Text ->
  ListMeetingTags
newListMeetingTags pMeetingId_ =
  ListMeetingTags' {meetingId = pMeetingId_}

-- | The Amazon Chime SDK meeting ID.
listMeetingTags_meetingId :: Lens.Lens' ListMeetingTags Prelude.Text
listMeetingTags_meetingId = Lens.lens (\ListMeetingTags' {meetingId} -> meetingId) (\s@ListMeetingTags' {} a -> s {meetingId = a} :: ListMeetingTags)

instance Core.AWSRequest ListMeetingTags where
  type
    AWSResponse ListMeetingTags =
      ListMeetingTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMeetingTagsResponse'
            Prelude.<$> (x Core..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMeetingTags

instance Prelude.NFData ListMeetingTags

instance Core.ToHeaders ListMeetingTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListMeetingTags where
  toPath ListMeetingTags' {..} =
    Prelude.mconcat
      ["/meetings/", Core.toBS meetingId, "/tags"]

instance Core.ToQuery ListMeetingTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMeetingTagsResponse' smart constructor.
data ListMeetingTagsResponse = ListMeetingTagsResponse'
  { -- | A list of tag key-value pairs.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMeetingTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listMeetingTagsResponse_tags' - A list of tag key-value pairs.
--
-- 'httpStatus', 'listMeetingTagsResponse_httpStatus' - The response's http status code.
newListMeetingTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMeetingTagsResponse
newListMeetingTagsResponse pHttpStatus_ =
  ListMeetingTagsResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tag key-value pairs.
listMeetingTagsResponse_tags :: Lens.Lens' ListMeetingTagsResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listMeetingTagsResponse_tags = Lens.lens (\ListMeetingTagsResponse' {tags} -> tags) (\s@ListMeetingTagsResponse' {} a -> s {tags = a} :: ListMeetingTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMeetingTagsResponse_httpStatus :: Lens.Lens' ListMeetingTagsResponse Prelude.Int
listMeetingTagsResponse_httpStatus = Lens.lens (\ListMeetingTagsResponse' {httpStatus} -> httpStatus) (\s@ListMeetingTagsResponse' {} a -> s {httpStatus = a} :: ListMeetingTagsResponse)

instance Prelude.NFData ListMeetingTagsResponse
