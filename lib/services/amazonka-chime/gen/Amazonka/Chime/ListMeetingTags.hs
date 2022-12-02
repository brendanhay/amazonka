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
-- Module      : Amazonka.Chime.ListMeetingTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags applied to an Amazon Chime SDK meeting resource.
module Amazonka.Chime.ListMeetingTags
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

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMeetingTagsResponse'
            Prelude.<$> (x Data..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMeetingTags where
  hashWithSalt _salt ListMeetingTags' {..} =
    _salt `Prelude.hashWithSalt` meetingId

instance Prelude.NFData ListMeetingTags where
  rnf ListMeetingTags' {..} = Prelude.rnf meetingId

instance Data.ToHeaders ListMeetingTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMeetingTags where
  toPath ListMeetingTags' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/tags"]

instance Data.ToQuery ListMeetingTags where
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

instance Prelude.NFData ListMeetingTagsResponse where
  rnf ListMeetingTagsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
