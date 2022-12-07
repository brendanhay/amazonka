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
-- Module      : Amazonka.IAM.ListUserTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified IAM user. The returned
-- list of tags is sorted by tag key. For more information about tagging,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListUserTags
  ( -- * Creating a Request
    ListUserTags (..),
    newListUserTags,

    -- * Request Lenses
    listUserTags_marker,
    listUserTags_maxItems,
    listUserTags_userName,

    -- * Destructuring the Response
    ListUserTagsResponse (..),
    newListUserTagsResponse,

    -- * Response Lenses
    listUserTagsResponse_marker,
    listUserTagsResponse_isTruncated,
    listUserTagsResponse_httpStatus,
    listUserTagsResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUserTags' smart constructor.
data ListUserTags = ListUserTags'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The name of the IAM user whose tags you want to see.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listUserTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listUserTags_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'userName', 'listUserTags_userName' - The name of the IAM user whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListUserTags ::
  -- | 'userName'
  Prelude.Text ->
  ListUserTags
newListUserTags pUserName_ =
  ListUserTags'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      userName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listUserTags_marker :: Lens.Lens' ListUserTags (Prelude.Maybe Prelude.Text)
listUserTags_marker = Lens.lens (\ListUserTags' {marker} -> marker) (\s@ListUserTags' {} a -> s {marker = a} :: ListUserTags)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listUserTags_maxItems :: Lens.Lens' ListUserTags (Prelude.Maybe Prelude.Natural)
listUserTags_maxItems = Lens.lens (\ListUserTags' {maxItems} -> maxItems) (\s@ListUserTags' {} a -> s {maxItems = a} :: ListUserTags)

-- | The name of the IAM user whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listUserTags_userName :: Lens.Lens' ListUserTags Prelude.Text
listUserTags_userName = Lens.lens (\ListUserTags' {userName} -> userName) (\s@ListUserTags' {} a -> s {userName = a} :: ListUserTags)

instance Core.AWSPager ListUserTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserTagsResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listUserTagsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUserTags_marker
          Lens..~ rs
          Lens.^? listUserTagsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListUserTags where
  type AWSResponse ListUserTags = ListUserTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListUserTagsResult"
      ( \s h x ->
          ListUserTagsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListUserTags where
  hashWithSalt _salt ListUserTags' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListUserTags where
  rnf ListUserTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ListUserTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListUserTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUserTags where
  toQuery ListUserTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListUserTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "UserName" Data.=: userName
      ]

-- | /See:/ 'newListUserTagsResponse' smart constructor.
data ListUserTagsResponse = ListUserTagsResponse'
  { -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of tags that are currently attached to the user. Each tag
    -- consists of a key name and an associated value. If no tags are attached
    -- to the specified resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listUserTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'listUserTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'httpStatus', 'listUserTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listUserTagsResponse_tags' - The list of tags that are currently attached to the user. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
newListUserTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserTagsResponse
newListUserTagsResponse pHttpStatus_ =
  ListUserTagsResponse'
    { marker = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listUserTagsResponse_marker :: Lens.Lens' ListUserTagsResponse (Prelude.Maybe Prelude.Text)
listUserTagsResponse_marker = Lens.lens (\ListUserTagsResponse' {marker} -> marker) (\s@ListUserTagsResponse' {} a -> s {marker = a} :: ListUserTagsResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listUserTagsResponse_isTruncated :: Lens.Lens' ListUserTagsResponse (Prelude.Maybe Prelude.Bool)
listUserTagsResponse_isTruncated = Lens.lens (\ListUserTagsResponse' {isTruncated} -> isTruncated) (\s@ListUserTagsResponse' {} a -> s {isTruncated = a} :: ListUserTagsResponse)

-- | The response's http status code.
listUserTagsResponse_httpStatus :: Lens.Lens' ListUserTagsResponse Prelude.Int
listUserTagsResponse_httpStatus = Lens.lens (\ListUserTagsResponse' {httpStatus} -> httpStatus) (\s@ListUserTagsResponse' {} a -> s {httpStatus = a} :: ListUserTagsResponse)

-- | The list of tags that are currently attached to the user. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
listUserTagsResponse_tags :: Lens.Lens' ListUserTagsResponse [Tag]
listUserTagsResponse_tags = Lens.lens (\ListUserTagsResponse' {tags} -> tags) (\s@ListUserTagsResponse' {} a -> s {tags = a} :: ListUserTagsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListUserTagsResponse where
  rnf ListUserTagsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
