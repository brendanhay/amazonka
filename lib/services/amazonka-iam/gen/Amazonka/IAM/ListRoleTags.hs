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
-- Module      : Amazonka.IAM.ListRoleTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified role. The returned
-- list of tags is sorted by tag key. For more information about tagging,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.ListRoleTags
  ( -- * Creating a Request
    ListRoleTags (..),
    newListRoleTags,

    -- * Request Lenses
    listRoleTags_marker,
    listRoleTags_maxItems,
    listRoleTags_roleName,

    -- * Destructuring the Response
    ListRoleTagsResponse (..),
    newListRoleTagsResponse,

    -- * Response Lenses
    listRoleTagsResponse_isTruncated,
    listRoleTagsResponse_marker,
    listRoleTagsResponse_httpStatus,
    listRoleTagsResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
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
    -- | The name of the IAM role for which you want to see the list of tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoleTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listRoleTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listRoleTags_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'roleName', 'listRoleTags_roleName' - The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListRoleTags ::
  -- | 'roleName'
  Prelude.Text ->
  ListRoleTags
newListRoleTags pRoleName_ =
  ListRoleTags'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      roleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listRoleTags_marker :: Lens.Lens' ListRoleTags (Prelude.Maybe Prelude.Text)
listRoleTags_marker = Lens.lens (\ListRoleTags' {marker} -> marker) (\s@ListRoleTags' {} a -> s {marker = a} :: ListRoleTags)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listRoleTags_maxItems :: Lens.Lens' ListRoleTags (Prelude.Maybe Prelude.Natural)
listRoleTags_maxItems = Lens.lens (\ListRoleTags' {maxItems} -> maxItems) (\s@ListRoleTags' {} a -> s {maxItems = a} :: ListRoleTags)

-- | The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listRoleTags_roleName :: Lens.Lens' ListRoleTags Prelude.Text
listRoleTags_roleName = Lens.lens (\ListRoleTags' {roleName} -> roleName) (\s@ListRoleTags' {} a -> s {roleName = a} :: ListRoleTags)

instance Core.AWSRequest ListRoleTags where
  type AWSResponse ListRoleTags = ListRoleTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListRoleTagsResult"
      ( \s h x ->
          ListRoleTagsResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "Tags"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListRoleTags where
  hashWithSalt _salt ListRoleTags' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData ListRoleTags where
  rnf ListRoleTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf roleName

instance Data.ToHeaders ListRoleTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListRoleTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRoleTags where
  toQuery ListRoleTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListRoleTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "RoleName" Data.=: roleName
      ]

-- | /See:/ 'newListRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of tags that are currently attached to the role. Each tag
    -- consists of a key name and an associated value. If no tags are attached
    -- to the specified resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoleTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listRoleTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listRoleTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listRoleTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listRoleTagsResponse_tags' - The list of tags that are currently attached to the role. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
newListRoleTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoleTagsResponse
newListRoleTagsResponse pHttpStatus_ =
  ListRoleTagsResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listRoleTagsResponse_isTruncated :: Lens.Lens' ListRoleTagsResponse (Prelude.Maybe Prelude.Bool)
listRoleTagsResponse_isTruncated = Lens.lens (\ListRoleTagsResponse' {isTruncated} -> isTruncated) (\s@ListRoleTagsResponse' {} a -> s {isTruncated = a} :: ListRoleTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listRoleTagsResponse_marker :: Lens.Lens' ListRoleTagsResponse (Prelude.Maybe Prelude.Text)
listRoleTagsResponse_marker = Lens.lens (\ListRoleTagsResponse' {marker} -> marker) (\s@ListRoleTagsResponse' {} a -> s {marker = a} :: ListRoleTagsResponse)

-- | The response's http status code.
listRoleTagsResponse_httpStatus :: Lens.Lens' ListRoleTagsResponse Prelude.Int
listRoleTagsResponse_httpStatus = Lens.lens (\ListRoleTagsResponse' {httpStatus} -> httpStatus) (\s@ListRoleTagsResponse' {} a -> s {httpStatus = a} :: ListRoleTagsResponse)

-- | The list of tags that are currently attached to the role. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
listRoleTagsResponse_tags :: Lens.Lens' ListRoleTagsResponse [Tag]
listRoleTagsResponse_tags = Lens.lens (\ListRoleTagsResponse' {tags} -> tags) (\s@ListRoleTagsResponse' {} a -> s {tags = a} :: ListRoleTagsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRoleTagsResponse where
  rnf ListRoleTagsResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
