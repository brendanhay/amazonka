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
-- Module      : Network.AWS.IAM.ListRoleTags
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IAM.ListRoleTags
  ( -- * Creating a Request
    ListRoleTags (..),
    newListRoleTags,

    -- * Request Lenses
    listRoleTags_maxItems,
    listRoleTags_marker,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
  { -- | (Optional) Use this only when paginating results to indicate the maximum
    -- number of items that you want in the response. If additional items exist
    -- beyond the maximum that you specify, the @IsTruncated@ response element
    -- is @true@.
    --
    -- If you do not include this parameter, it defaults to 100. Note that IAM
    -- might return fewer results, even when more results are available. In
    -- that case, the @IsTruncated@ response element returns @true@, and
    -- @Marker@ contains a value to include in the subsequent call that tells
    -- the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Core.Maybe Core.Text,
    -- | The name of the IAM role for which you want to see the list of tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoleTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listRoleTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
--
-- 'marker', 'listRoleTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'roleName', 'listRoleTags_roleName' - The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListRoleTags ::
  -- | 'roleName'
  Core.Text ->
  ListRoleTags
newListRoleTags pRoleName_ =
  ListRoleTags'
    { maxItems = Core.Nothing,
      marker = Core.Nothing,
      roleName = pRoleName_
    }

-- | (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
listRoleTags_maxItems :: Lens.Lens' ListRoleTags (Core.Maybe Core.Natural)
listRoleTags_maxItems = Lens.lens (\ListRoleTags' {maxItems} -> maxItems) (\s@ListRoleTags' {} a -> s {maxItems = a} :: ListRoleTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listRoleTags_marker :: Lens.Lens' ListRoleTags (Core.Maybe Core.Text)
listRoleTags_marker = Lens.lens (\ListRoleTags' {marker} -> marker) (\s@ListRoleTags' {} a -> s {marker = a} :: ListRoleTags)

-- | The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listRoleTags_roleName :: Lens.Lens' ListRoleTags Core.Text
listRoleTags_roleName = Lens.lens (\ListRoleTags' {roleName} -> roleName) (\s@ListRoleTags' {} a -> s {roleName = a} :: ListRoleTags)

instance Core.AWSRequest ListRoleTags where
  type AWSResponse ListRoleTags = ListRoleTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListRoleTagsResult"
      ( \s h x ->
          ListRoleTagsResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListRoleTags

instance Core.NFData ListRoleTags

instance Core.ToHeaders ListRoleTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListRoleTags where
  toPath = Core.const "/"

instance Core.ToQuery ListRoleTags where
  toQuery ListRoleTags' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListRoleTags" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "RoleName" Core.=: roleName
      ]

-- | /See:/ 'newListRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can use the @Marker@ request parameter to
    -- make a subsequent pagination request that retrieves more items. Note
    -- that IAM might return fewer than the @MaxItems@ number of results even
    -- when more results are available. Check @IsTruncated@ after every call to
    -- ensure that you receive all of your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The list of tags that are currently attached to the role. Each tag
    -- consists of a key name and an associated value. If no tags are attached
    -- to the specified resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoleTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listRoleTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
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
  Core.Int ->
  ListRoleTagsResponse
newListRoleTagsResponse pHttpStatus_ =
  ListRoleTagsResponse'
    { isTruncated = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      tags = Core.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
listRoleTagsResponse_isTruncated :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Core.Bool)
listRoleTagsResponse_isTruncated = Lens.lens (\ListRoleTagsResponse' {isTruncated} -> isTruncated) (\s@ListRoleTagsResponse' {} a -> s {isTruncated = a} :: ListRoleTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listRoleTagsResponse_marker :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Core.Text)
listRoleTagsResponse_marker = Lens.lens (\ListRoleTagsResponse' {marker} -> marker) (\s@ListRoleTagsResponse' {} a -> s {marker = a} :: ListRoleTagsResponse)

-- | The response's http status code.
listRoleTagsResponse_httpStatus :: Lens.Lens' ListRoleTagsResponse Core.Int
listRoleTagsResponse_httpStatus = Lens.lens (\ListRoleTagsResponse' {httpStatus} -> httpStatus) (\s@ListRoleTagsResponse' {} a -> s {httpStatus = a} :: ListRoleTagsResponse)

-- | The list of tags that are currently attached to the role. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
listRoleTagsResponse_tags :: Lens.Lens' ListRoleTagsResponse [Tag]
listRoleTagsResponse_tags = Lens.lens (\ListRoleTagsResponse' {tags} -> tags) (\s@ListRoleTagsResponse' {} a -> s {tags = a} :: ListRoleTagsResponse) Core.. Lens._Coerce

instance Core.NFData ListRoleTagsResponse
