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
-- Module      : Network.AWS.IAM.ListPolicyTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified IAM customer managed
-- policy. The returned list of tags is sorted by tag key. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ListPolicyTags
  ( -- * Creating a Request
    ListPolicyTags (..),
    newListPolicyTags,

    -- * Request Lenses
    listPolicyTags_maxItems,
    listPolicyTags_marker,
    listPolicyTags_policyArn,

    -- * Destructuring the Response
    ListPolicyTagsResponse (..),
    newListPolicyTagsResponse,

    -- * Response Lenses
    listPolicyTagsResponse_isTruncated,
    listPolicyTagsResponse_marker,
    listPolicyTagsResponse_httpStatus,
    listPolicyTagsResponse_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicyTags' smart constructor.
data ListPolicyTags = ListPolicyTags'
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
    -- | The ARN of the IAM customer managed policy whose tags you want to see.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    policyArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPolicyTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listPolicyTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
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
-- 'marker', 'listPolicyTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'policyArn', 'listPolicyTags_policyArn' - The ARN of the IAM customer managed policy whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListPolicyTags ::
  -- | 'policyArn'
  Core.Text ->
  ListPolicyTags
newListPolicyTags pPolicyArn_ =
  ListPolicyTags'
    { maxItems = Core.Nothing,
      marker = Core.Nothing,
      policyArn = pPolicyArn_
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
listPolicyTags_maxItems :: Lens.Lens' ListPolicyTags (Core.Maybe Core.Natural)
listPolicyTags_maxItems = Lens.lens (\ListPolicyTags' {maxItems} -> maxItems) (\s@ListPolicyTags' {} a -> s {maxItems = a} :: ListPolicyTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPolicyTags_marker :: Lens.Lens' ListPolicyTags (Core.Maybe Core.Text)
listPolicyTags_marker = Lens.lens (\ListPolicyTags' {marker} -> marker) (\s@ListPolicyTags' {} a -> s {marker = a} :: ListPolicyTags)

-- | The ARN of the IAM customer managed policy whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listPolicyTags_policyArn :: Lens.Lens' ListPolicyTags Core.Text
listPolicyTags_policyArn = Lens.lens (\ListPolicyTags' {policyArn} -> policyArn) (\s@ListPolicyTags' {} a -> s {policyArn = a} :: ListPolicyTags)

instance Core.AWSRequest ListPolicyTags where
  type
    AWSResponse ListPolicyTags =
      ListPolicyTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPolicyTagsResult"
      ( \s h x ->
          ListPolicyTagsResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListPolicyTags

instance Core.NFData ListPolicyTags

instance Core.ToHeaders ListPolicyTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPolicyTags where
  toPath = Core.const "/"

instance Core.ToQuery ListPolicyTags where
  toQuery ListPolicyTags' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListPolicyTags" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "PolicyArn" Core.=: policyArn
      ]

-- | /See:/ 'newListPolicyTagsResponse' smart constructor.
data ListPolicyTagsResponse = ListPolicyTagsResponse'
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
    -- | The list of tags that are currently attached to the IAM customer managed
    -- policy. Each tag consists of a key name and an associated value. If no
    -- tags are attached to the specified resource, the response contains an
    -- empty list.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPolicyTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listPolicyTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listPolicyTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listPolicyTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listPolicyTagsResponse_tags' - The list of tags that are currently attached to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
newListPolicyTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPolicyTagsResponse
newListPolicyTagsResponse pHttpStatus_ =
  ListPolicyTagsResponse'
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
listPolicyTagsResponse_isTruncated :: Lens.Lens' ListPolicyTagsResponse (Core.Maybe Core.Bool)
listPolicyTagsResponse_isTruncated = Lens.lens (\ListPolicyTagsResponse' {isTruncated} -> isTruncated) (\s@ListPolicyTagsResponse' {} a -> s {isTruncated = a} :: ListPolicyTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listPolicyTagsResponse_marker :: Lens.Lens' ListPolicyTagsResponse (Core.Maybe Core.Text)
listPolicyTagsResponse_marker = Lens.lens (\ListPolicyTagsResponse' {marker} -> marker) (\s@ListPolicyTagsResponse' {} a -> s {marker = a} :: ListPolicyTagsResponse)

-- | The response's http status code.
listPolicyTagsResponse_httpStatus :: Lens.Lens' ListPolicyTagsResponse Core.Int
listPolicyTagsResponse_httpStatus = Lens.lens (\ListPolicyTagsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyTagsResponse' {} a -> s {httpStatus = a} :: ListPolicyTagsResponse)

-- | The list of tags that are currently attached to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
listPolicyTagsResponse_tags :: Lens.Lens' ListPolicyTagsResponse [Tag]
listPolicyTagsResponse_tags = Lens.lens (\ListPolicyTagsResponse' {tags} -> tags) (\s@ListPolicyTagsResponse' {} a -> s {tags = a} :: ListPolicyTagsResponse) Core.. Lens._Coerce

instance Core.NFData ListPolicyTagsResponse
