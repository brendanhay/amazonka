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
-- Module      : Amazonka.IAM.ListPolicyTags
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IAM.ListPolicyTags
  ( -- * Creating a Request
    ListPolicyTags (..),
    newListPolicyTags,

    -- * Request Lenses
    listPolicyTags_marker,
    listPolicyTags_maxItems,
    listPolicyTags_policyArn,

    -- * Destructuring the Response
    ListPolicyTagsResponse (..),
    newListPolicyTagsResponse,

    -- * Response Lenses
    listPolicyTagsResponse_marker,
    listPolicyTagsResponse_isTruncated,
    listPolicyTagsResponse_httpStatus,
    listPolicyTagsResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPolicyTags' smart constructor.
data ListPolicyTags = ListPolicyTags'
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
    -- | The ARN of the IAM customer managed policy whose tags you want to see.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPolicyTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listPolicyTags_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'policyArn', 'listPolicyTags_policyArn' - The ARN of the IAM customer managed policy whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListPolicyTags ::
  -- | 'policyArn'
  Prelude.Text ->
  ListPolicyTags
newListPolicyTags pPolicyArn_ =
  ListPolicyTags'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPolicyTags_marker :: Lens.Lens' ListPolicyTags (Prelude.Maybe Prelude.Text)
listPolicyTags_marker = Lens.lens (\ListPolicyTags' {marker} -> marker) (\s@ListPolicyTags' {} a -> s {marker = a} :: ListPolicyTags)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listPolicyTags_maxItems :: Lens.Lens' ListPolicyTags (Prelude.Maybe Prelude.Natural)
listPolicyTags_maxItems = Lens.lens (\ListPolicyTags' {maxItems} -> maxItems) (\s@ListPolicyTags' {} a -> s {maxItems = a} :: ListPolicyTags)

-- | The ARN of the IAM customer managed policy whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listPolicyTags_policyArn :: Lens.Lens' ListPolicyTags Prelude.Text
listPolicyTags_policyArn = Lens.lens (\ListPolicyTags' {policyArn} -> policyArn) (\s@ListPolicyTags' {} a -> s {policyArn = a} :: ListPolicyTags)

instance Core.AWSRequest ListPolicyTags where
  type
    AWSResponse ListPolicyTags =
      ListPolicyTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListPolicyTagsResult"
      ( \s h x ->
          ListPolicyTagsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListPolicyTags where
  hashWithSalt _salt ListPolicyTags' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData ListPolicyTags where
  rnf ListPolicyTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf policyArn

instance Core.ToHeaders ListPolicyTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPolicyTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPolicyTags where
  toQuery ListPolicyTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListPolicyTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems,
        "PolicyArn" Core.=: policyArn
      ]

-- | /See:/ 'newListPolicyTagsResponse' smart constructor.
data ListPolicyTagsResponse = ListPolicyTagsResponse'
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
    -- | The list of tags that are currently attached to the IAM customer managed
    -- policy. Each tag consists of a key name and an associated value. If no
    -- tags are attached to the specified resource, the response contains an
    -- empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPolicyTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'listPolicyTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'httpStatus', 'listPolicyTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listPolicyTagsResponse_tags' - The list of tags that are currently attached to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
newListPolicyTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyTagsResponse
newListPolicyTagsResponse pHttpStatus_ =
  ListPolicyTagsResponse'
    { marker = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listPolicyTagsResponse_marker :: Lens.Lens' ListPolicyTagsResponse (Prelude.Maybe Prelude.Text)
listPolicyTagsResponse_marker = Lens.lens (\ListPolicyTagsResponse' {marker} -> marker) (\s@ListPolicyTagsResponse' {} a -> s {marker = a} :: ListPolicyTagsResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listPolicyTagsResponse_isTruncated :: Lens.Lens' ListPolicyTagsResponse (Prelude.Maybe Prelude.Bool)
listPolicyTagsResponse_isTruncated = Lens.lens (\ListPolicyTagsResponse' {isTruncated} -> isTruncated) (\s@ListPolicyTagsResponse' {} a -> s {isTruncated = a} :: ListPolicyTagsResponse)

-- | The response's http status code.
listPolicyTagsResponse_httpStatus :: Lens.Lens' ListPolicyTagsResponse Prelude.Int
listPolicyTagsResponse_httpStatus = Lens.lens (\ListPolicyTagsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyTagsResponse' {} a -> s {httpStatus = a} :: ListPolicyTagsResponse)

-- | The list of tags that are currently attached to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
listPolicyTagsResponse_tags :: Lens.Lens' ListPolicyTagsResponse [Tag]
listPolicyTagsResponse_tags = Lens.lens (\ListPolicyTagsResponse' {tags} -> tags) (\s@ListPolicyTagsResponse' {} a -> s {tags = a} :: ListPolicyTagsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPolicyTagsResponse where
  rnf ListPolicyTagsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
