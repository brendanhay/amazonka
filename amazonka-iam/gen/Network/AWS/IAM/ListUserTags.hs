{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.ListUserTags
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IAM.ListUserTags
  ( -- * Creating a Request
    ListUserTags (..),
    newListUserTags,

    -- * Request Lenses
    listUserTags_maxItems,
    listUserTags_marker,
    listUserTags_userName,

    -- * Destructuring the Response
    ListUserTagsResponse (..),
    newListUserTagsResponse,

    -- * Response Lenses
    listUserTagsResponse_isTruncated,
    listUserTagsResponse_marker,
    listUserTagsResponse_httpStatus,
    listUserTagsResponse_tags,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUserTags' smart constructor.
data ListUserTags = ListUserTags'
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
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM user whose tags you want to see.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListUserTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listUserTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
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
-- 'marker', 'listUserTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'userName', 'listUserTags_userName' - The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListUserTags ::
  -- | 'userName'
  Prelude.Text ->
  ListUserTags
newListUserTags pUserName_ =
  ListUserTags'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      userName = pUserName_
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
listUserTags_maxItems :: Lens.Lens' ListUserTags (Prelude.Maybe Prelude.Natural)
listUserTags_maxItems = Lens.lens (\ListUserTags' {maxItems} -> maxItems) (\s@ListUserTags' {} a -> s {maxItems = a} :: ListUserTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listUserTags_marker :: Lens.Lens' ListUserTags (Prelude.Maybe Prelude.Text)
listUserTags_marker = Lens.lens (\ListUserTags' {marker} -> marker) (\s@ListUserTags' {} a -> s {marker = a} :: ListUserTags)

-- | The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listUserTags_userName :: Lens.Lens' ListUserTags Prelude.Text
listUserTags_userName = Lens.lens (\ListUserTags' {userName} -> userName) (\s@ListUserTags' {} a -> s {userName = a} :: ListUserTags)

instance Prelude.AWSRequest ListUserTags where
  type Rs ListUserTags = ListUserTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListUserTagsResult"
      ( \s h x ->
          ListUserTagsResponse'
            Prelude.<$> (x Prelude..@? "IsTruncated")
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListUserTags

instance Prelude.NFData ListUserTags

instance Prelude.ToHeaders ListUserTags where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListUserTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListUserTags where
  toQuery ListUserTags' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListUserTags" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker,
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newListUserTagsResponse' smart constructor.
data ListUserTagsResponse = ListUserTagsResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can use the @Marker@ request parameter to
    -- make a subsequent pagination request that retrieves more items. Note
    -- that IAM might return fewer than the @MaxItems@ number of results even
    -- when more results are available. Check @IsTruncated@ after every call to
    -- ensure that you receive all of your results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of tags that are currently attached to the user. Each tag
    -- consists of a key name and an associated value. If no tags are attached
    -- to the specified resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListUserTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listUserTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listUserTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
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
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
listUserTagsResponse_isTruncated :: Lens.Lens' ListUserTagsResponse (Prelude.Maybe Prelude.Bool)
listUserTagsResponse_isTruncated = Lens.lens (\ListUserTagsResponse' {isTruncated} -> isTruncated) (\s@ListUserTagsResponse' {} a -> s {isTruncated = a} :: ListUserTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listUserTagsResponse_marker :: Lens.Lens' ListUserTagsResponse (Prelude.Maybe Prelude.Text)
listUserTagsResponse_marker = Lens.lens (\ListUserTagsResponse' {marker} -> marker) (\s@ListUserTagsResponse' {} a -> s {marker = a} :: ListUserTagsResponse)

-- | The response's http status code.
listUserTagsResponse_httpStatus :: Lens.Lens' ListUserTagsResponse Prelude.Int
listUserTagsResponse_httpStatus = Lens.lens (\ListUserTagsResponse' {httpStatus} -> httpStatus) (\s@ListUserTagsResponse' {} a -> s {httpStatus = a} :: ListUserTagsResponse)

-- | The list of tags that are currently attached to the user. Each tag
-- consists of a key name and an associated value. If no tags are attached
-- to the specified resource, the response contains an empty list.
listUserTagsResponse_tags :: Lens.Lens' ListUserTagsResponse [Tag]
listUserTagsResponse_tags = Lens.lens (\ListUserTagsResponse' {tags} -> tags) (\s@ListUserTagsResponse' {} a -> s {tags = a} :: ListUserTagsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListUserTagsResponse
