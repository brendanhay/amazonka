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
-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the
-- specified IAM user. If there is none, the operation returns an empty
-- list.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- If the @UserName@ field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request. This
-- operation works for access keys under the AWS account. Consequently, you
-- can use this operation to manage AWS account root user credentials even
-- if the AWS account has no associated users.
--
-- To ensure the security of your AWS account, the secret access key is
-- accessible only during key and user creation.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccessKeys
  ( -- * Creating a Request
    ListAccessKeys (..),
    newListAccessKeys,

    -- * Request Lenses
    listAccessKeys_userName,
    listAccessKeys_maxItems,
    listAccessKeys_marker,

    -- * Destructuring the Response
    ListAccessKeysResponse (..),
    newListAccessKeysResponse,

    -- * Response Lenses
    listAccessKeysResponse_isTruncated,
    listAccessKeysResponse_marker,
    listAccessKeysResponse_httpStatus,
    listAccessKeysResponse_accessKeyMetadata,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
  { -- | The name of the user.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Core.Maybe Core.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccessKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'listAccessKeys_userName' - The name of the user.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'maxItems', 'listAccessKeys_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listAccessKeys_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListAccessKeys ::
  ListAccessKeys
newListAccessKeys =
  ListAccessKeys'
    { userName = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The name of the user.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAccessKeys_userName :: Lens.Lens' ListAccessKeys (Core.Maybe Core.Text)
listAccessKeys_userName = Lens.lens (\ListAccessKeys' {userName} -> userName) (\s@ListAccessKeys' {} a -> s {userName = a} :: ListAccessKeys)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAccessKeys_maxItems :: Lens.Lens' ListAccessKeys (Core.Maybe Core.Natural)
listAccessKeys_maxItems = Lens.lens (\ListAccessKeys' {maxItems} -> maxItems) (\s@ListAccessKeys' {} a -> s {maxItems = a} :: ListAccessKeys)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAccessKeys_marker :: Lens.Lens' ListAccessKeys (Core.Maybe Core.Text)
listAccessKeys_marker = Lens.lens (\ListAccessKeys' {marker} -> marker) (\s@ListAccessKeys' {} a -> s {marker = a} :: ListAccessKeys)

instance Core.AWSPager ListAccessKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessKeysResponse_isTruncated Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listAccessKeysResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAccessKeys_marker
          Lens..~ rs
          Lens.^? listAccessKeysResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListAccessKeys where
  type
    AWSResponse ListAccessKeys =
      ListAccessKeysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAccessKeysResult"
      ( \s h x ->
          ListAccessKeysResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "AccessKeyMetadata" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListAccessKeys

instance Core.NFData ListAccessKeys

instance Core.ToHeaders ListAccessKeys where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListAccessKeys where
  toPath = Core.const "/"

instance Core.ToQuery ListAccessKeys where
  toQuery ListAccessKeys' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListAccessKeys" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "UserName" Core.=: userName,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListAccessKeys request.
--
-- /See:/ 'newListAccessKeysResponse' smart constructor.
data ListAccessKeysResponse = ListAccessKeysResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects containing metadata about the access keys.
    accessKeyMetadata :: [AccessKeyMetadata]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccessKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listAccessKeysResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listAccessKeysResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAccessKeysResponse_httpStatus' - The response's http status code.
--
-- 'accessKeyMetadata', 'listAccessKeysResponse_accessKeyMetadata' - A list of objects containing metadata about the access keys.
newListAccessKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAccessKeysResponse
newListAccessKeysResponse pHttpStatus_ =
  ListAccessKeysResponse'
    { isTruncated = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      accessKeyMetadata = Core.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAccessKeysResponse_isTruncated :: Lens.Lens' ListAccessKeysResponse (Core.Maybe Core.Bool)
listAccessKeysResponse_isTruncated = Lens.lens (\ListAccessKeysResponse' {isTruncated} -> isTruncated) (\s@ListAccessKeysResponse' {} a -> s {isTruncated = a} :: ListAccessKeysResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAccessKeysResponse_marker :: Lens.Lens' ListAccessKeysResponse (Core.Maybe Core.Text)
listAccessKeysResponse_marker = Lens.lens (\ListAccessKeysResponse' {marker} -> marker) (\s@ListAccessKeysResponse' {} a -> s {marker = a} :: ListAccessKeysResponse)

-- | The response's http status code.
listAccessKeysResponse_httpStatus :: Lens.Lens' ListAccessKeysResponse Core.Int
listAccessKeysResponse_httpStatus = Lens.lens (\ListAccessKeysResponse' {httpStatus} -> httpStatus) (\s@ListAccessKeysResponse' {} a -> s {httpStatus = a} :: ListAccessKeysResponse)

-- | A list of objects containing metadata about the access keys.
listAccessKeysResponse_accessKeyMetadata :: Lens.Lens' ListAccessKeysResponse [AccessKeyMetadata]
listAccessKeysResponse_accessKeyMetadata = Lens.lens (\ListAccessKeysResponse' {accessKeyMetadata} -> accessKeyMetadata) (\s@ListAccessKeysResponse' {} a -> s {accessKeyMetadata = a} :: ListAccessKeysResponse) Core.. Lens._Coerce

instance Core.NFData ListAccessKeysResponse
