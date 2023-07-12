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
-- Module      : Amazonka.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- If the @UserName@ is not specified, the user name is determined
-- implicitly based on the Amazon Web Services access key ID used to sign
-- the request. If a temporary access key is used, then @UserName@ is
-- required. If a long-term key is assigned to the user, then @UserName@ is
-- not required. This operation works for access keys under the Amazon Web
-- Services account. Consequently, you can use this operation to manage
-- Amazon Web Services account root user credentials even if the Amazon Web
-- Services account has no associated users.
--
-- To ensure the security of your Amazon Web Services account, the secret
-- access key is accessible only during key and user creation.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListAccessKeys
  ( -- * Creating a Request
    ListAccessKeys (..),
    newListAccessKeys,

    -- * Request Lenses
    listAccessKeys_marker,
    listAccessKeys_maxItems,
    listAccessKeys_userName,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
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
    -- | The name of the user.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listAccessKeys_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
-- 'userName', 'listAccessKeys_userName' - The name of the user.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListAccessKeys ::
  ListAccessKeys
newListAccessKeys =
  ListAccessKeys'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAccessKeys_marker :: Lens.Lens' ListAccessKeys (Prelude.Maybe Prelude.Text)
listAccessKeys_marker = Lens.lens (\ListAccessKeys' {marker} -> marker) (\s@ListAccessKeys' {} a -> s {marker = a} :: ListAccessKeys)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAccessKeys_maxItems :: Lens.Lens' ListAccessKeys (Prelude.Maybe Prelude.Natural)
listAccessKeys_maxItems = Lens.lens (\ListAccessKeys' {maxItems} -> maxItems) (\s@ListAccessKeys' {} a -> s {maxItems = a} :: ListAccessKeys)

-- | The name of the user.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAccessKeys_userName :: Lens.Lens' ListAccessKeys (Prelude.Maybe Prelude.Text)
listAccessKeys_userName = Lens.lens (\ListAccessKeys' {userName} -> userName) (\s@ListAccessKeys' {} a -> s {userName = a} :: ListAccessKeys)

instance Core.AWSPager ListAccessKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessKeysResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listAccessKeysResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAccessKeys_marker
          Lens..~ rs
          Lens.^? listAccessKeysResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListAccessKeys where
  type
    AWSResponse ListAccessKeys =
      ListAccessKeysResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListAccessKeysResult"
      ( \s h x ->
          ListAccessKeysResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "AccessKeyMetadata"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListAccessKeys where
  hashWithSalt _salt ListAccessKeys' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListAccessKeys where
  rnf ListAccessKeys' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ListAccessKeys where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAccessKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccessKeys where
  toQuery ListAccessKeys' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListAccessKeys" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "UserName" Data.=: userName
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
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing metadata about the access keys.
    accessKeyMetadata :: [AccessKeyMetadata]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAccessKeysResponse
newListAccessKeysResponse pHttpStatus_ =
  ListAccessKeysResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accessKeyMetadata = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAccessKeysResponse_isTruncated :: Lens.Lens' ListAccessKeysResponse (Prelude.Maybe Prelude.Bool)
listAccessKeysResponse_isTruncated = Lens.lens (\ListAccessKeysResponse' {isTruncated} -> isTruncated) (\s@ListAccessKeysResponse' {} a -> s {isTruncated = a} :: ListAccessKeysResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAccessKeysResponse_marker :: Lens.Lens' ListAccessKeysResponse (Prelude.Maybe Prelude.Text)
listAccessKeysResponse_marker = Lens.lens (\ListAccessKeysResponse' {marker} -> marker) (\s@ListAccessKeysResponse' {} a -> s {marker = a} :: ListAccessKeysResponse)

-- | The response's http status code.
listAccessKeysResponse_httpStatus :: Lens.Lens' ListAccessKeysResponse Prelude.Int
listAccessKeysResponse_httpStatus = Lens.lens (\ListAccessKeysResponse' {httpStatus} -> httpStatus) (\s@ListAccessKeysResponse' {} a -> s {httpStatus = a} :: ListAccessKeysResponse)

-- | A list of objects containing metadata about the access keys.
listAccessKeysResponse_accessKeyMetadata :: Lens.Lens' ListAccessKeysResponse [AccessKeyMetadata]
listAccessKeysResponse_accessKeyMetadata = Lens.lens (\ListAccessKeysResponse' {accessKeyMetadata} -> accessKeyMetadata) (\s@ListAccessKeysResponse' {} a -> s {accessKeyMetadata = a} :: ListAccessKeysResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAccessKeysResponse where
  rnf ListAccessKeysResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessKeyMetadata
