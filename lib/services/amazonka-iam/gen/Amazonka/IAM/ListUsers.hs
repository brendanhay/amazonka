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
-- Module      : Amazonka.IAM.ListUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM users that have the specified path prefix. If no path
-- prefix is specified, the operation returns all users in the Amazon Web
-- Services account. If there are none, the operation returns an empty
-- list.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for a user, see GetUser.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_marker,
    listUsers_maxItems,
    listUsers_pathPrefix,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_isTruncated,
    listUsersResponse_marker,
    listUsersResponse_httpStatus,
    listUsersResponse_users,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
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
    -- | The path prefix for filtering the results. For example:
    -- @\/division_abc\/subdivision_xyz\/@, which would get all user names
    -- whose path starts with @\/division_abc\/subdivision_xyz\/@.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/), listing all user names. This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    pathPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listUsers_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listUsers_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'pathPrefix', 'listUsers_pathPrefix' - The path prefix for filtering the results. For example:
-- @\/division_abc\/subdivision_xyz\/@, which would get all user names
-- whose path starts with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all user names. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
newListUsers ::
  ListUsers
newListUsers =
  ListUsers'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      pathPrefix = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listUsers_marker :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_marker = Lens.lens (\ListUsers' {marker} -> marker) (\s@ListUsers' {} a -> s {marker = a} :: ListUsers)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listUsers_maxItems :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Natural)
listUsers_maxItems = Lens.lens (\ListUsers' {maxItems} -> maxItems) (\s@ListUsers' {} a -> s {maxItems = a} :: ListUsers)

-- | The path prefix for filtering the results. For example:
-- @\/division_abc\/subdivision_xyz\/@, which would get all user names
-- whose path starts with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all user names. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
listUsers_pathPrefix :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_pathPrefix = Lens.lens (\ListUsers' {pathPrefix} -> pathPrefix) (\s@ListUsers' {} a -> s {pathPrefix = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listUsersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listUsers_marker
          Lens..~ rs
          Lens.^? listUsersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListUsersResult"
      ( \s h x ->
          ListUsersResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "Users"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListUsers where
  hashWithSalt _salt ListUsers' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` pathPrefix

instance Prelude.NFData ListUsers where
  rnf ListUsers' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf pathPrefix

instance Data.ToHeaders ListUsers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListUsers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PathPrefix" Data.=: pathPrefix
      ]

-- | Contains the response to a successful ListUsers request.
--
-- /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
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
    -- | A list of users.
    users :: [User]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listUsersResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listUsersResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
--
-- 'users', 'listUsersResponse_users' - A list of users.
newListUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      users = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listUsersResponse_isTruncated :: Lens.Lens' ListUsersResponse (Prelude.Maybe Prelude.Bool)
listUsersResponse_isTruncated = Lens.lens (\ListUsersResponse' {isTruncated} -> isTruncated) (\s@ListUsersResponse' {} a -> s {isTruncated = a} :: ListUsersResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listUsersResponse_marker :: Lens.Lens' ListUsersResponse (Prelude.Maybe Prelude.Text)
listUsersResponse_marker = Lens.lens (\ListUsersResponse' {marker} -> marker) (\s@ListUsersResponse' {} a -> s {marker = a} :: ListUsersResponse)

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Prelude.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

-- | A list of users.
listUsersResponse_users :: Lens.Lens' ListUsersResponse [User]
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListUsersResponse where
  rnf ListUsersResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf users
