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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the Amazon Cognito user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_paginationToken,
    listUsers_filter,
    listUsers_limit,
    listUsers_attributesToGet,
    listUsers_userPoolId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_paginationToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list users.
--
-- /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    paginationToken :: Core.Maybe Core.Text,
    -- | A filter string of the form \"/AttributeName/ /Filter-Type/
    -- \"/AttributeValue/\"\". Quotation marks within the filter string must be
    -- escaped using the backslash (\\) character. For example, \"@family_name@
    -- = \\\"Reddy\\\"\".
    --
    -- -   /AttributeName/: The name of the attribute to search for. You can
    --     only search for one attribute at a time.
    --
    -- -   /Filter-Type/: For an exact match, use =, for example,
    --     \"@given_name@ = \\\"Jon\\\"\". For a prefix (\"starts with\")
    --     match, use ^=, for example, \"@given_name@ ^= \\\"Jon\\\"\".
    --
    -- -   /AttributeValue/: The attribute value that must be matched for each
    --     user.
    --
    -- If the filter string is empty, @ListUsers@ returns all users in the user
    -- pool.
    --
    -- You can only search for the following standard attributes:
    --
    -- -   @username@ (case-sensitive)
    --
    -- -   @email@
    --
    -- -   @phone_number@
    --
    -- -   @name@
    --
    -- -   @given_name@
    --
    -- -   @family_name@
    --
    -- -   @preferred_username@
    --
    -- -   @cognito:user_status@ (called __Status__ in the Console)
    --     (case-insensitive)
    --
    -- -   @status (called Enabled in the Console) (case-sensitive)@
    --
    -- -   @sub@
    --
    -- Custom attributes are not searchable.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API>
    -- and
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API>
    -- in the /Amazon Cognito Developer Guide/.
    filter' :: Core.Maybe Core.Text,
    -- | Maximum number of users to be returned.
    limit :: Core.Maybe Core.Natural,
    -- | An array of strings, where each string is the name of a user attribute
    -- to be returned for each user in the search results. If the array is
    -- null, all attributes are returned.
    attributesToGet :: Core.Maybe [Core.Text],
    -- | The user pool ID for the user pool on which the search should be
    -- performed.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'listUsers_paginationToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'filter'', 'listUsers_filter' - A filter string of the form \"/AttributeName/ /Filter-Type/
-- \"/AttributeValue/\"\". Quotation marks within the filter string must be
-- escaped using the backslash (\\) character. For example, \"@family_name@
-- = \\\"Reddy\\\"\".
--
-- -   /AttributeName/: The name of the attribute to search for. You can
--     only search for one attribute at a time.
--
-- -   /Filter-Type/: For an exact match, use =, for example,
--     \"@given_name@ = \\\"Jon\\\"\". For a prefix (\"starts with\")
--     match, use ^=, for example, \"@given_name@ ^= \\\"Jon\\\"\".
--
-- -   /AttributeValue/: The attribute value that must be matched for each
--     user.
--
-- If the filter string is empty, @ListUsers@ returns all users in the user
-- pool.
--
-- You can only search for the following standard attributes:
--
-- -   @username@ (case-sensitive)
--
-- -   @email@
--
-- -   @phone_number@
--
-- -   @name@
--
-- -   @given_name@
--
-- -   @family_name@
--
-- -   @preferred_username@
--
-- -   @cognito:user_status@ (called __Status__ in the Console)
--     (case-insensitive)
--
-- -   @status (called Enabled in the Console) (case-sensitive)@
--
-- -   @sub@
--
-- Custom attributes are not searchable.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API>
-- and
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API>
-- in the /Amazon Cognito Developer Guide/.
--
-- 'limit', 'listUsers_limit' - Maximum number of users to be returned.
--
-- 'attributesToGet', 'listUsers_attributesToGet' - An array of strings, where each string is the name of a user attribute
-- to be returned for each user in the search results. If the array is
-- null, all attributes are returned.
--
-- 'userPoolId', 'listUsers_userPoolId' - The user pool ID for the user pool on which the search should be
-- performed.
newListUsers ::
  -- | 'userPoolId'
  Core.Text ->
  ListUsers
newListUsers pUserPoolId_ =
  ListUsers'
    { paginationToken = Core.Nothing,
      filter' = Core.Nothing,
      limit = Core.Nothing,
      attributesToGet = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsers_paginationToken :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
listUsers_paginationToken = Lens.lens (\ListUsers' {paginationToken} -> paginationToken) (\s@ListUsers' {} a -> s {paginationToken = a} :: ListUsers)

-- | A filter string of the form \"/AttributeName/ /Filter-Type/
-- \"/AttributeValue/\"\". Quotation marks within the filter string must be
-- escaped using the backslash (\\) character. For example, \"@family_name@
-- = \\\"Reddy\\\"\".
--
-- -   /AttributeName/: The name of the attribute to search for. You can
--     only search for one attribute at a time.
--
-- -   /Filter-Type/: For an exact match, use =, for example,
--     \"@given_name@ = \\\"Jon\\\"\". For a prefix (\"starts with\")
--     match, use ^=, for example, \"@given_name@ ^= \\\"Jon\\\"\".
--
-- -   /AttributeValue/: The attribute value that must be matched for each
--     user.
--
-- If the filter string is empty, @ListUsers@ returns all users in the user
-- pool.
--
-- You can only search for the following standard attributes:
--
-- -   @username@ (case-sensitive)
--
-- -   @email@
--
-- -   @phone_number@
--
-- -   @name@
--
-- -   @given_name@
--
-- -   @family_name@
--
-- -   @preferred_username@
--
-- -   @cognito:user_status@ (called __Status__ in the Console)
--     (case-insensitive)
--
-- -   @status (called Enabled in the Console) (case-sensitive)@
--
-- -   @sub@
--
-- Custom attributes are not searchable.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API>
-- and
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API>
-- in the /Amazon Cognito Developer Guide/.
listUsers_filter :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
listUsers_filter = Lens.lens (\ListUsers' {filter'} -> filter') (\s@ListUsers' {} a -> s {filter' = a} :: ListUsers)

-- | Maximum number of users to be returned.
listUsers_limit :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
listUsers_limit = Lens.lens (\ListUsers' {limit} -> limit) (\s@ListUsers' {} a -> s {limit = a} :: ListUsers)

-- | An array of strings, where each string is the name of a user attribute
-- to be returned for each user in the search results. If the array is
-- null, all attributes are returned.
listUsers_attributesToGet :: Lens.Lens' ListUsers (Core.Maybe [Core.Text])
listUsers_attributesToGet = Lens.lens (\ListUsers' {attributesToGet} -> attributesToGet) (\s@ListUsers' {} a -> s {attributesToGet = a} :: ListUsers) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID for the user pool on which the search should be
-- performed.
listUsers_userPoolId :: Lens.Lens' ListUsers Core.Text
listUsers_userPoolId = Lens.lens (\ListUsers' {userPoolId} -> userPoolId) (\s@ListUsers' {} a -> s {userPoolId = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_paginationToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_users Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUsers_paginationToken
          Lens..~ rs
          Lens.^? listUsersResponse_paginationToken Core.. Lens._Just

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..?> "PaginationToken")
            Core.<*> (x Core..?> "Users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUsers

instance Core.NFData ListUsers

instance Core.ToHeaders ListUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUsers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken,
            ("Filter" Core..=) Core.<$> filter',
            ("Limit" Core..=) Core.<$> limit,
            ("AttributesToGet" Core..=) Core.<$> attributesToGet,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath ListUsers where
  toPath = Core.const "/"

instance Core.ToQuery ListUsers where
  toQuery = Core.const Core.mempty

-- | The response from the request to list users.
--
-- /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    paginationToken :: Core.Maybe Core.Text,
    -- | The users returned in the request to list users.
    users :: Core.Maybe [UserType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'listUsersResponse_paginationToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'users', 'listUsersResponse_users' - The users returned in the request to list users.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
newListUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { paginationToken = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersResponse_paginationToken :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
listUsersResponse_paginationToken = Lens.lens (\ListUsersResponse' {paginationToken} -> paginationToken) (\s@ListUsersResponse' {} a -> s {paginationToken = a} :: ListUsersResponse)

-- | The users returned in the request to list users.
listUsersResponse_users :: Lens.Lens' ListUsersResponse (Core.Maybe [UserType])
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Core.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Core.NFData ListUsersResponse
