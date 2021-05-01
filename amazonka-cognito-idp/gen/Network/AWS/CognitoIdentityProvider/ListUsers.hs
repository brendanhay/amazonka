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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list users.
--
-- /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    paginationToken :: Prelude.Maybe Prelude.Text,
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
    filter' :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of users to be returned.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | An array of strings, where each string is the name of a user attribute
    -- to be returned for each user in the search results. If the array is
    -- null, all attributes are returned.
    attributesToGet :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID for the user pool on which the search should be
    -- performed.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListUsers
newListUsers pUserPoolId_ =
  ListUsers'
    { paginationToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      limit = Prelude.Nothing,
      attributesToGet = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsers_paginationToken :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
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
listUsers_filter :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Text)
listUsers_filter = Lens.lens (\ListUsers' {filter'} -> filter') (\s@ListUsers' {} a -> s {filter' = a} :: ListUsers)

-- | Maximum number of users to be returned.
listUsers_limit :: Lens.Lens' ListUsers (Prelude.Maybe Prelude.Natural)
listUsers_limit = Lens.lens (\ListUsers' {limit} -> limit) (\s@ListUsers' {} a -> s {limit = a} :: ListUsers)

-- | An array of strings, where each string is the name of a user attribute
-- to be returned for each user in the search results. If the array is
-- null, all attributes are returned.
listUsers_attributesToGet :: Lens.Lens' ListUsers (Prelude.Maybe [Prelude.Text])
listUsers_attributesToGet = Lens.lens (\ListUsers' {attributesToGet} -> attributesToGet) (\s@ListUsers' {} a -> s {attributesToGet = a} :: ListUsers) Prelude.. Lens.mapping Prelude._Coerce

-- | The user pool ID for the user pool on which the search should be
-- performed.
listUsers_userPoolId :: Lens.Lens' ListUsers Prelude.Text
listUsers_userPoolId = Lens.lens (\ListUsers' {userPoolId} -> userPoolId) (\s@ListUsers' {} a -> s {userPoolId = a} :: ListUsers)

instance Pager.AWSPager ListUsers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listUsersResponse_paginationToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listUsersResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listUsers_paginationToken
          Lens..~ rs
          Lens.^? listUsersResponse_paginationToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Prelude.<$> (x Prelude..?> "PaginationToken")
            Prelude.<*> (x Prelude..?> "Users" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsers

instance Prelude.NFData ListUsers

instance Prelude.ToHeaders ListUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ListUsers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Prelude..=)
              Prelude.<$> paginationToken,
            ("Filter" Prelude..=) Prelude.<$> filter',
            ("Limit" Prelude..=) Prelude.<$> limit,
            ("AttributesToGet" Prelude..=)
              Prelude.<$> attributesToGet,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId)
          ]
      )

instance Prelude.ToPath ListUsers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListUsers where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the request to list users.
--
-- /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The users returned in the request to list users.
    users :: Prelude.Maybe [UserType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { paginationToken =
        Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersResponse_paginationToken :: Lens.Lens' ListUsersResponse (Prelude.Maybe Prelude.Text)
listUsersResponse_paginationToken = Lens.lens (\ListUsersResponse' {paginationToken} -> paginationToken) (\s@ListUsersResponse' {} a -> s {paginationToken = a} :: ListUsersResponse)

-- | The users returned in the request to list users.
listUsersResponse_users :: Lens.Lens' ListUsersResponse (Prelude.Maybe [UserType])
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Prelude.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Prelude.NFData ListUsersResponse
