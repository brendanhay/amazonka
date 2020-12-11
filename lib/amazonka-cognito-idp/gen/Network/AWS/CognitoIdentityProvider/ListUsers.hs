{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the Amazon Cognito user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUsers
  ( -- * Creating a request
    ListUsers (..),
    mkListUsers,

    -- ** Request lenses
    luPaginationToken,
    luAttributesToGet,
    luLimit,
    luFilter,
    luUserPoolId,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lursPaginationToken,
    lursUsers,
    lursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list users.
--
-- /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    attributesToGet :: Lude.Maybe [Lude.Text],
    limit :: Lude.Maybe Lude.Natural,
    filter :: Lude.Maybe Lude.Text,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- * 'attributesToGet' - An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
-- * 'filter' - A filter string of the form "/AttributeName/ /Filter-Type/ "/AttributeValue/ "". Quotation marks within the filter string must be escaped using the backslash (\) character. For example, "@family_name@ = \"Reddy\"".
--
--
--     * /AttributeName/ : The name of the attribute to search for. You can only search for one attribute at a time.
--
--
--     * /Filter-Type/ : For an exact match, use =, for example, "@given_name@ = \"Jon\"". For a prefix ("starts with") match, use ^=, for example, "@given_name@ ^= \"Jon\"".
--
--
--     * /AttributeValue/ : The attribute value that must be matched for each user.
--
--
-- If the filter string is empty, @ListUsers@ returns all users in the user pool.
-- You can only search for the following standard attributes:
--
--     * @username@ (case-sensitive)
--
--
--     * @email@
--
--
--     * @phone_number@
--
--
--     * @name@
--
--
--     * @given_name@
--
--
--     * @family_name@
--
--
--     * @preferred_username@
--
--
--     * @cognito:user_status@ (called __Status__ in the Console) (case-insensitive)
--
--
--     * @status (called __Enabled__ in the Console) (case-sensitive)@
--
--
--     * @sub@
--
--
-- Custom attributes are not searchable.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API> and <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API> in the /Amazon Cognito Developer Guide/ .
-- * 'limit' - Maximum number of users to be returned.
-- * 'paginationToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'userPoolId' - The user pool ID for the user pool on which the search should be performed.
mkListUsers ::
  -- | 'userPoolId'
  Lude.Text ->
  ListUsers
mkListUsers pUserPoolId_ =
  ListUsers'
    { paginationToken = Lude.Nothing,
      attributesToGet = Lude.Nothing,
      limit = Lude.Nothing,
      filter = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luPaginationToken :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luPaginationToken = Lens.lens (paginationToken :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListUsers)
{-# DEPRECATED luPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luAttributesToGet :: Lens.Lens' ListUsers (Lude.Maybe [Lude.Text])
luAttributesToGet = Lens.lens (attributesToGet :: ListUsers -> Lude.Maybe [Lude.Text]) (\s a -> s {attributesToGet = a} :: ListUsers)
{-# DEPRECATED luAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

-- | Maximum number of users to be returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luLimit :: Lens.Lens' ListUsers (Lude.Maybe Lude.Natural)
luLimit = Lens.lens (limit :: ListUsers -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListUsers)
{-# DEPRECATED luLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A filter string of the form "/AttributeName/ /Filter-Type/ "/AttributeValue/ "". Quotation marks within the filter string must be escaped using the backslash (\) character. For example, "@family_name@ = \"Reddy\"".
--
--
--     * /AttributeName/ : The name of the attribute to search for. You can only search for one attribute at a time.
--
--
--     * /Filter-Type/ : For an exact match, use =, for example, "@given_name@ = \"Jon\"". For a prefix ("starts with") match, use ^=, for example, "@given_name@ ^= \"Jon\"".
--
--
--     * /AttributeValue/ : The attribute value that must be matched for each user.
--
--
-- If the filter string is empty, @ListUsers@ returns all users in the user pool.
-- You can only search for the following standard attributes:
--
--     * @username@ (case-sensitive)
--
--
--     * @email@
--
--
--     * @phone_number@
--
--
--     * @name@
--
--
--     * @given_name@
--
--
--     * @family_name@
--
--
--     * @preferred_username@
--
--
--     * @cognito:user_status@ (called __Status__ in the Console) (case-insensitive)
--
--
--     * @status (called __Enabled__ in the Console) (case-sensitive)@
--
--
--     * @sub@
--
--
-- Custom attributes are not searchable.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API> and <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luFilter :: Lens.Lens' ListUsers (Lude.Maybe Lude.Text)
luFilter = Lens.lens (filter :: ListUsers -> Lude.Maybe Lude.Text) (\s a -> s {filter = a} :: ListUsers)
{-# DEPRECATED luFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The user pool ID for the user pool on which the search should be performed.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luUserPoolId :: Lens.Lens' ListUsers Lude.Text
luUserPoolId = Lens.lens (userPoolId :: ListUsers -> Lude.Text) (\s a -> s {userPoolId = a} :: ListUsers)
{-# DEPRECATED luUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Page.AWSPager ListUsers where
  page rq rs
    | Page.stop (rs Lens.^. lursPaginationToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luPaginationToken Lens..~ rs Lens.^. lursPaginationToken

instance Lude.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityProviderService.ListUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            ("AttributesToGet" Lude..=) Lude.<$> attributesToGet,
            ("Limit" Lude..=) Lude.<$> limit,
            ("Filter" Lude..=) Lude.<$> filter,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath ListUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUsers where
  toQuery = Lude.const Lude.mempty

-- | The response from the request to list users.
--
-- /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    users :: Lude.Maybe [UserType],
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'users' - The users returned in the request to list users.
mkListUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUsersResponse
mkListUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { paginationToken = Lude.Nothing,
      users = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursPaginationToken :: Lens.Lens' ListUsersResponse (Lude.Maybe Lude.Text)
lursPaginationToken = Lens.lens (paginationToken :: ListUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: ListUsersResponse)
{-# DEPRECATED lursPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The users returned in the request to list users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUsers :: Lens.Lens' ListUsersResponse (Lude.Maybe [UserType])
lursUsers = Lens.lens (users :: ListUsersResponse -> Lude.Maybe [UserType]) (\s a -> s {users = a} :: ListUsersResponse)
{-# DEPRECATED lursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUsersResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUsersResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
