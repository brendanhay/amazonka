{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    luUserPoolId,
    luAttributesToGet,
    luFilter,
    luLimit,
    luPaginationToken,

    -- * Destructuring the response
    ListUsersResponse (..),
    mkListUsersResponse,

    -- ** Response lenses
    lurrsPaginationToken,
    lurrsUsers,
    lurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list users.
--
-- /See:/ 'mkListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The user pool ID for the user pool on which the search should be performed.
    userPoolId :: Types.UserPoolIdType,
    -- | An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
    attributesToGet :: Core.Maybe [Types.AttributeNameType],
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
    filter :: Core.Maybe Types.UserFilterType,
    -- | Maximum number of users to be returned.
    limit :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    paginationToken :: Core.Maybe Types.SearchPaginationTokenType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsers' value with any optional fields omitted.
mkListUsers ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  ListUsers
mkListUsers userPoolId =
  ListUsers'
    { userPoolId,
      attributesToGet = Core.Nothing,
      filter = Core.Nothing,
      limit = Core.Nothing,
      paginationToken = Core.Nothing
    }

-- | The user pool ID for the user pool on which the search should be performed.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luUserPoolId :: Lens.Lens' ListUsers Types.UserPoolIdType
luUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED luUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luAttributesToGet :: Lens.Lens' ListUsers (Core.Maybe [Types.AttributeNameType])
luAttributesToGet = Lens.field @"attributesToGet"
{-# DEPRECATED luAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

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
luFilter :: Lens.Lens' ListUsers (Core.Maybe Types.UserFilterType)
luFilter = Lens.field @"filter"
{-# DEPRECATED luFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Maximum number of users to be returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luLimit :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
luLimit = Lens.field @"limit"
{-# DEPRECATED luLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luPaginationToken :: Lens.Lens' ListUsers (Core.Maybe Types.SearchPaginationTokenType)
luPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED luPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

instance Core.FromJSON ListUsers where
  toJSON ListUsers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            ("AttributesToGet" Core..=) Core.<$> attributesToGet,
            ("Filter" Core..=) Core.<$> filter,
            ("Limit" Core..=) Core.<$> limit,
            ("PaginationToken" Core..=) Core.<$> paginationToken
          ]
      )

instance Core.AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.ListUsers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..:? "PaginationToken")
            Core.<*> (x Core..:? "Users")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListUsers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"paginationToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"paginationToken"
            Lens..~ rs Lens.^. Lens.field @"paginationToken"
        )

-- | The response from the request to list users.
--
-- /See:/ 'mkListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    paginationToken :: Core.Maybe Types.PaginationToken,
    -- | The users returned in the request to list users.
    users :: Core.Maybe [Types.UserType],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListUsersResponse' value with any optional fields omitted.
mkListUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUsersResponse
mkListUsersResponse responseStatus =
  ListUsersResponse'
    { paginationToken = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsPaginationToken :: Lens.Lens' ListUsersResponse (Core.Maybe Types.PaginationToken)
lurrsPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED lurrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The users returned in the request to list users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsUsers :: Lens.Lens' ListUsersResponse (Core.Maybe [Types.UserType])
lurrsUsers = Lens.field @"users"
{-# DEPRECATED lurrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsResponseStatus :: Lens.Lens' ListUsersResponse Core.Int
lurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
