{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the user attributes in a user pool as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
  ( -- * Creating a request
    AdminDeleteUserAttributes (..),
    mkAdminDeleteUserAttributes,

    -- ** Request lenses
    aduaUserPoolId,
    aduaUsername,
    aduaUserAttributeNames,

    -- * Destructuring the response
    AdminDeleteUserAttributesResponse (..),
    mkAdminDeleteUserAttributesResponse,

    -- ** Response lenses
    aduarrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete user attributes as an administrator.
--
-- /See:/ 'mkAdminDeleteUserAttributes' smart constructor.
data AdminDeleteUserAttributes = AdminDeleteUserAttributes'
  { -- | The user pool ID for the user pool where you want to delete user attributes.
    userPoolId :: Types.UserPoolIdType,
    -- | The user name of the user from which you would like to delete attributes.
    username :: Types.UsernameType,
    -- | An array of strings representing the user attribute names you wish to delete.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributeNames :: [Types.AttributeNameType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDeleteUserAttributes' value with any optional fields omitted.
mkAdminDeleteUserAttributes ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  -- | 'username'
  Types.UsernameType ->
  AdminDeleteUserAttributes
mkAdminDeleteUserAttributes userPoolId username =
  AdminDeleteUserAttributes'
    { userPoolId,
      username,
      userAttributeNames = Core.mempty
    }

-- | The user pool ID for the user pool where you want to delete user attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUserPoolId :: Lens.Lens' AdminDeleteUserAttributes Types.UserPoolIdType
aduaUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED aduaUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user from which you would like to delete attributes.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUsername :: Lens.Lens' AdminDeleteUserAttributes Types.UsernameType
aduaUsername = Lens.field @"username"
{-# DEPRECATED aduaUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUserAttributeNames :: Lens.Lens' AdminDeleteUserAttributes [Types.AttributeNameType]
aduaUserAttributeNames = Lens.field @"userAttributeNames"
{-# DEPRECATED aduaUserAttributeNames "Use generic-lens or generic-optics with 'userAttributeNames' instead." #-}

instance Core.FromJSON AdminDeleteUserAttributes where
  toJSON AdminDeleteUserAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("UserAttributeNames" Core..= userAttributeNames)
          ]
      )

instance Core.AWSRequest AdminDeleteUserAttributes where
  type
    Rs AdminDeleteUserAttributes =
      AdminDeleteUserAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminDeleteUserAttributes"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminDeleteUserAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response received from the server for a request to delete user attributes.
--
-- /See:/ 'mkAdminDeleteUserAttributesResponse' smart constructor.
newtype AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDeleteUserAttributesResponse' value with any optional fields omitted.
mkAdminDeleteUserAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminDeleteUserAttributesResponse
mkAdminDeleteUserAttributesResponse responseStatus =
  AdminDeleteUserAttributesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduarrsResponseStatus :: Lens.Lens' AdminDeleteUserAttributesResponse Core.Int
aduarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aduarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
