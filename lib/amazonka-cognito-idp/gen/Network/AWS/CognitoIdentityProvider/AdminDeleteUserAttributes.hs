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
    aduarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete user attributes as an administrator.
--
-- /See:/ 'mkAdminDeleteUserAttributes' smart constructor.
data AdminDeleteUserAttributes = AdminDeleteUserAttributes'
  { -- | The user pool ID for the user pool where you want to delete user attributes.
    userPoolId :: Lude.Text,
    -- | The user name of the user from which you would like to delete attributes.
    username :: Lude.Sensitive Lude.Text,
    -- | An array of strings representing the user attribute names you wish to delete.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributeNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDeleteUserAttributes' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to delete user attributes.
-- * 'username' - The user name of the user from which you would like to delete attributes.
-- * 'userAttributeNames' - An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
mkAdminDeleteUserAttributes ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminDeleteUserAttributes
mkAdminDeleteUserAttributes pUserPoolId_ pUsername_ =
  AdminDeleteUserAttributes'
    { userPoolId = pUserPoolId_,
      username = pUsername_,
      userAttributeNames = Lude.mempty
    }

-- | The user pool ID for the user pool where you want to delete user attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUserPoolId :: Lens.Lens' AdminDeleteUserAttributes Lude.Text
aduaUserPoolId = Lens.lens (userPoolId :: AdminDeleteUserAttributes -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminDeleteUserAttributes)
{-# DEPRECATED aduaUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user from which you would like to delete attributes.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUsername :: Lens.Lens' AdminDeleteUserAttributes (Lude.Sensitive Lude.Text)
aduaUsername = Lens.lens (username :: AdminDeleteUserAttributes -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminDeleteUserAttributes)
{-# DEPRECATED aduaUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduaUserAttributeNames :: Lens.Lens' AdminDeleteUserAttributes [Lude.Text]
aduaUserAttributeNames = Lens.lens (userAttributeNames :: AdminDeleteUserAttributes -> [Lude.Text]) (\s a -> s {userAttributeNames = a} :: AdminDeleteUserAttributes)
{-# DEPRECATED aduaUserAttributeNames "Use generic-lens or generic-optics with 'userAttributeNames' instead." #-}

instance Lude.AWSRequest AdminDeleteUserAttributes where
  type
    Rs AdminDeleteUserAttributes =
      AdminDeleteUserAttributesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminDeleteUserAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminDeleteUserAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminDeleteUserAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminDeleteUserAttributes where
  toJSON AdminDeleteUserAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("UserAttributeNames" Lude..= userAttributeNames)
          ]
      )

instance Lude.ToPath AdminDeleteUserAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminDeleteUserAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the response received from the server for a request to delete user attributes.
--
-- /See:/ 'mkAdminDeleteUserAttributesResponse' smart constructor.
newtype AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminDeleteUserAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminDeleteUserAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminDeleteUserAttributesResponse
mkAdminDeleteUserAttributesResponse pResponseStatus_ =
  AdminDeleteUserAttributesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduarsResponseStatus :: Lens.Lens' AdminDeleteUserAttributesResponse Lude.Int
aduarsResponseStatus = Lens.lens (responseStatus :: AdminDeleteUserAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminDeleteUserAttributesResponse)
{-# DEPRECATED aduarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
