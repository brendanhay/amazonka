{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the attributes for a user.
module Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
  ( -- * Creating a request
    DeleteUserAttributes (..),
    mkDeleteUserAttributes,

    -- ** Request lenses
    duaAccessToken,
    duaUserAttributeNames,

    -- * Destructuring the response
    DeleteUserAttributesResponse (..),
    mkDeleteUserAttributesResponse,

    -- ** Response lenses
    duarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete user attributes.
--
-- /See:/ 'mkDeleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
  { -- | The access token used in the request to delete user attributes.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | An array of strings representing the user attribute names you wish to delete.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributeNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserAttributes' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token used in the request to delete user attributes.
-- * 'userAttributeNames' - An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
mkDeleteUserAttributes ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  DeleteUserAttributes
mkDeleteUserAttributes pAccessToken_ =
  DeleteUserAttributes'
    { accessToken = pAccessToken_,
      userAttributeNames = Lude.mempty
    }

-- | The access token used in the request to delete user attributes.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaAccessToken :: Lens.Lens' DeleteUserAttributes (Lude.Sensitive Lude.Text)
duaAccessToken = Lens.lens (accessToken :: DeleteUserAttributes -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: DeleteUserAttributes)
{-# DEPRECATED duaAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaUserAttributeNames :: Lens.Lens' DeleteUserAttributes [Lude.Text]
duaUserAttributeNames = Lens.lens (userAttributeNames :: DeleteUserAttributes -> [Lude.Text]) (\s a -> s {userAttributeNames = a} :: DeleteUserAttributes)
{-# DEPRECATED duaUserAttributeNames "Use generic-lens or generic-optics with 'userAttributeNames' instead." #-}

instance Lude.AWSRequest DeleteUserAttributes where
  type Rs DeleteUserAttributes = DeleteUserAttributesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUserAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteUserAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserAttributes where
  toJSON DeleteUserAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("UserAttributeNames" Lude..= userAttributeNames)
          ]
      )

instance Lude.ToPath DeleteUserAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to delete user attributes.
--
-- /See:/ 'mkDeleteUserAttributesResponse' smart constructor.
newtype DeleteUserAttributesResponse = DeleteUserAttributesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUserAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserAttributesResponse
mkDeleteUserAttributesResponse pResponseStatus_ =
  DeleteUserAttributesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarsResponseStatus :: Lens.Lens' DeleteUserAttributesResponse Lude.Int
duarsResponseStatus = Lens.lens (responseStatus :: DeleteUserAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserAttributesResponse)
{-# DEPRECATED duarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
