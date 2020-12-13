{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to delete the user pool client.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
  ( -- * Creating a request
    DeleteUserPoolClient (..),
    mkDeleteUserPoolClient,

    -- ** Request lenses
    dupcClientId,
    dupcUserPoolId,

    -- * Destructuring the response
    DeleteUserPoolClientResponse (..),
    mkDeleteUserPoolClientResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete a user pool client.
--
-- /See:/ 'mkDeleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { -- | The app client ID of the app associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The user pool ID for the user pool where you want to delete the client.
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPoolClient' with the minimum fields required to make a request.
--
-- * 'clientId' - The app client ID of the app associated with the user pool.
-- * 'userPoolId' - The user pool ID for the user pool where you want to delete the client.
mkDeleteUserPoolClient ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  DeleteUserPoolClient
mkDeleteUserPoolClient pClientId_ pUserPoolId_ =
  DeleteUserPoolClient'
    { clientId = pClientId_,
      userPoolId = pUserPoolId_
    }

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcClientId :: Lens.Lens' DeleteUserPoolClient (Lude.Sensitive Lude.Text)
dupcClientId = Lens.lens (clientId :: DeleteUserPoolClient -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: DeleteUserPoolClient)
{-# DEPRECATED dupcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user pool ID for the user pool where you want to delete the client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcUserPoolId :: Lens.Lens' DeleteUserPoolClient Lude.Text
dupcUserPoolId = Lens.lens (userPoolId :: DeleteUserPoolClient -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteUserPoolClient)
{-# DEPRECATED dupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest DeleteUserPoolClient where
  type Rs DeleteUserPoolClient = DeleteUserPoolClientResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteUserPoolClientResponse'

instance Lude.ToHeaders DeleteUserPoolClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolClient" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserPoolClient where
  toJSON DeleteUserPoolClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath DeleteUserPoolClient where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserPoolClient where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPoolClientResponse' with the minimum fields required to make a request.
mkDeleteUserPoolClientResponse ::
  DeleteUserPoolClientResponse
mkDeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
