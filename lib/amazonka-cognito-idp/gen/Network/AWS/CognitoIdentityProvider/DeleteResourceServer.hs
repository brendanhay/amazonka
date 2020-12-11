{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource server.
module Network.AWS.CognitoIdentityProvider.DeleteResourceServer
  ( -- * Creating a request
    DeleteResourceServer (..),
    mkDeleteResourceServer,

    -- ** Request lenses
    drsUserPoolId,
    drsIdentifier,

    -- * Destructuring the response
    DeleteResourceServerResponse (..),
    mkDeleteResourceServerResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { userPoolId ::
      Lude.Text,
    identifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceServer' with the minimum fields required to make a request.
--
-- * 'identifier' - The identifier for the resource server.
-- * 'userPoolId' - The user pool ID for the user pool that hosts the resource server.
mkDeleteResourceServer ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'identifier'
  Lude.Text ->
  DeleteResourceServer
mkDeleteResourceServer pUserPoolId_ pIdentifier_ =
  DeleteResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_
    }

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUserPoolId :: Lens.Lens' DeleteResourceServer Lude.Text
drsUserPoolId = Lens.lens (userPoolId :: DeleteResourceServer -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteResourceServer)
{-# DEPRECATED drsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsIdentifier :: Lens.Lens' DeleteResourceServer Lude.Text
drsIdentifier = Lens.lens (identifier :: DeleteResourceServer -> Lude.Text) (\s a -> s {identifier = a} :: DeleteResourceServer)
{-# DEPRECATED drsIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

instance Lude.AWSRequest DeleteResourceServer where
  type Rs DeleteResourceServer = DeleteResourceServerResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteResourceServerResponse'

instance Lude.ToHeaders DeleteResourceServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteResourceServer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourceServer where
  toJSON DeleteResourceServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Identifier" Lude..= identifier)
          ]
      )

instance Lude.ToPath DeleteResourceServer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourceServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse = DeleteResourceServerResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceServerResponse' with the minimum fields required to make a request.
mkDeleteResourceServerResponse ::
  DeleteResourceServerResponse
mkDeleteResourceServerResponse = DeleteResourceServerResponse'
