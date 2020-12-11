{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
  ( -- * Creating a request
    DeleteIdentityProvider (..),
    mkDeleteIdentityProvider,

    -- ** Request lenses
    delUserPoolId,
    delProviderName,

    -- * Destructuring the response
    DeleteIdentityProviderResponse (..),
    mkDeleteIdentityProviderResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { userPoolId ::
      Lude.Text,
    providerName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityProvider' with the minimum fields required to make a request.
--
-- * 'providerName' - The identity provider name.
-- * 'userPoolId' - The user pool ID.
mkDeleteIdentityProvider ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  DeleteIdentityProvider
mkDeleteIdentityProvider pUserPoolId_ pProviderName_ =
  DeleteIdentityProvider'
    { userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delUserPoolId :: Lens.Lens' DeleteIdentityProvider Lude.Text
delUserPoolId = Lens.lens (userPoolId :: DeleteIdentityProvider -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteIdentityProvider)
{-# DEPRECATED delUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delProviderName :: Lens.Lens' DeleteIdentityProvider Lude.Text
delProviderName = Lens.lens (providerName :: DeleteIdentityProvider -> Lude.Text) (\s a -> s {providerName = a} :: DeleteIdentityProvider)
{-# DEPRECATED delProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.AWSRequest DeleteIdentityProvider where
  type Rs DeleteIdentityProvider = DeleteIdentityProviderResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteIdentityProviderResponse'

instance Lude.ToHeaders DeleteIdentityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteIdentityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteIdentityProvider where
  toJSON DeleteIdentityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ProviderName" Lude..= providerName)
          ]
      )

instance Lude.ToPath DeleteIdentityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIdentityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityProviderResponse' with the minimum fields required to make a request.
mkDeleteIdentityProviderResponse ::
  DeleteIdentityProviderResponse
mkDeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
