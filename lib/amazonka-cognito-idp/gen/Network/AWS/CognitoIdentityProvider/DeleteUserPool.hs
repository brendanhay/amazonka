{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Cognito user pool.
module Network.AWS.CognitoIdentityProvider.DeleteUserPool
  ( -- * Creating a request
    DeleteUserPool (..),
    mkDeleteUserPool,

    -- ** Request lenses
    dupUserPoolId,

    -- * Destructuring the response
    DeleteUserPoolResponse (..),
    mkDeleteUserPoolResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete a user pool.
--
-- /See:/ 'mkDeleteUserPool' smart constructor.
newtype DeleteUserPool = DeleteUserPool' {userPoolId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPool' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool you want to delete.
mkDeleteUserPool ::
  -- | 'userPoolId'
  Lude.Text ->
  DeleteUserPool
mkDeleteUserPool pUserPoolId_ =
  DeleteUserPool' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool you want to delete.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserPoolId :: Lens.Lens' DeleteUserPool Lude.Text
dupUserPoolId = Lens.lens (userPoolId :: DeleteUserPool -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteUserPool)
{-# DEPRECATED dupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest DeleteUserPool where
  type Rs DeleteUserPool = DeleteUserPoolResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteUserPoolResponse'

instance Lude.ToHeaders DeleteUserPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteUserPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserPool where
  toJSON DeleteUserPool' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("UserPoolId" Lude..= userPoolId)])

instance Lude.ToPath DeleteUserPool where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserPool where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserPoolResponse' smart constructor.
data DeleteUserPoolResponse = DeleteUserPoolResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPoolResponse' with the minimum fields required to make a request.
mkDeleteUserPoolResponse ::
  DeleteUserPoolResponse
mkDeleteUserPoolResponse = DeleteUserPoolResponse'
