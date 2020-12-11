{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity pool. Once a pool is deleted, users will not be able to authenticate with the pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentityPool
  ( -- * Creating a request
    DeleteIdentityPool (..),
    mkDeleteIdentityPool,

    -- ** Request lenses
    dIdentityPoolId,

    -- * Destructuring the response
    DeleteIdentityPoolResponse (..),
    mkDeleteIdentityPoolResponse,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'mkDeleteIdentityPool' smart constructor.
newtype DeleteIdentityPool = DeleteIdentityPool'
  { identityPoolId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityPool' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
mkDeleteIdentityPool ::
  -- | 'identityPoolId'
  Lude.Text ->
  DeleteIdentityPool
mkDeleteIdentityPool pIdentityPoolId_ =
  DeleteIdentityPool' {identityPoolId = pIdentityPoolId_}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentityPoolId :: Lens.Lens' DeleteIdentityPool Lude.Text
dIdentityPoolId = Lens.lens (identityPoolId :: DeleteIdentityPool -> Lude.Text) (\s a -> s {identityPoolId = a} :: DeleteIdentityPool)
{-# DEPRECATED dIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest DeleteIdentityPool where
  type Rs DeleteIdentityPool = DeleteIdentityPoolResponse
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveNull DeleteIdentityPoolResponse'

instance Lude.ToHeaders DeleteIdentityPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.DeleteIdentityPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteIdentityPool where
  toJSON DeleteIdentityPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("IdentityPoolId" Lude..= identityPoolId)]
      )

instance Lude.ToPath DeleteIdentityPool where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIdentityPool where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityPoolResponse' with the minimum fields required to make a request.
mkDeleteIdentityPoolResponse ::
  DeleteIdentityPoolResponse
mkDeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
