{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an authorizer.
module Network.AWS.IoT.DeleteAuthorizer
  ( -- * Creating a request
    DeleteAuthorizer (..),
    mkDeleteAuthorizer,

    -- ** Request lenses
    dAuthorizerName,

    -- * Destructuring the response
    DeleteAuthorizerResponse (..),
    mkDeleteAuthorizerResponse,

    -- ** Response lenses
    dafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAuthorizer' smart constructor.
newtype DeleteAuthorizer = DeleteAuthorizer'
  { -- | The name of the authorizer to delete.
    authorizerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerName' - The name of the authorizer to delete.
mkDeleteAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  DeleteAuthorizer
mkDeleteAuthorizer pAuthorizerName_ =
  DeleteAuthorizer' {authorizerName = pAuthorizerName_}

-- | The name of the authorizer to delete.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAuthorizerName :: Lens.Lens' DeleteAuthorizer Lude.Text
dAuthorizerName = Lens.lens (authorizerName :: DeleteAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: DeleteAuthorizer)
{-# DEPRECATED dAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Lude.AWSRequest DeleteAuthorizer where
  type Rs DeleteAuthorizer = DeleteAuthorizerResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAuthorizerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Lude.mconcat ["/authorizer/", Lude.toBS authorizerName]

instance Lude.ToQuery DeleteAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAuthorizerResponse' smart constructor.
newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAuthorizerResponse
mkDeleteAuthorizerResponse pResponseStatus_ =
  DeleteAuthorizerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrsResponseStatus :: Lens.Lens' DeleteAuthorizerResponse Lude.Int
dafrsResponseStatus = Lens.lens (responseStatus :: DeleteAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAuthorizerResponse)
{-# DEPRECATED dafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
