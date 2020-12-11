{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DeleteEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.
module Network.AWS.Cloud9.DeleteEnvironment
  ( -- * Creating a request
    DeleteEnvironment (..),
    mkDeleteEnvironment,

    -- ** Request lenses
    deEnvironmentId,

    -- * Destructuring the response
    DeleteEnvironmentResponse (..),
    mkDeleteEnvironmentResponse,

    -- ** Response lenses
    dersResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEnvironment' smart constructor.
newtype DeleteEnvironment = DeleteEnvironment'
  { environmentId ::
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

-- | Creates a value of 'DeleteEnvironment' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment to delete.
mkDeleteEnvironment ::
  -- | 'environmentId'
  Lude.Text ->
  DeleteEnvironment
mkDeleteEnvironment pEnvironmentId_ =
  DeleteEnvironment' {environmentId = pEnvironmentId_}

-- | The ID of the environment to delete.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentId :: Lens.Lens' DeleteEnvironment Lude.Text
deEnvironmentId = Lens.lens (environmentId :: DeleteEnvironment -> Lude.Text) (\s a -> s {environmentId = a} :: DeleteEnvironment)
{-# DEPRECATED deEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DeleteEnvironment where
  type Rs DeleteEnvironment = DeleteEnvironmentResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEnvironment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.DeleteEnvironment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEnvironment where
  toJSON DeleteEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("environmentId" Lude..= environmentId)]
      )

instance Lude.ToPath DeleteEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEnvironment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEnvironmentResponse' smart constructor.
newtype DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteEnvironmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEnvironmentResponse
mkDeleteEnvironmentResponse pResponseStatus_ =
  DeleteEnvironmentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DeleteEnvironmentResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DeleteEnvironmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEnvironmentResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
