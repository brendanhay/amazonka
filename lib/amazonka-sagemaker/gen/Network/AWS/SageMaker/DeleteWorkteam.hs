{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing work team. This operation can't be undone.
module Network.AWS.SageMaker.DeleteWorkteam
  ( -- * Creating a request
    DeleteWorkteam (..),
    mkDeleteWorkteam,

    -- ** Request lenses
    dwWorkteamName,

    -- * Destructuring the response
    DeleteWorkteamResponse (..),
    mkDeleteWorkteamResponse,

    -- ** Response lenses
    dwfrsSuccess,
    dwfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteWorkteam' smart constructor.
newtype DeleteWorkteam = DeleteWorkteam'
  { -- | The name of the work team to delete.
    workteamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkteam' with the minimum fields required to make a request.
--
-- * 'workteamName' - The name of the work team to delete.
mkDeleteWorkteam ::
  -- | 'workteamName'
  Lude.Text ->
  DeleteWorkteam
mkDeleteWorkteam pWorkteamName_ =
  DeleteWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to delete.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkteamName :: Lens.Lens' DeleteWorkteam Lude.Text
dwWorkteamName = Lens.lens (workteamName :: DeleteWorkteam -> Lude.Text) (\s a -> s {workteamName = a} :: DeleteWorkteam)
{-# DEPRECATED dwWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

instance Lude.AWSRequest DeleteWorkteam where
  type Rs DeleteWorkteam = DeleteWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteWorkteamResponse'
            Lude.<$> (x Lude..:> "Success") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkteam where
  toJSON DeleteWorkteam' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkteamName" Lude..= workteamName)])

instance Lude.ToPath DeleteWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkteamResponse' smart constructor.
data DeleteWorkteamResponse = DeleteWorkteamResponse'
  { -- | Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
    success :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'success' - Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
-- * 'responseStatus' - The response status code.
mkDeleteWorkteamResponse ::
  -- | 'success'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkteamResponse
mkDeleteWorkteamResponse pSuccess_ pResponseStatus_ =
  DeleteWorkteamResponse'
    { success = pSuccess_,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the work team was successfully deleted; otherwise, returns @false@ .
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwfrsSuccess :: Lens.Lens' DeleteWorkteamResponse Lude.Bool
dwfrsSuccess = Lens.lens (success :: DeleteWorkteamResponse -> Lude.Bool) (\s a -> s {success = a} :: DeleteWorkteamResponse)
{-# DEPRECATED dwfrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwfrsResponseStatus :: Lens.Lens' DeleteWorkteamResponse Lude.Int
dwfrsResponseStatus = Lens.lens (responseStatus :: DeleteWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkteamResponse)
{-# DEPRECATED dwfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
