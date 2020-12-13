{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DeleteActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activity.
module Network.AWS.StepFunctions.DeleteActivity
  ( -- * Creating a request
    DeleteActivity (..),
    mkDeleteActivity,

    -- ** Request lenses
    dActivityARN,

    -- * Destructuring the response
    DeleteActivityResponse (..),
    mkDeleteActivityResponse,

    -- ** Response lenses
    dafrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDeleteActivity' smart constructor.
newtype DeleteActivity = DeleteActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to delete.
    activityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteActivity' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) of the activity to delete.
mkDeleteActivity ::
  -- | 'activityARN'
  Lude.Text ->
  DeleteActivity
mkDeleteActivity pActivityARN_ =
  DeleteActivity' {activityARN = pActivityARN_}

-- | The Amazon Resource Name (ARN) of the activity to delete.
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActivityARN :: Lens.Lens' DeleteActivity Lude.Text
dActivityARN = Lens.lens (activityARN :: DeleteActivity -> Lude.Text) (\s a -> s {activityARN = a} :: DeleteActivity)
{-# DEPRECATED dActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

instance Lude.AWSRequest DeleteActivity where
  type Rs DeleteActivity = DeleteActivityResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteActivityResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteActivity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.DeleteActivity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteActivity where
  toJSON DeleteActivity' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("activityArn" Lude..= activityARN)])

instance Lude.ToPath DeleteActivity where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteActivity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteActivityResponse' smart constructor.
newtype DeleteActivityResponse = DeleteActivityResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteActivityResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteActivityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteActivityResponse
mkDeleteActivityResponse pResponseStatus_ =
  DeleteActivityResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrsResponseStatus :: Lens.Lens' DeleteActivityResponse Lude.Int
dafrsResponseStatus = Lens.lens (responseStatus :: DeleteActivityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteActivityResponse)
{-# DEPRECATED dafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
