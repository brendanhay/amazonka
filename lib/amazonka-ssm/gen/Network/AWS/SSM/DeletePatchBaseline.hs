{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeletePatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a patch baseline.
module Network.AWS.SSM.DeletePatchBaseline
  ( -- * Creating a request
    DeletePatchBaseline (..),
    mkDeletePatchBaseline,

    -- ** Request lenses
    dpbBaselineId,

    -- * Destructuring the response
    DeletePatchBaselineResponse (..),
    mkDeletePatchBaselineResponse,

    -- ** Response lenses
    dpbrsBaselineId,
    dpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeletePatchBaseline' smart constructor.
newtype DeletePatchBaseline = DeletePatchBaseline'
  { baselineId ::
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

-- | Creates a value of 'DeletePatchBaseline' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline to delete.
mkDeletePatchBaseline ::
  -- | 'baselineId'
  Lude.Text ->
  DeletePatchBaseline
mkDeletePatchBaseline pBaselineId_ =
  DeletePatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline to delete.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbBaselineId :: Lens.Lens' DeletePatchBaseline Lude.Text
dpbBaselineId = Lens.lens (baselineId :: DeletePatchBaseline -> Lude.Text) (\s a -> s {baselineId = a} :: DeletePatchBaseline)
{-# DEPRECATED dpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Lude.AWSRequest DeletePatchBaseline where
  type Rs DeletePatchBaseline = DeletePatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeletePatchBaselineResponse'
            Lude.<$> (x Lude..?> "BaselineId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeletePatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePatchBaseline where
  toJSON DeletePatchBaseline' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BaselineId" Lude..= baselineId)])

instance Lude.ToPath DeletePatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePatchBaselineResponse' smart constructor.
data DeletePatchBaselineResponse = DeletePatchBaselineResponse'
  { baselineId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the deleted patch baseline.
-- * 'responseStatus' - The response status code.
mkDeletePatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePatchBaselineResponse
mkDeletePatchBaselineResponse pResponseStatus_ =
  DeletePatchBaselineResponse'
    { baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deleted patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrsBaselineId :: Lens.Lens' DeletePatchBaselineResponse (Lude.Maybe Lude.Text)
dpbrsBaselineId = Lens.lens (baselineId :: DeletePatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: DeletePatchBaselineResponse)
{-# DEPRECATED dpbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrsResponseStatus :: Lens.Lens' DeletePatchBaselineResponse Lude.Int
dpbrsResponseStatus = Lens.lens (responseStatus :: DeletePatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePatchBaselineResponse)
{-# DEPRECATED dpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
