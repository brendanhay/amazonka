{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a defined mitigation action from your AWS account.
module Network.AWS.IoT.DeleteMitigationAction
  ( -- * Creating a request
    DeleteMitigationAction (..),
    mkDeleteMitigationAction,

    -- ** Request lenses
    dmaActionName,

    -- * Destructuring the response
    DeleteMitigationActionResponse (..),
    mkDeleteMitigationActionResponse,

    -- ** Response lenses
    dmarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMitigationAction' smart constructor.
newtype DeleteMitigationAction = DeleteMitigationAction'
  { actionName ::
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

-- | Creates a value of 'DeleteMitigationAction' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the mitigation action that you want to delete.
mkDeleteMitigationAction ::
  -- | 'actionName'
  Lude.Text ->
  DeleteMitigationAction
mkDeleteMitigationAction pActionName_ =
  DeleteMitigationAction' {actionName = pActionName_}

-- | The name of the mitigation action that you want to delete.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaActionName :: Lens.Lens' DeleteMitigationAction Lude.Text
dmaActionName = Lens.lens (actionName :: DeleteMitigationAction -> Lude.Text) (\s a -> s {actionName = a} :: DeleteMitigationAction)
{-# DEPRECATED dmaActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Lude.AWSRequest DeleteMitigationAction where
  type Rs DeleteMitigationAction = DeleteMitigationActionResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMitigationActionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMitigationAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteMitigationAction where
  toPath DeleteMitigationAction' {..} =
    Lude.mconcat
      ["/mitigationactions/actions/", Lude.toBS actionName]

instance Lude.ToQuery DeleteMitigationAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMitigationActionResponse' smart constructor.
newtype DeleteMitigationActionResponse = DeleteMitigationActionResponse'
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

-- | Creates a value of 'DeleteMitigationActionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMitigationActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMitigationActionResponse
mkDeleteMitigationActionResponse pResponseStatus_ =
  DeleteMitigationActionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarsResponseStatus :: Lens.Lens' DeleteMitigationActionResponse Lude.Int
dmarsResponseStatus = Lens.lens (responseStatus :: DeleteMitigationActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMitigationActionResponse)
{-# DEPRECATED dmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
