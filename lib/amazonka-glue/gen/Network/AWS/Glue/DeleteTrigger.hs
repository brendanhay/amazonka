{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified trigger. If the trigger is not found, no exception is thrown.
module Network.AWS.Glue.DeleteTrigger
  ( -- * Creating a request
    DeleteTrigger (..),
    mkDeleteTrigger,

    -- ** Request lenses
    dttName,

    -- * Destructuring the response
    DeleteTriggerResponse (..),
    mkDeleteTriggerResponse,

    -- ** Response lenses
    delrsName,
    delrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTrigger' smart constructor.
newtype DeleteTrigger = DeleteTrigger' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrigger' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger to delete.
mkDeleteTrigger ::
  -- | 'name'
  Lude.Text ->
  DeleteTrigger
mkDeleteTrigger pName_ = DeleteTrigger' {name = pName_}

-- | The name of the trigger to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttName :: Lens.Lens' DeleteTrigger Lude.Text
dttName = Lens.lens (name :: DeleteTrigger -> Lude.Text) (\s a -> s {name = a} :: DeleteTrigger)
{-# DEPRECATED dttName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteTrigger where
  type Rs DeleteTrigger = DeleteTriggerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTriggerResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrigger where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteTrigger" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTrigger where
  toJSON DeleteTrigger' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteTrigger where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrigger where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTriggerResponse' smart constructor.
data DeleteTriggerResponse = DeleteTriggerResponse'
  { name ::
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

-- | Creates a value of 'DeleteTriggerResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteTriggerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTriggerResponse
mkDeleteTriggerResponse pResponseStatus_ =
  DeleteTriggerResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the trigger that was deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsName :: Lens.Lens' DeleteTriggerResponse (Lude.Maybe Lude.Text)
delrsName = Lens.lens (name :: DeleteTriggerResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteTriggerResponse)
{-# DEPRECATED delrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteTriggerResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteTriggerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTriggerResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
