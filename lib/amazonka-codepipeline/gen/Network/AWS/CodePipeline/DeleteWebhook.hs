{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeleteWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created webhook by name. Deleting the webhook stops AWS CodePipeline from starting a pipeline every time an external event occurs. The API returns successfully when trying to delete a webhook that is already deleted. If a deleted webhook is re-created by calling PutWebhook with the same name, it will have a different URL.
module Network.AWS.CodePipeline.DeleteWebhook
  ( -- * Creating a request
    DeleteWebhook (..),
    mkDeleteWebhook,

    -- ** Request lenses
    dwName,

    -- * Destructuring the response
    DeleteWebhookResponse (..),
    mkDeleteWebhookResponse,

    -- ** Response lenses
    dwrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWebhook' with the minimum fields required to make a request.
--
-- * 'name' - The name of the webhook you want to delete.
mkDeleteWebhook ::
  -- | 'name'
  Lude.Text ->
  DeleteWebhook
mkDeleteWebhook pName_ = DeleteWebhook' {name = pName_}

-- | The name of the webhook you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwName :: Lens.Lens' DeleteWebhook Lude.Text
dwName = Lens.lens (name :: DeleteWebhook -> Lude.Text) (\s a -> s {name = a} :: DeleteWebhook)
{-# DEPRECATED dwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteWebhook where
  type Rs DeleteWebhook = DeleteWebhookResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteWebhookResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWebhook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.DeleteWebhook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWebhook where
  toJSON DeleteWebhook' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath DeleteWebhook where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWebhook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWebhookResponse' smart constructor.
newtype DeleteWebhookResponse = DeleteWebhookResponse'
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

-- | Creates a value of 'DeleteWebhookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteWebhookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWebhookResponse
mkDeleteWebhookResponse pResponseStatus_ =
  DeleteWebhookResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrsResponseStatus :: Lens.Lens' DeleteWebhookResponse Lude.Int
dwrsResponseStatus = Lens.lens (responseStatus :: DeleteWebhookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWebhookResponse)
{-# DEPRECATED dwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
