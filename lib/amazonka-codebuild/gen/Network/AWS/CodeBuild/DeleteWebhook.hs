{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, stops AWS CodeBuild from rebuilding the source code every time a code change is pushed to the repository.
module Network.AWS.CodeBuild.DeleteWebhook
  ( -- * Creating a request
    DeleteWebhook (..),
    mkDeleteWebhook,

    -- ** Request lenses
    dwProjectName,

    -- * Destructuring the response
    DeleteWebhookResponse (..),
    mkDeleteWebhookResponse,

    -- ** Response lenses
    dwrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook' {projectName :: Lude.Text}
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
-- * 'projectName' - The name of the AWS CodeBuild project.
mkDeleteWebhook ::
  -- | 'projectName'
  Lude.Text ->
  DeleteWebhook
mkDeleteWebhook pProjectName_ =
  DeleteWebhook' {projectName = pProjectName_}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwProjectName :: Lens.Lens' DeleteWebhook Lude.Text
dwProjectName = Lens.lens (projectName :: DeleteWebhook -> Lude.Text) (\s a -> s {projectName = a} :: DeleteWebhook)
{-# DEPRECATED dwProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Lude.AWSRequest DeleteWebhook where
  type Rs DeleteWebhook = DeleteWebhookResponse
  request = Req.postJSON codeBuildService
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
              Lude.=# ("CodeBuild_20161006.DeleteWebhook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWebhook where
  toJSON DeleteWebhook' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("projectName" Lude..= projectName)])

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
