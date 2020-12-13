{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DeleteJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a job template you have created.
module Network.AWS.MediaConvert.DeleteJobTemplate
  ( -- * Creating a request
    DeleteJobTemplate (..),
    mkDeleteJobTemplate,

    -- ** Request lenses
    djtName,

    -- * Destructuring the response
    DeleteJobTemplateResponse (..),
    mkDeleteJobTemplateResponse,

    -- ** Response lenses
    djtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteJobTemplate' smart constructor.
newtype DeleteJobTemplate = DeleteJobTemplate'
  { -- | The name of the job template to be deleted.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobTemplate' with the minimum fields required to make a request.
--
-- * 'name' - The name of the job template to be deleted.
mkDeleteJobTemplate ::
  -- | 'name'
  Lude.Text ->
  DeleteJobTemplate
mkDeleteJobTemplate pName_ = DeleteJobTemplate' {name = pName_}

-- | The name of the job template to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djtName :: Lens.Lens' DeleteJobTemplate Lude.Text
djtName = Lens.lens (name :: DeleteJobTemplate -> Lude.Text) (\s a -> s {name = a} :: DeleteJobTemplate)
{-# DEPRECATED djtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteJobTemplate where
  type Rs DeleteJobTemplate = DeleteJobTemplateResponse
  request = Req.delete mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteJobTemplateResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteJobTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteJobTemplate where
  toPath DeleteJobTemplate' {..} =
    Lude.mconcat ["/2017-08-29/jobTemplates/", Lude.toBS name]

instance Lude.ToQuery DeleteJobTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteJobTemplateResponse' smart constructor.
newtype DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteJobTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteJobTemplateResponse
mkDeleteJobTemplateResponse pResponseStatus_ =
  DeleteJobTemplateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djtrsResponseStatus :: Lens.Lens' DeleteJobTemplateResponse Lude.Int
djtrsResponseStatus = Lens.lens (responseStatus :: DeleteJobTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteJobTemplateResponse)
{-# DEPRECATED djtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
