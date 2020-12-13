{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.GetJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific job template.
module Network.AWS.MediaConvert.GetJobTemplate
  ( -- * Creating a request
    GetJobTemplate (..),
    mkGetJobTemplate,

    -- ** Request lenses
    gjtName,

    -- * Destructuring the response
    GetJobTemplateResponse (..),
    mkGetJobTemplateResponse,

    -- ** Response lenses
    gjtrsJobTemplate,
    gjtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobTemplate' smart constructor.
newtype GetJobTemplate = GetJobTemplate'
  { -- | The name of the job template.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobTemplate' with the minimum fields required to make a request.
--
-- * 'name' - The name of the job template.
mkGetJobTemplate ::
  -- | 'name'
  Lude.Text ->
  GetJobTemplate
mkGetJobTemplate pName_ = GetJobTemplate' {name = pName_}

-- | The name of the job template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtName :: Lens.Lens' GetJobTemplate Lude.Text
gjtName = Lens.lens (name :: GetJobTemplate -> Lude.Text) (\s a -> s {name = a} :: GetJobTemplate)
{-# DEPRECATED gjtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetJobTemplate where
  type Rs GetJobTemplate = GetJobTemplateResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobTemplateResponse'
            Lude.<$> (x Lude..?> "jobTemplate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJobTemplate where
  toPath GetJobTemplate' {..} =
    Lude.mconcat ["/2017-08-29/jobTemplates/", Lude.toBS name]

instance Lude.ToQuery GetJobTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobTemplateResponse' smart constructor.
data GetJobTemplateResponse = GetJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
    jobTemplate :: Lude.Maybe JobTemplate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobTemplateResponse' with the minimum fields required to make a request.
--
-- * 'jobTemplate' - A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
-- * 'responseStatus' - The response status code.
mkGetJobTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobTemplateResponse
mkGetJobTemplateResponse pResponseStatus_ =
  GetJobTemplateResponse'
    { jobTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtrsJobTemplate :: Lens.Lens' GetJobTemplateResponse (Lude.Maybe JobTemplate)
gjtrsJobTemplate = Lens.lens (jobTemplate :: GetJobTemplateResponse -> Lude.Maybe JobTemplate) (\s a -> s {jobTemplate = a} :: GetJobTemplateResponse)
{-# DEPRECATED gjtrsJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtrsResponseStatus :: Lens.Lens' GetJobTemplateResponse Lude.Int
gjtrsResponseStatus = Lens.lens (responseStatus :: GetJobTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobTemplateResponse)
{-# DEPRECATED gjtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
