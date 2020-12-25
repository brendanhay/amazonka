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
    gjtrrsJobTemplate,
    gjtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobTemplate' smart constructor.
newtype GetJobTemplate = GetJobTemplate'
  { -- | The name of the job template.
    name :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobTemplate' value with any optional fields omitted.
mkGetJobTemplate ::
  -- | 'name'
  Core.Text ->
  GetJobTemplate
mkGetJobTemplate name = GetJobTemplate' {name}

-- | The name of the job template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtName :: Lens.Lens' GetJobTemplate Core.Text
gjtName = Lens.field @"name"
{-# DEPRECATED gjtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest GetJobTemplate where
  type Rs GetJobTemplate = GetJobTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2017-08-29/jobTemplates/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobTemplateResponse'
            Core.<$> (x Core..:? "jobTemplate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetJobTemplateResponse' smart constructor.
data GetJobTemplateResponse = GetJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
    jobTemplate :: Core.Maybe Types.JobTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetJobTemplateResponse' value with any optional fields omitted.
mkGetJobTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobTemplateResponse
mkGetJobTemplateResponse responseStatus =
  GetJobTemplateResponse'
    { jobTemplate = Core.Nothing,
      responseStatus
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtrrsJobTemplate :: Lens.Lens' GetJobTemplateResponse (Core.Maybe Types.JobTemplate)
gjtrrsJobTemplate = Lens.field @"jobTemplate"
{-# DEPRECATED gjtrrsJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjtrrsResponseStatus :: Lens.Lens' GetJobTemplateResponse Core.Int
gjtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
