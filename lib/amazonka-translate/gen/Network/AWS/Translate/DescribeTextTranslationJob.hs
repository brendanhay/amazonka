{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.DescribeTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an asycnhronous batch translation job including name, ID, status, source and target languages, input/output S3 buckets, and so on.
module Network.AWS.Translate.DescribeTextTranslationJob
  ( -- * Creating a request
    DescribeTextTranslationJob (..),
    mkDescribeTextTranslationJob,

    -- ** Request lenses
    dttjJobId,

    -- * Destructuring the response
    DescribeTextTranslationJobResponse (..),
    mkDescribeTextTranslationJobResponse,

    -- ** Response lenses
    dttjrrsTextTranslationJobProperties,
    dttjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkDescribeTextTranslationJob' smart constructor.
newtype DescribeTextTranslationJob = DescribeTextTranslationJob'
  { -- | The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTextTranslationJob' value with any optional fields omitted.
mkDescribeTextTranslationJob ::
  -- | 'jobId'
  Types.JobId ->
  DescribeTextTranslationJob
mkDescribeTextTranslationJob jobId =
  DescribeTextTranslationJob' {jobId}

-- | The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjJobId :: Lens.Lens' DescribeTextTranslationJob Types.JobId
dttjJobId = Lens.field @"jobId"
{-# DEPRECATED dttjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribeTextTranslationJob where
  toJSON DescribeTextTranslationJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeTextTranslationJob where
  type
    Rs DescribeTextTranslationJob =
      DescribeTextTranslationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.DescribeTextTranslationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTextTranslationJobResponse'
            Core.<$> (x Core..:? "TextTranslationJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTextTranslationJobResponse' smart constructor.
data DescribeTextTranslationJobResponse = DescribeTextTranslationJobResponse'
  { -- | An object that contains the properties associated with an asynchronous batch translation job.
    textTranslationJobProperties :: Core.Maybe Types.TextTranslationJobProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTextTranslationJobResponse' value with any optional fields omitted.
mkDescribeTextTranslationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTextTranslationJobResponse
mkDescribeTextTranslationJobResponse responseStatus =
  DescribeTextTranslationJobResponse'
    { textTranslationJobProperties =
        Core.Nothing,
      responseStatus
    }

-- | An object that contains the properties associated with an asynchronous batch translation job.
--
-- /Note:/ Consider using 'textTranslationJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrrsTextTranslationJobProperties :: Lens.Lens' DescribeTextTranslationJobResponse (Core.Maybe Types.TextTranslationJobProperties)
dttjrrsTextTranslationJobProperties = Lens.field @"textTranslationJobProperties"
{-# DEPRECATED dttjrrsTextTranslationJobProperties "Use generic-lens or generic-optics with 'textTranslationJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrrsResponseStatus :: Lens.Lens' DescribeTextTranslationJobResponse Core.Int
dttjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dttjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
