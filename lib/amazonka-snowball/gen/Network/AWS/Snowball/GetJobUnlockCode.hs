{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular @UnlockCode@ value can be accessed for up to 90 days after the associated job has been created.
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric characters and 4 hyphens. This code is used to decrypt the manifest file when it is passed along with the manifest to the Snow device through the Snowball client when the client is started for the first time.
-- As a best practice, we recommend that you don't save a copy of the @UnlockCode@ in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snow device associated with that job.
module Network.AWS.Snowball.GetJobUnlockCode
  ( -- * Creating a request
    GetJobUnlockCode (..),
    mkGetJobUnlockCode,

    -- ** Request lenses
    gjucJobId,

    -- * Destructuring the response
    GetJobUnlockCodeResponse (..),
    mkGetJobUnlockCodeResponse,

    -- ** Response lenses
    gjucrrsUnlockCode,
    gjucrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkGetJobUnlockCode' smart constructor.
newtype GetJobUnlockCode = GetJobUnlockCode'
  { -- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobUnlockCode' value with any optional fields omitted.
mkGetJobUnlockCode ::
  -- | 'jobId'
  Types.JobId ->
  GetJobUnlockCode
mkGetJobUnlockCode jobId = GetJobUnlockCode' {jobId}

-- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucJobId :: Lens.Lens' GetJobUnlockCode Types.JobId
gjucJobId = Lens.field @"jobId"
{-# DEPRECATED gjucJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON GetJobUnlockCode where
  toJSON GetJobUnlockCode {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest GetJobUnlockCode where
  type Rs GetJobUnlockCode = GetJobUnlockCodeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.GetJobUnlockCode"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobUnlockCodeResponse'
            Core.<$> (x Core..:? "UnlockCode") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { -- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
    unlockCode :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobUnlockCodeResponse' value with any optional fields omitted.
mkGetJobUnlockCodeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobUnlockCodeResponse
mkGetJobUnlockCodeResponse responseStatus =
  GetJobUnlockCodeResponse'
    { unlockCode = Core.Nothing,
      responseStatus
    }

-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
--
-- /Note:/ Consider using 'unlockCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrrsUnlockCode :: Lens.Lens' GetJobUnlockCodeResponse (Core.Maybe Types.String)
gjucrrsUnlockCode = Lens.field @"unlockCode"
{-# DEPRECATED gjucrrsUnlockCode "Use generic-lens or generic-optics with 'unlockCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrrsResponseStatus :: Lens.Lens' GetJobUnlockCodeResponse Core.Int
gjucrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjucrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
