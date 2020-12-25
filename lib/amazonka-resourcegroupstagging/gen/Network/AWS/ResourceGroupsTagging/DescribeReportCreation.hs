{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.DescribeReportCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the @StartReportCreation@ operation.
--
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.DescribeReportCreation
  ( -- * Creating a request
    DescribeReportCreation (..),
    mkDescribeReportCreation,

    -- * Destructuring the response
    DescribeReportCreationResponse (..),
    mkDescribeReportCreationResponse,

    -- ** Response lenses
    drcrrsErrorMessage,
    drcrrsS3Location,
    drcrrsStatus,
    drcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeReportCreation' smart constructor.
data DescribeReportCreation = DescribeReportCreation'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReportCreation' value with any optional fields omitted.
mkDescribeReportCreation ::
  DescribeReportCreation
mkDescribeReportCreation = DescribeReportCreation'

instance Core.FromJSON DescribeReportCreation where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeReportCreation where
  type Rs DescribeReportCreation = DescribeReportCreationResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "ResourceGroupsTaggingAPI_20170126.DescribeReportCreation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportCreationResponse'
            Core.<$> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "S3Location")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeReportCreationResponse' smart constructor.
data DescribeReportCreationResponse = DescribeReportCreationResponse'
  { -- | Details of the common errors that all operations return.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The path to the Amazon S3 bucket where the report was stored on creation.
    s3Location :: Core.Maybe Types.S3Location,
    -- | Reports the status of the operation.
    --
    -- The operation status can be one of the following:
    --
    --     * @RUNNING@ - Report creation is in progress.
    --
    --
    --     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .
    --
    --
    --     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.
    --
    --
    --     * @NO REPORT@ - No report was generated in the last 90 days.
    status :: Core.Maybe Types.Status,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReportCreationResponse' value with any optional fields omitted.
mkDescribeReportCreationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReportCreationResponse
mkDescribeReportCreationResponse responseStatus =
  DescribeReportCreationResponse'
    { errorMessage = Core.Nothing,
      s3Location = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Details of the common errors that all operations return.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsErrorMessage :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Types.ErrorMessage)
drcrrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED drcrrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The path to the Amazon S3 bucket where the report was stored on creation.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsS3Location :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Types.S3Location)
drcrrsS3Location = Lens.field @"s3Location"
{-# DEPRECATED drcrrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | Reports the status of the operation.
--
-- The operation status can be one of the following:
--
--     * @RUNNING@ - Report creation is in progress.
--
--
--     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .
--
--
--     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.
--
--
--     * @NO REPORT@ - No report was generated in the last 90 days.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsStatus :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Types.Status)
drcrrsStatus = Lens.field @"status"
{-# DEPRECATED drcrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DescribeReportCreationResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
