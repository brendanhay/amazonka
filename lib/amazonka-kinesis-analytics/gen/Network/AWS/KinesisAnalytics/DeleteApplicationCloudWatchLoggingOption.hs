{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
  ( -- * Creating a request
    DeleteApplicationCloudWatchLoggingOption (..),
    mkDeleteApplicationCloudWatchLoggingOption,

    -- ** Request lenses
    dacwloApplicationName,
    dacwloCurrentApplicationVersionId,
    dacwloCloudWatchLoggingOptionId,

    -- * Destructuring the response
    DeleteApplicationCloudWatchLoggingOptionResponse (..),
    mkDeleteApplicationCloudWatchLoggingOptionResponse,

    -- ** Response lenses
    dacwlorrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { -- | The Kinesis Analytics application name.
    applicationName :: Types.ApplicationName,
    -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Core.Natural,
    -- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
    cloudWatchLoggingOptionId :: Types.CloudWatchLoggingOptionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationCloudWatchLoggingOption' value with any optional fields omitted.
mkDeleteApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'cloudWatchLoggingOptionId'
  Types.CloudWatchLoggingOptionId ->
  DeleteApplicationCloudWatchLoggingOption
mkDeleteApplicationCloudWatchLoggingOption
  applicationName
  currentApplicationVersionId
  cloudWatchLoggingOptionId =
    DeleteApplicationCloudWatchLoggingOption'
      { applicationName,
        currentApplicationVersionId,
        cloudWatchLoggingOptionId
      }

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloApplicationName :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Types.ApplicationName
dacwloApplicationName = Lens.field @"applicationName"
{-# DEPRECATED dacwloApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Core.Natural
dacwloCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED dacwloCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCloudWatchLoggingOptionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Types.CloudWatchLoggingOptionId
dacwloCloudWatchLoggingOptionId = Lens.field @"cloudWatchLoggingOptionId"
{-# DEPRECATED dacwloCloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead." #-}

instance Core.FromJSON DeleteApplicationCloudWatchLoggingOption where
  toJSON DeleteApplicationCloudWatchLoggingOption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just
              ("CloudWatchLoggingOptionId" Core..= cloudWatchLoggingOptionId)
          ]
      )

instance Core.AWSRequest DeleteApplicationCloudWatchLoggingOption where
  type
    Rs DeleteApplicationCloudWatchLoggingOption =
      DeleteApplicationCloudWatchLoggingOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "KinesisAnalytics_20150814.DeleteApplicationCloudWatchLoggingOption"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationCloudWatchLoggingOptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationCloudWatchLoggingOptionResponse' value with any optional fields omitted.
mkDeleteApplicationCloudWatchLoggingOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteApplicationCloudWatchLoggingOptionResponse
mkDeleteApplicationCloudWatchLoggingOptionResponse responseStatus =
  DeleteApplicationCloudWatchLoggingOptionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwlorrsResponseStatus :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse Core.Int
dacwlorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dacwlorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
