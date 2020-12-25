{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
module Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
  ( -- * Creating a request
    AddApplicationCloudWatchLoggingOption (..),
    mkAddApplicationCloudWatchLoggingOption,

    -- ** Request lenses
    aacwloApplicationName,
    aacwloCurrentApplicationVersionId,
    aacwloCloudWatchLoggingOption,

    -- * Destructuring the response
    AddApplicationCloudWatchLoggingOptionResponse (..),
    mkAddApplicationCloudWatchLoggingOptionResponse,

    -- ** Response lenses
    aacwlorrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddApplicationCloudWatchLoggingOption' smart constructor.
data AddApplicationCloudWatchLoggingOption = AddApplicationCloudWatchLoggingOption'
  { -- | The Kinesis Analytics application name.
    applicationName :: Types.ApplicationName,
    -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Core.Natural,
    -- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
    cloudWatchLoggingOption :: Types.CloudWatchLoggingOption
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationCloudWatchLoggingOption' value with any optional fields omitted.
mkAddApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'cloudWatchLoggingOption'
  Types.CloudWatchLoggingOption ->
  AddApplicationCloudWatchLoggingOption
mkAddApplicationCloudWatchLoggingOption
  applicationName
  currentApplicationVersionId
  cloudWatchLoggingOption =
    AddApplicationCloudWatchLoggingOption'
      { applicationName,
        currentApplicationVersionId,
        cloudWatchLoggingOption
      }

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloApplicationName :: Lens.Lens' AddApplicationCloudWatchLoggingOption Types.ApplicationName
aacwloApplicationName = Lens.field @"applicationName"
{-# DEPRECATED aacwloApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloCurrentApplicationVersionId :: Lens.Lens' AddApplicationCloudWatchLoggingOption Core.Natural
aacwloCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED aacwloCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'cloudWatchLoggingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloCloudWatchLoggingOption :: Lens.Lens' AddApplicationCloudWatchLoggingOption Types.CloudWatchLoggingOption
aacwloCloudWatchLoggingOption = Lens.field @"cloudWatchLoggingOption"
{-# DEPRECATED aacwloCloudWatchLoggingOption "Use generic-lens or generic-optics with 'cloudWatchLoggingOption' instead." #-}

instance Core.FromJSON AddApplicationCloudWatchLoggingOption where
  toJSON AddApplicationCloudWatchLoggingOption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just
              ("CloudWatchLoggingOption" Core..= cloudWatchLoggingOption)
          ]
      )

instance Core.AWSRequest AddApplicationCloudWatchLoggingOption where
  type
    Rs AddApplicationCloudWatchLoggingOption =
      AddApplicationCloudWatchLoggingOptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "KinesisAnalytics_20150814.AddApplicationCloudWatchLoggingOption"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationCloudWatchLoggingOptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationCloudWatchLoggingOptionResponse' value with any optional fields omitted.
mkAddApplicationCloudWatchLoggingOptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddApplicationCloudWatchLoggingOptionResponse
mkAddApplicationCloudWatchLoggingOptionResponse responseStatus =
  AddApplicationCloudWatchLoggingOptionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwlorrsResponseStatus :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse Core.Int
aacwlorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aacwlorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
