{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteApplicationCloudWatchLoggingOption (..)
    , mkDeleteApplicationCloudWatchLoggingOption
    -- ** Request lenses
    , dacwloApplicationName
    , dacwloCurrentApplicationVersionId
    , dacwloCloudWatchLoggingOptionId

    -- * Destructuring the response
    , DeleteApplicationCloudWatchLoggingOptionResponse (..)
    , mkDeleteApplicationCloudWatchLoggingOptionResponse
    -- ** Response lenses
    , dacwlorrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { applicationName :: Types.ApplicationName
    -- ^ The Kinesis Analytics application name.
  , currentApplicationVersionId :: Core.Natural
    -- ^ The version ID of the Kinesis Analytics application.
  , cloudWatchLoggingOptionId :: Types.CloudWatchLoggingOptionId
    -- ^ The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationCloudWatchLoggingOption' value with any optional fields omitted.
mkDeleteApplicationCloudWatchLoggingOption
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.CloudWatchLoggingOptionId -- ^ 'cloudWatchLoggingOptionId'
    -> DeleteApplicationCloudWatchLoggingOption
mkDeleteApplicationCloudWatchLoggingOption applicationName
  currentApplicationVersionId cloudWatchLoggingOptionId
  = DeleteApplicationCloudWatchLoggingOption'{applicationName,
                                              currentApplicationVersionId,
                                              cloudWatchLoggingOptionId}

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloApplicationName :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Types.ApplicationName
dacwloApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dacwloApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Core.Natural
dacwloCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE dacwloCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation. 
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCloudWatchLoggingOptionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Types.CloudWatchLoggingOptionId
dacwloCloudWatchLoggingOptionId = Lens.field @"cloudWatchLoggingOptionId"
{-# INLINEABLE dacwloCloudWatchLoggingOptionId #-}
{-# DEPRECATED cloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead"  #-}

instance Core.ToQuery DeleteApplicationCloudWatchLoggingOption
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplicationCloudWatchLoggingOption
         where
        toHeaders DeleteApplicationCloudWatchLoggingOption{..}
          = Core.pure
              ("X-Amz-Target",
               "KinesisAnalytics_20150814.DeleteApplicationCloudWatchLoggingOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApplicationCloudWatchLoggingOption
         where
        toJSON DeleteApplicationCloudWatchLoggingOption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just
                    ("CloudWatchLoggingOptionId" Core..= cloudWatchLoggingOptionId)])

instance Core.AWSRequest DeleteApplicationCloudWatchLoggingOption
         where
        type Rs DeleteApplicationCloudWatchLoggingOption =
             DeleteApplicationCloudWatchLoggingOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApplicationCloudWatchLoggingOptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationCloudWatchLoggingOptionResponse' value with any optional fields omitted.
mkDeleteApplicationCloudWatchLoggingOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApplicationCloudWatchLoggingOptionResponse
mkDeleteApplicationCloudWatchLoggingOptionResponse responseStatus
  = DeleteApplicationCloudWatchLoggingOptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwlorrsResponseStatus :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse Core.Int
dacwlorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dacwlorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
