{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an external destination to your Amazon Kinesis Analytics application.
--
-- If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.
-- You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Understanding Application Output (Destination)> . 
-- Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
-- For the limits on the number of application inputs and outputs you can configure, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationOutput@ action.
module Network.AWS.KinesisAnalytics.AddApplicationOutput
    (
    -- * Creating a request
      AddApplicationOutput (..)
    , mkAddApplicationOutput
    -- ** Request lenses
    , aaoApplicationName
    , aaoCurrentApplicationVersionId
    , aaoOutput

    -- * Destructuring the response
    , AddApplicationOutputResponse (..)
    , mkAddApplicationOutputResponse
    -- ** Response lenses
    , aaorrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkAddApplicationOutput' smart constructor.
data AddApplicationOutput = AddApplicationOutput'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the application to which you want to add the output configuration.
  , currentApplicationVersionId :: Core.Natural
    -- ^ Version of the application to which you want to add the output configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned. 
  , output :: Types.Output
    -- ^ An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), and record the formation to use when writing to the destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationOutput' value with any optional fields omitted.
mkAddApplicationOutput
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.Output -- ^ 'output'
    -> AddApplicationOutput
mkAddApplicationOutput applicationName currentApplicationVersionId
  output
  = AddApplicationOutput'{applicationName,
                          currentApplicationVersionId, output}

-- | Name of the application to which you want to add the output configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoApplicationName :: Lens.Lens' AddApplicationOutput Types.ApplicationName
aaoApplicationName = Lens.field @"applicationName"
{-# INLINEABLE aaoApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Version of the application to which you want to add the output configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned. 
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoCurrentApplicationVersionId :: Lens.Lens' AddApplicationOutput Core.Natural
aaoCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE aaoCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), and record the formation to use when writing to the destination.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoOutput :: Lens.Lens' AddApplicationOutput Types.Output
aaoOutput = Lens.field @"output"
{-# INLINEABLE aaoOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

instance Core.ToQuery AddApplicationOutput where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddApplicationOutput where
        toHeaders AddApplicationOutput{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.AddApplicationOutput")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddApplicationOutput where
        toJSON AddApplicationOutput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just ("Output" Core..= output)])

instance Core.AWSRequest AddApplicationOutput where
        type Rs AddApplicationOutput = AddApplicationOutputResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddApplicationOutputResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkAddApplicationOutputResponse' smart constructor.
newtype AddApplicationOutputResponse = AddApplicationOutputResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationOutputResponse' value with any optional fields omitted.
mkAddApplicationOutputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddApplicationOutputResponse
mkAddApplicationOutputResponse responseStatus
  = AddApplicationOutputResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaorrsResponseStatus :: Lens.Lens' AddApplicationOutputResponse Core.Int
aaorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aaorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
