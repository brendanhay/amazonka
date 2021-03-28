{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricWidgetImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the @GetMetricWidgetImage@ API to retrieve a snapshot graph of one or more Amazon CloudWatch metrics as a bitmap image. You can then embed this image into your services and products, such as wiki pages, reports, and documents. You could also retrieve images regularly, such as every minute, and create your own custom live dashboard.
--
-- The graph you retrieve can include all CloudWatch metric graph features, including metric math and horizontal and vertical annotations.
-- There is a limit of 20 transactions per second for this API. Each @GetMetricWidgetImage@ action has the following limits:
--
--     * As many as 100 metrics in the graph.
--
--
--     * Up to 100 KB uncompressed payload.
--
--
module Network.AWS.CloudWatch.GetMetricWidgetImage
    (
    -- * Creating a request
      GetMetricWidgetImage (..)
    , mkGetMetricWidgetImage
    -- ** Request lenses
    , gmwiMetricWidget
    , gmwiOutputFormat

    -- * Destructuring the response
    , GetMetricWidgetImageResponse (..)
    , mkGetMetricWidgetImageResponse
    -- ** Response lenses
    , gmwirrsMetricWidgetImage
    , gmwirrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMetricWidgetImage' smart constructor.
data GetMetricWidgetImage = GetMetricWidgetImage'
  { metricWidget :: Types.MetricWidget
    -- ^ A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax> .
-- If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
  , outputFormat :: Core.Maybe Types.OutputFormat
    -- ^ The format of the resulting image. Only PNG images are supported.
--
-- The default is @png@ . If you specify @png@ , the API returns an HTTP response with the content-type set to @text/xml@ . The image data is in a @MetricWidgetImage@ field. For example:
-- @<GetMetricWidgetImageResponse xmlns=<URLstring>>@ 
-- @<GetMetricWidgetImageResult>@ 
-- @<MetricWidgetImage>@ 
-- @iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@ 
-- @</MetricWidgetImage>@ 
-- @</GetMetricWidgetImageResult>@ 
-- @<ResponseMetadata>@ 
-- @<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b</RequestId>@ 
-- @</ResponseMetadata>@ 
-- @</GetMetricWidgetImageResponse>@ 
-- The @image/png@ setting is intended only for custom HTTP requests. For most use cases, and all actions using an AWS SDK, you should use @png@ . If you specify @image/png@ , the HTTP response has a content-type set to @image/png@ , and the body of the response is a PNG image. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMetricWidgetImage' value with any optional fields omitted.
mkGetMetricWidgetImage
    :: Types.MetricWidget -- ^ 'metricWidget'
    -> GetMetricWidgetImage
mkGetMetricWidgetImage metricWidget
  = GetMetricWidgetImage'{metricWidget, outputFormat = Core.Nothing}

-- | A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax> .
-- If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
--
-- /Note:/ Consider using 'metricWidget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwiMetricWidget :: Lens.Lens' GetMetricWidgetImage Types.MetricWidget
gmwiMetricWidget = Lens.field @"metricWidget"
{-# INLINEABLE gmwiMetricWidget #-}
{-# DEPRECATED metricWidget "Use generic-lens or generic-optics with 'metricWidget' instead"  #-}

-- | The format of the resulting image. Only PNG images are supported.
--
-- The default is @png@ . If you specify @png@ , the API returns an HTTP response with the content-type set to @text/xml@ . The image data is in a @MetricWidgetImage@ field. For example:
-- @<GetMetricWidgetImageResponse xmlns=<URLstring>>@ 
-- @<GetMetricWidgetImageResult>@ 
-- @<MetricWidgetImage>@ 
-- @iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@ 
-- @</MetricWidgetImage>@ 
-- @</GetMetricWidgetImageResult>@ 
-- @<ResponseMetadata>@ 
-- @<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b</RequestId>@ 
-- @</ResponseMetadata>@ 
-- @</GetMetricWidgetImageResponse>@ 
-- The @image/png@ setting is intended only for custom HTTP requests. For most use cases, and all actions using an AWS SDK, you should use @png@ . If you specify @image/png@ , the HTTP response has a content-type set to @image/png@ , and the body of the response is a PNG image. 
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwiOutputFormat :: Lens.Lens' GetMetricWidgetImage (Core.Maybe Types.OutputFormat)
gmwiOutputFormat = Lens.field @"outputFormat"
{-# INLINEABLE gmwiOutputFormat #-}
{-# DEPRECATED outputFormat "Use generic-lens or generic-optics with 'outputFormat' instead"  #-}

instance Core.ToQuery GetMetricWidgetImage where
        toQuery GetMetricWidgetImage{..}
          = Core.toQueryPair "Action" ("GetMetricWidgetImage" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "MetricWidget" metricWidget
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OutputFormat")
                outputFormat

instance Core.ToHeaders GetMetricWidgetImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetMetricWidgetImage where
        type Rs GetMetricWidgetImage = GetMetricWidgetImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetMetricWidgetImageResult"
              (\ s h x ->
                 GetMetricWidgetImageResponse' Core.<$>
                   (x Core..@? "MetricWidgetImage") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMetricWidgetImageResponse' smart constructor.
data GetMetricWidgetImageResponse = GetMetricWidgetImageResponse'
  { metricWidgetImage :: Core.Maybe Core.Base64
    -- ^ The image of the graph, in the output format specified. The output is base64-encoded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMetricWidgetImageResponse' value with any optional fields omitted.
mkGetMetricWidgetImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMetricWidgetImageResponse
mkGetMetricWidgetImageResponse responseStatus
  = GetMetricWidgetImageResponse'{metricWidgetImage = Core.Nothing,
                                  responseStatus}

-- | The image of the graph, in the output format specified. The output is base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'metricWidgetImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwirrsMetricWidgetImage :: Lens.Lens' GetMetricWidgetImageResponse (Core.Maybe Core.Base64)
gmwirrsMetricWidgetImage = Lens.field @"metricWidgetImage"
{-# INLINEABLE gmwirrsMetricWidgetImage #-}
{-# DEPRECATED metricWidgetImage "Use generic-lens or generic-optics with 'metricWidgetImage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwirrsResponseStatus :: Lens.Lens' GetMetricWidgetImageResponse Core.Int
gmwirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmwirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
