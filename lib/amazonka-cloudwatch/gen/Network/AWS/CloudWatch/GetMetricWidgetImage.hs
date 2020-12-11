{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.CloudWatch.GetMetricWidgetImage
  ( -- * Creating a request
    GetMetricWidgetImage (..),
    mkGetMetricWidgetImage,

    -- ** Request lenses
    gmwiOutputFormat,
    gmwiMetricWidget,

    -- * Destructuring the response
    GetMetricWidgetImageResponse (..),
    mkGetMetricWidgetImageResponse,

    -- ** Response lenses
    gmwirsMetricWidgetImage,
    gmwirsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMetricWidgetImage' smart constructor.
data GetMetricWidgetImage = GetMetricWidgetImage'
  { outputFormat ::
      Lude.Maybe Lude.Text,
    metricWidget :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricWidgetImage' with the minimum fields required to make a request.
--
-- * 'metricWidget' - A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax> .
-- If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
-- * 'outputFormat' - The format of the resulting image. Only PNG images are supported.
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
mkGetMetricWidgetImage ::
  -- | 'metricWidget'
  Lude.Text ->
  GetMetricWidgetImage
mkGetMetricWidgetImage pMetricWidget_ =
  GetMetricWidgetImage'
    { outputFormat = Lude.Nothing,
      metricWidget = pMetricWidget_
    }

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
gmwiOutputFormat :: Lens.Lens' GetMetricWidgetImage (Lude.Maybe Lude.Text)
gmwiOutputFormat = Lens.lens (outputFormat :: GetMetricWidgetImage -> Lude.Maybe Lude.Text) (\s a -> s {outputFormat = a} :: GetMetricWidgetImage)
{-# DEPRECATED gmwiOutputFormat "Use generic-lens or generic-optics with 'outputFormat' instead." #-}

-- | A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax> .
-- If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
--
-- /Note:/ Consider using 'metricWidget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwiMetricWidget :: Lens.Lens' GetMetricWidgetImage Lude.Text
gmwiMetricWidget = Lens.lens (metricWidget :: GetMetricWidgetImage -> Lude.Text) (\s a -> s {metricWidget = a} :: GetMetricWidgetImage)
{-# DEPRECATED gmwiMetricWidget "Use generic-lens or generic-optics with 'metricWidget' instead." #-}

instance Lude.AWSRequest GetMetricWidgetImage where
  type Rs GetMetricWidgetImage = GetMetricWidgetImageResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "GetMetricWidgetImageResult"
      ( \s h x ->
          GetMetricWidgetImageResponse'
            Lude.<$> (x Lude..@? "MetricWidgetImage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMetricWidgetImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetMetricWidgetImage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMetricWidgetImage where
  toQuery GetMetricWidgetImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetMetricWidgetImage" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "OutputFormat" Lude.=: outputFormat,
        "MetricWidget" Lude.=: metricWidget
      ]

-- | /See:/ 'mkGetMetricWidgetImageResponse' smart constructor.
data GetMetricWidgetImageResponse = GetMetricWidgetImageResponse'
  { metricWidgetImage ::
      Lude.Maybe Lude.Base64,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricWidgetImageResponse' with the minimum fields required to make a request.
--
-- * 'metricWidgetImage' - The image of the graph, in the output format specified. The output is base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'responseStatus' - The response status code.
mkGetMetricWidgetImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMetricWidgetImageResponse
mkGetMetricWidgetImageResponse pResponseStatus_ =
  GetMetricWidgetImageResponse'
    { metricWidgetImage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The image of the graph, in the output format specified. The output is base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'metricWidgetImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwirsMetricWidgetImage :: Lens.Lens' GetMetricWidgetImageResponse (Lude.Maybe Lude.Base64)
gmwirsMetricWidgetImage = Lens.lens (metricWidgetImage :: GetMetricWidgetImageResponse -> Lude.Maybe Lude.Base64) (\s a -> s {metricWidgetImage = a} :: GetMetricWidgetImageResponse)
{-# DEPRECATED gmwirsMetricWidgetImage "Use generic-lens or generic-optics with 'metricWidgetImage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwirsResponseStatus :: Lens.Lens' GetMetricWidgetImageResponse Lude.Int
gmwirsResponseStatus = Lens.lens (responseStatus :: GetMetricWidgetImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMetricWidgetImageResponse)
{-# DEPRECATED gmwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
