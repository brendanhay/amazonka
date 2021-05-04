{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricWidgetImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the @GetMetricWidgetImage@ API to retrieve a snapshot graph
-- of one or more Amazon CloudWatch metrics as a bitmap image. You can then
-- embed this image into your services and products, such as wiki pages,
-- reports, and documents. You could also retrieve images regularly, such
-- as every minute, and create your own custom live dashboard.
--
-- The graph you retrieve can include all CloudWatch metric graph features,
-- including metric math and horizontal and vertical annotations.
--
-- There is a limit of 20 transactions per second for this API. Each
-- @GetMetricWidgetImage@ action has the following limits:
--
-- -   As many as 100 metrics in the graph.
--
-- -   Up to 100 KB uncompressed payload.
module Network.AWS.CloudWatch.GetMetricWidgetImage
  ( -- * Creating a Request
    GetMetricWidgetImage (..),
    newGetMetricWidgetImage,

    -- * Request Lenses
    getMetricWidgetImage_outputFormat,
    getMetricWidgetImage_metricWidget,

    -- * Destructuring the Response
    GetMetricWidgetImageResponse (..),
    newGetMetricWidgetImageResponse,

    -- * Response Lenses
    getMetricWidgetImageResponse_metricWidgetImage,
    getMetricWidgetImageResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMetricWidgetImage' smart constructor.
data GetMetricWidgetImage = GetMetricWidgetImage'
  { -- | The format of the resulting image. Only PNG images are supported.
    --
    -- The default is @png@. If you specify @png@, the API returns an HTTP
    -- response with the content-type set to @text\/xml@. The image data is in
    -- a @MetricWidgetImage@ field. For example:
    --
    -- @ \<GetMetricWidgetImageResponse xmlns=\<URLstring>>@
    --
    -- @ \<GetMetricWidgetImageResult>@
    --
    -- @ \<MetricWidgetImage>@
    --
    -- @ iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@
    --
    -- @ \<\/MetricWidgetImage>@
    --
    -- @ \<\/GetMetricWidgetImageResult>@
    --
    -- @ \<ResponseMetadata>@
    --
    -- @ \<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b\<\/RequestId>@
    --
    -- @ \<\/ResponseMetadata>@
    --
    -- @\<\/GetMetricWidgetImageResponse>@
    --
    -- The @image\/png@ setting is intended only for custom HTTP requests. For
    -- most use cases, and all actions using an AWS SDK, you should use @png@.
    -- If you specify @image\/png@, the HTTP response has a content-type set to
    -- @image\/png@, and the body of the response is a PNG image.
    outputFormat :: Prelude.Maybe Prelude.Text,
    -- | A JSON string that defines the bitmap graph to be retrieved. The string
    -- includes the metrics to include in the graph, statistics, annotations,
    -- title, axis limits, and so on. You can include only one @MetricWidget@
    -- parameter in each @GetMetricWidgetImage@ call.
    --
    -- For more information about the syntax of @MetricWidget@ see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax>.
    --
    -- If any metric on the graph could not load all the requested data points,
    -- an orange triangle with an exclamation point appears next to the graph
    -- legend.
    metricWidget :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMetricWidgetImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputFormat', 'getMetricWidgetImage_outputFormat' - The format of the resulting image. Only PNG images are supported.
--
-- The default is @png@. If you specify @png@, the API returns an HTTP
-- response with the content-type set to @text\/xml@. The image data is in
-- a @MetricWidgetImage@ field. For example:
--
-- @ \<GetMetricWidgetImageResponse xmlns=\<URLstring>>@
--
-- @ \<GetMetricWidgetImageResult>@
--
-- @ \<MetricWidgetImage>@
--
-- @ iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@
--
-- @ \<\/MetricWidgetImage>@
--
-- @ \<\/GetMetricWidgetImageResult>@
--
-- @ \<ResponseMetadata>@
--
-- @ \<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b\<\/RequestId>@
--
-- @ \<\/ResponseMetadata>@
--
-- @\<\/GetMetricWidgetImageResponse>@
--
-- The @image\/png@ setting is intended only for custom HTTP requests. For
-- most use cases, and all actions using an AWS SDK, you should use @png@.
-- If you specify @image\/png@, the HTTP response has a content-type set to
-- @image\/png@, and the body of the response is a PNG image.
--
-- 'metricWidget', 'getMetricWidgetImage_metricWidget' - A JSON string that defines the bitmap graph to be retrieved. The string
-- includes the metrics to include in the graph, statistics, annotations,
-- title, axis limits, and so on. You can include only one @MetricWidget@
-- parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax>.
--
-- If any metric on the graph could not load all the requested data points,
-- an orange triangle with an exclamation point appears next to the graph
-- legend.
newGetMetricWidgetImage ::
  -- | 'metricWidget'
  Prelude.Text ->
  GetMetricWidgetImage
newGetMetricWidgetImage pMetricWidget_ =
  GetMetricWidgetImage'
    { outputFormat =
        Prelude.Nothing,
      metricWidget = pMetricWidget_
    }

-- | The format of the resulting image. Only PNG images are supported.
--
-- The default is @png@. If you specify @png@, the API returns an HTTP
-- response with the content-type set to @text\/xml@. The image data is in
-- a @MetricWidgetImage@ field. For example:
--
-- @ \<GetMetricWidgetImageResponse xmlns=\<URLstring>>@
--
-- @ \<GetMetricWidgetImageResult>@
--
-- @ \<MetricWidgetImage>@
--
-- @ iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@
--
-- @ \<\/MetricWidgetImage>@
--
-- @ \<\/GetMetricWidgetImageResult>@
--
-- @ \<ResponseMetadata>@
--
-- @ \<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b\<\/RequestId>@
--
-- @ \<\/ResponseMetadata>@
--
-- @\<\/GetMetricWidgetImageResponse>@
--
-- The @image\/png@ setting is intended only for custom HTTP requests. For
-- most use cases, and all actions using an AWS SDK, you should use @png@.
-- If you specify @image\/png@, the HTTP response has a content-type set to
-- @image\/png@, and the body of the response is a PNG image.
getMetricWidgetImage_outputFormat :: Lens.Lens' GetMetricWidgetImage (Prelude.Maybe Prelude.Text)
getMetricWidgetImage_outputFormat = Lens.lens (\GetMetricWidgetImage' {outputFormat} -> outputFormat) (\s@GetMetricWidgetImage' {} a -> s {outputFormat = a} :: GetMetricWidgetImage)

-- | A JSON string that defines the bitmap graph to be retrieved. The string
-- includes the metrics to include in the graph, statistics, annotations,
-- title, axis limits, and so on. You can include only one @MetricWidget@
-- parameter in each @GetMetricWidgetImage@ call.
--
-- For more information about the syntax of @MetricWidget@ see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Metric-Widget-Structure.html GetMetricWidgetImage: Metric Widget Structure and Syntax>.
--
-- If any metric on the graph could not load all the requested data points,
-- an orange triangle with an exclamation point appears next to the graph
-- legend.
getMetricWidgetImage_metricWidget :: Lens.Lens' GetMetricWidgetImage Prelude.Text
getMetricWidgetImage_metricWidget = Lens.lens (\GetMetricWidgetImage' {metricWidget} -> metricWidget) (\s@GetMetricWidgetImage' {} a -> s {metricWidget = a} :: GetMetricWidgetImage)

instance Prelude.AWSRequest GetMetricWidgetImage where
  type
    Rs GetMetricWidgetImage =
      GetMetricWidgetImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetMetricWidgetImageResult"
      ( \s h x ->
          GetMetricWidgetImageResponse'
            Prelude.<$> (x Prelude..@? "MetricWidgetImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricWidgetImage

instance Prelude.NFData GetMetricWidgetImage

instance Prelude.ToHeaders GetMetricWidgetImage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetMetricWidgetImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetMetricWidgetImage where
  toQuery GetMetricWidgetImage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetMetricWidgetImage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "OutputFormat" Prelude.=: outputFormat,
        "MetricWidget" Prelude.=: metricWidget
      ]

-- | /See:/ 'newGetMetricWidgetImageResponse' smart constructor.
data GetMetricWidgetImageResponse = GetMetricWidgetImageResponse'
  { -- | The image of the graph, in the output format specified. The output is
    -- base64-encoded.
    metricWidgetImage :: Prelude.Maybe Prelude.Base64,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMetricWidgetImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricWidgetImage', 'getMetricWidgetImageResponse_metricWidgetImage' - The image of the graph, in the output format specified. The output is
-- base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'getMetricWidgetImageResponse_httpStatus' - The response's http status code.
newGetMetricWidgetImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetricWidgetImageResponse
newGetMetricWidgetImageResponse pHttpStatus_ =
  GetMetricWidgetImageResponse'
    { metricWidgetImage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image of the graph, in the output format specified. The output is
-- base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getMetricWidgetImageResponse_metricWidgetImage :: Lens.Lens' GetMetricWidgetImageResponse (Prelude.Maybe Prelude.ByteString)
getMetricWidgetImageResponse_metricWidgetImage = Lens.lens (\GetMetricWidgetImageResponse' {metricWidgetImage} -> metricWidgetImage) (\s@GetMetricWidgetImageResponse' {} a -> s {metricWidgetImage = a} :: GetMetricWidgetImageResponse) Prelude.. Lens.mapping Prelude._Base64

-- | The response's http status code.
getMetricWidgetImageResponse_httpStatus :: Lens.Lens' GetMetricWidgetImageResponse Prelude.Int
getMetricWidgetImageResponse_httpStatus = Lens.lens (\GetMetricWidgetImageResponse' {httpStatus} -> httpStatus) (\s@GetMetricWidgetImageResponse' {} a -> s {httpStatus = a} :: GetMetricWidgetImageResponse)

instance Prelude.NFData GetMetricWidgetImageResponse
