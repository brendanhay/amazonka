{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricWidgetImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the @GetMetricWidgetImage@ API to retrieve a snapshot graph of one or more Amazon CloudWatch metrics as a bitmap image. You can then embed this image into your services and products, such as wiki pages, reports, and documents. You could also retrieve images regularly, such as every minute, and create your own custom live dashboard.
--
--
-- The graph you retrieve can include all CloudWatch metric graph features, including metric math and horizontal and vertical annotations.
--
-- There is a limit of 20 transactions per second for this API. Each @GetMetricWidgetImage@ action has the following limits:
--
--     * As many as 100 metrics in the graph.
--
--     * Up to 100 KB uncompressed payload.
--
--
--
module Network.AWS.CloudWatch.GetMetricWidgetImage
    (
    -- * Creating a Request
      getMetricWidgetImage
    , GetMetricWidgetImage
    -- * Request Lenses
    , gmwiOutputFormat
    , gmwiMetricWidget

    -- * Destructuring the Response
    , getMetricWidgetImageResponse
    , GetMetricWidgetImageResponse
    -- * Response Lenses
    , gmwirsMetricWidgetImage
    , gmwirsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMetricWidgetImage' smart constructor.
data GetMetricWidgetImage = GetMetricWidgetImage'
  { _gmwiOutputFormat :: !(Maybe Text)
  , _gmwiMetricWidget :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMetricWidgetImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwiOutputFormat' - The format of the resulting image. Only PNG images are supported. The default is @png@ . If you specify @png@ , the API returns an HTTP response with the content-type set to @text/xml@ . The image data is in a @MetricWidgetImage@ field. For example: @<GetMetricWidgetImageResponse xmlns="http://monitoring.amazonaws.com/doc/2010-08-01/">@  @<GetMetricWidgetImageResult>@  @<MetricWidgetImage>@  @iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@  @</MetricWidgetImage>@  @</GetMetricWidgetImageResult>@  @<ResponseMetadata>@  @<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b</RequestId>@  @</ResponseMetadata>@  @</GetMetricWidgetImageResponse>@  The @image/png@ setting is intended only for custom HTTP requests. For most use cases, and all actions using an AWS SDK, you should use @png@ . If you specify @image/png@ , the HTTP response has a content-type set to @image/png@ , and the body of the response is a PNG image.
--
-- * 'gmwiMetricWidget' - A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call. For more information about the syntax of @MetricWidget@ see 'CloudWatch-Metric-Widget-Structure' . If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
getMetricWidgetImage
    :: Text -- ^ 'gmwiMetricWidget'
    -> GetMetricWidgetImage
getMetricWidgetImage pMetricWidget_ =
  GetMetricWidgetImage'
    {_gmwiOutputFormat = Nothing, _gmwiMetricWidget = pMetricWidget_}


-- | The format of the resulting image. Only PNG images are supported. The default is @png@ . If you specify @png@ , the API returns an HTTP response with the content-type set to @text/xml@ . The image data is in a @MetricWidgetImage@ field. For example: @<GetMetricWidgetImageResponse xmlns="http://monitoring.amazonaws.com/doc/2010-08-01/">@  @<GetMetricWidgetImageResult>@  @<MetricWidgetImage>@  @iVBORw0KGgoAAAANSUhEUgAAAlgAAAGQEAYAAAAip...@  @</MetricWidgetImage>@  @</GetMetricWidgetImageResult>@  @<ResponseMetadata>@  @<RequestId>6f0d4192-4d42-11e8-82c1-f539a07e0e3b</RequestId>@  @</ResponseMetadata>@  @</GetMetricWidgetImageResponse>@  The @image/png@ setting is intended only for custom HTTP requests. For most use cases, and all actions using an AWS SDK, you should use @png@ . If you specify @image/png@ , the HTTP response has a content-type set to @image/png@ , and the body of the response is a PNG image.
gmwiOutputFormat :: Lens' GetMetricWidgetImage (Maybe Text)
gmwiOutputFormat = lens _gmwiOutputFormat (\ s a -> s{_gmwiOutputFormat = a})

-- | A JSON string that defines the bitmap graph to be retrieved. The string includes the metrics to include in the graph, statistics, annotations, title, axis limits, and so on. You can include only one @MetricWidget@ parameter in each @GetMetricWidgetImage@ call. For more information about the syntax of @MetricWidget@ see 'CloudWatch-Metric-Widget-Structure' . If any metric on the graph could not load all the requested data points, an orange triangle with an exclamation point appears next to the graph legend.
gmwiMetricWidget :: Lens' GetMetricWidgetImage Text
gmwiMetricWidget = lens _gmwiMetricWidget (\ s a -> s{_gmwiMetricWidget = a})

instance AWSRequest GetMetricWidgetImage where
        type Rs GetMetricWidgetImage =
             GetMetricWidgetImageResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "GetMetricWidgetImageResult"
              (\ s h x ->
                 GetMetricWidgetImageResponse' <$>
                   (x .@? "MetricWidgetImage") <*> (pure (fromEnum s)))

instance Hashable GetMetricWidgetImage where

instance NFData GetMetricWidgetImage where

instance ToHeaders GetMetricWidgetImage where
        toHeaders = const mempty

instance ToPath GetMetricWidgetImage where
        toPath = const "/"

instance ToQuery GetMetricWidgetImage where
        toQuery GetMetricWidgetImage'{..}
          = mconcat
              ["Action" =: ("GetMetricWidgetImage" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "OutputFormat" =: _gmwiOutputFormat,
               "MetricWidget" =: _gmwiMetricWidget]

-- | /See:/ 'getMetricWidgetImageResponse' smart constructor.
data GetMetricWidgetImageResponse = GetMetricWidgetImageResponse'
  { _gmwirsMetricWidgetImage :: !(Maybe Base64)
  , _gmwirsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMetricWidgetImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwirsMetricWidgetImage' - The image of the graph, in the output format specified.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gmwirsResponseStatus' - -- | The response status code.
getMetricWidgetImageResponse
    :: Int -- ^ 'gmwirsResponseStatus'
    -> GetMetricWidgetImageResponse
getMetricWidgetImageResponse pResponseStatus_ =
  GetMetricWidgetImageResponse'
    { _gmwirsMetricWidgetImage = Nothing
    , _gmwirsResponseStatus = pResponseStatus_
    }


-- | The image of the graph, in the output format specified.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gmwirsMetricWidgetImage :: Lens' GetMetricWidgetImageResponse (Maybe ByteString)
gmwirsMetricWidgetImage = lens _gmwirsMetricWidgetImage (\ s a -> s{_gmwirsMetricWidgetImage = a}) . mapping _Base64

-- | -- | The response status code.
gmwirsResponseStatus :: Lens' GetMetricWidgetImageResponse Int
gmwirsResponseStatus = lens _gmwirsResponseStatus (\ s a -> s{_gmwirsResponseStatus = a})

instance NFData GetMetricWidgetImageResponse where
