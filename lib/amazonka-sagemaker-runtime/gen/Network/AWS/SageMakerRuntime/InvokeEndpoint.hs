{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint.
--
--
-- For an overview of Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
--
-- Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax.
--
-- Calls to @InvokeEndpoint@ are authenticated by using AWS Signature Version 4. For information, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> in the /Amazon S3 API Reference/ .
--
-- A customer's model containers must respond to requests within 60 seconds. The model itself can have a maximum processing time of 60 seconds before responding to the /invocations. If your model is going to take 50-60 seconds of processing time, the SDK socket timeout should be set to be 70 seconds.
module Network.AWS.SageMakerRuntime.InvokeEndpoint
  ( -- * Creating a Request
    invokeEndpoint,
    InvokeEndpoint,

    -- * Request Lenses
    ieAccept,
    ieTargetModel,
    ieCustomAttributes,
    ieTargetVariant,
    ieContentType,
    ieEndpointName,
    ieBody,

    -- * Destructuring the Response
    invokeEndpointResponse,
    InvokeEndpointResponse,

    -- * Response Lenses
    iersInvokedProductionVariant,
    iersCustomAttributes,
    iersContentType,
    iersResponseStatus,
    iersBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMakerRuntime.Types

-- | /See:/ 'invokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { _ieAccept :: !(Maybe Text),
    _ieTargetModel :: !(Maybe Text),
    _ieCustomAttributes :: !(Maybe (Sensitive Text)),
    _ieTargetVariant :: !(Maybe Text),
    _ieContentType :: !(Maybe Text),
    _ieEndpointName :: !Text,
    _ieBody :: !ByteString
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InvokeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieAccept' - The desired MIME type of the inference in the response.
--
-- * 'ieTargetModel' - The model to request for inference when invoking a multi-model endpoint.
--
-- * 'ieCustomAttributes' - Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- * 'ieTargetVariant' - Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
--
-- * 'ieContentType' - The MIME type of the input data in the request body.
--
-- * 'ieEndpointName' - The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
-- * 'ieBody' - Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.  For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
invokeEndpoint ::
  -- | 'ieEndpointName'
  Text ->
  -- | 'ieBody'
  ByteString ->
  InvokeEndpoint
invokeEndpoint pEndpointName_ pBody_ =
  InvokeEndpoint'
    { _ieAccept = Nothing,
      _ieTargetModel = Nothing,
      _ieCustomAttributes = Nothing,
      _ieTargetVariant = Nothing,
      _ieContentType = Nothing,
      _ieEndpointName = pEndpointName_,
      _ieBody = pBody_
    }

-- | The desired MIME type of the inference in the response.
ieAccept :: Lens' InvokeEndpoint (Maybe Text)
ieAccept = lens _ieAccept (\s a -> s {_ieAccept = a})

-- | The model to request for inference when invoking a multi-model endpoint.
ieTargetModel :: Lens' InvokeEndpoint (Maybe Text)
ieTargetModel = lens _ieTargetModel (\s a -> s {_ieTargetModel = a})

-- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
ieCustomAttributes :: Lens' InvokeEndpoint (Maybe Text)
ieCustomAttributes = lens _ieCustomAttributes (\s a -> s {_ieCustomAttributes = a}) . mapping _Sensitive

-- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
ieTargetVariant :: Lens' InvokeEndpoint (Maybe Text)
ieTargetVariant = lens _ieTargetVariant (\s a -> s {_ieTargetVariant = a})

-- | The MIME type of the input data in the request body.
ieContentType :: Lens' InvokeEndpoint (Maybe Text)
ieContentType = lens _ieContentType (\s a -> s {_ieContentType = a})

-- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
ieEndpointName :: Lens' InvokeEndpoint Text
ieEndpointName = lens _ieEndpointName (\s a -> s {_ieEndpointName = a})

-- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.  For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
ieBody :: Lens' InvokeEndpoint ByteString
ieBody = lens _ieBody (\s a -> s {_ieBody = a})

instance AWSRequest InvokeEndpoint where
  type Rs InvokeEndpoint = InvokeEndpointResponse
  request = postBody sageMakerRuntime
  response =
    receiveBytes
      ( \s h x ->
          InvokeEndpointResponse'
            <$> (h .#? "x-Amzn-Invoked-Production-Variant")
            <*> (h .#? "X-Amzn-SageMaker-Custom-Attributes")
            <*> (h .#? "Content-Type")
            <*> (pure (fromEnum s))
            <*> (pure x)
      )

instance Hashable InvokeEndpoint

instance NFData InvokeEndpoint

instance ToBody InvokeEndpoint where
  toBody = toBody . _ieBody

instance ToHeaders InvokeEndpoint where
  toHeaders InvokeEndpoint' {..} =
    mconcat
      [ "Accept" =# _ieAccept,
        "X-Amzn-SageMaker-Target-Model" =# _ieTargetModel,
        "X-Amzn-SageMaker-Custom-Attributes" =# _ieCustomAttributes,
        "X-Amzn-SageMaker-Target-Variant" =# _ieTargetVariant,
        "Content-Type" =# _ieContentType
      ]

instance ToPath InvokeEndpoint where
  toPath InvokeEndpoint' {..} =
    mconcat ["/endpoints/", toBS _ieEndpointName, "/invocations"]

instance ToQuery InvokeEndpoint where
  toQuery = const mempty

-- | /See:/ 'invokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { _iersInvokedProductionVariant ::
      !(Maybe Text),
    _iersCustomAttributes ::
      !(Maybe (Sensitive Text)),
    _iersContentType :: !(Maybe Text),
    _iersResponseStatus :: !Int,
    _iersBody :: !ByteString
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InvokeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iersInvokedProductionVariant' - Identifies the production variant that was invoked.
--
-- * 'iersCustomAttributes' - Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.  This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- * 'iersContentType' - The MIME type of the inference returned in the response body.
--
-- * 'iersResponseStatus' - -- | The response status code.
--
-- * 'iersBody' - Includes the inference provided by the model. For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
invokeEndpointResponse ::
  -- | 'iersResponseStatus'
  Int ->
  -- | 'iersBody'
  ByteString ->
  InvokeEndpointResponse
invokeEndpointResponse pResponseStatus_ pBody_ =
  InvokeEndpointResponse'
    { _iersInvokedProductionVariant = Nothing,
      _iersCustomAttributes = Nothing,
      _iersContentType = Nothing,
      _iersResponseStatus = pResponseStatus_,
      _iersBody = pBody_
    }

-- | Identifies the production variant that was invoked.
iersInvokedProductionVariant :: Lens' InvokeEndpointResponse (Maybe Text)
iersInvokedProductionVariant = lens _iersInvokedProductionVariant (\s a -> s {_iersInvokedProductionVariant = a})

-- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.  This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
iersCustomAttributes :: Lens' InvokeEndpointResponse (Maybe Text)
iersCustomAttributes = lens _iersCustomAttributes (\s a -> s {_iersCustomAttributes = a}) . mapping _Sensitive

-- | The MIME type of the inference returned in the response body.
iersContentType :: Lens' InvokeEndpointResponse (Maybe Text)
iersContentType = lens _iersContentType (\s a -> s {_iersContentType = a})

-- | -- | The response status code.
iersResponseStatus :: Lens' InvokeEndpointResponse Int
iersResponseStatus = lens _iersResponseStatus (\s a -> s {_iersResponseStatus = a})

-- | Includes the inference provided by the model. For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
iersBody :: Lens' InvokeEndpointResponse ByteString
iersBody = lens _iersBody (\s a -> s {_iersBody = a})

instance NFData InvokeEndpointResponse
