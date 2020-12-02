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
-- Module      : Network.AWS.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint.
--
--
-- For an overview of Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works>
--
-- Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax.
--
module Network.AWS.SageMakerRuntime.InvokeEndpoint
    (
    -- * Creating a Request
      invokeEndpoint
    , InvokeEndpoint
    -- * Request Lenses
    , ieAccept
    , ieContentType
    , ieEndpointName
    , ieBody

    -- * Destructuring the Response
    , invokeEndpointResponse
    , InvokeEndpointResponse
    -- * Response Lenses
    , iersInvokedProductionVariant
    , iersContentType
    , iersResponseStatus
    , iersBody
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMakerRuntime.Types
import Network.AWS.SageMakerRuntime.Types.Product

-- | /See:/ 'invokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { _ieAccept       :: !(Maybe Text)
  , _ieContentType  :: !(Maybe Text)
  , _ieEndpointName :: !Text
  , _ieBody         :: !ByteString
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvokeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieAccept' - The desired MIME type of the inference in the response.
--
-- * 'ieContentType' - The MIME type of the input data in the request body.
--
-- * 'ieEndpointName' - The name of the endpoint that you specified when you created the endpoint using the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
-- * 'ieBody' - Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
invokeEndpoint
    :: Text -- ^ 'ieEndpointName'
    -> ByteString -- ^ 'ieBody'
    -> InvokeEndpoint
invokeEndpoint pEndpointName_ pBody_ =
  InvokeEndpoint'
    { _ieAccept = Nothing
    , _ieContentType = Nothing
    , _ieEndpointName = pEndpointName_
    , _ieBody = pBody_
    }


-- | The desired MIME type of the inference in the response.
ieAccept :: Lens' InvokeEndpoint (Maybe Text)
ieAccept = lens _ieAccept (\ s a -> s{_ieAccept = a})

-- | The MIME type of the input data in the request body.
ieContentType :: Lens' InvokeEndpoint (Maybe Text)
ieContentType = lens _ieContentType (\ s a -> s{_ieContentType = a})

-- | The name of the endpoint that you specified when you created the endpoint using the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
ieEndpointName :: Lens' InvokeEndpoint Text
ieEndpointName = lens _ieEndpointName (\ s a -> s{_ieEndpointName = a})

-- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
ieBody :: Lens' InvokeEndpoint ByteString
ieBody = lens _ieBody (\ s a -> s{_ieBody = a})

instance AWSRequest InvokeEndpoint where
        type Rs InvokeEndpoint = InvokeEndpointResponse
        request = postBody sageMakerRuntime
        response
          = receiveBytes
              (\ s h x ->
                 InvokeEndpointResponse' <$>
                   (h .#? "x-Amzn-Invoked-Production-Variant") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance Hashable InvokeEndpoint where

instance NFData InvokeEndpoint where

instance ToBody InvokeEndpoint where
        toBody = toBody . _ieBody

instance ToHeaders InvokeEndpoint where
        toHeaders InvokeEndpoint'{..}
          = mconcat
              ["Accept" =# _ieAccept,
               "Content-Type" =# _ieContentType]

instance ToPath InvokeEndpoint where
        toPath InvokeEndpoint'{..}
          = mconcat
              ["/endpoints/", toBS _ieEndpointName, "/invocations"]

instance ToQuery InvokeEndpoint where
        toQuery = const mempty

-- | /See:/ 'invokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { _iersInvokedProductionVariant :: !(Maybe Text)
  , _iersContentType              :: !(Maybe Text)
  , _iersResponseStatus           :: !Int
  , _iersBody                     :: !ByteString
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvokeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iersInvokedProductionVariant' - Identifies the production variant that was invoked.
--
-- * 'iersContentType' - The MIME type of the inference returned in the response body.
--
-- * 'iersResponseStatus' - -- | The response status code.
--
-- * 'iersBody' - Includes the inference provided by the model.
invokeEndpointResponse
    :: Int -- ^ 'iersResponseStatus'
    -> ByteString -- ^ 'iersBody'
    -> InvokeEndpointResponse
invokeEndpointResponse pResponseStatus_ pBody_ =
  InvokeEndpointResponse'
    { _iersInvokedProductionVariant = Nothing
    , _iersContentType = Nothing
    , _iersResponseStatus = pResponseStatus_
    , _iersBody = pBody_
    }


-- | Identifies the production variant that was invoked.
iersInvokedProductionVariant :: Lens' InvokeEndpointResponse (Maybe Text)
iersInvokedProductionVariant = lens _iersInvokedProductionVariant (\ s a -> s{_iersInvokedProductionVariant = a})

-- | The MIME type of the inference returned in the response body.
iersContentType :: Lens' InvokeEndpointResponse (Maybe Text)
iersContentType = lens _iersContentType (\ s a -> s{_iersContentType = a})

-- | -- | The response status code.
iersResponseStatus :: Lens' InvokeEndpointResponse Int
iersResponseStatus = lens _iersResponseStatus (\ s a -> s{_iersResponseStatus = a})

-- | Includes the inference provided by the model.
iersBody :: Lens' InvokeEndpointResponse ByteString
iersBody = lens _iersBody (\ s a -> s{_iersBody = a})

instance NFData InvokeEndpointResponse where
