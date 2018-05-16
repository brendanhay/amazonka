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
-- Module      : Network.AWS.SageMaker.CreateEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the endpoint configuration specified in the request. Amazon SageMaker uses the endpoint to provision resources and deploy models. You create the endpoint configuration with the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpointConfig.html CreateEndpointConfig> API.
--
--
-- The endpoint name must be unique within an AWS Region in your AWS account.
--
-- When it receives the request, Amazon SageMaker creates the endpoint, launches the resources (ML compute instances), and deploys the model(s) on them.
--
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Creating@ . After it creates the endpoint, it sets the status to @InService@ . Amazon SageMaker can then process incoming requests for inferences. To check the status of an endpoint, use the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_DescribeEndpoint.html DescribeEndpoint> API.
--
-- For an example, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ex1.html Exercise 1: Using the K-Means Algorithm Provided by Amazon SageMaker> .
--
module Network.AWS.SageMaker.CreateEndpoint
    (
    -- * Creating a Request
      createEndpoint
    , CreateEndpoint
    -- * Request Lenses
    , ceTags
    , ceEndpointName
    , ceEndpointConfigName

    -- * Destructuring the Response
    , createEndpointResponse
    , CreateEndpointResponse
    -- * Response Lenses
    , cersResponseStatus
    , cersEndpointARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { _ceTags               :: !(Maybe [Tag])
  , _ceEndpointName       :: !Text
  , _ceEndpointConfigName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceTags' - An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'ceEndpointName' - The name of the endpoint. The name must be unique within an AWS Region in your AWS account.
--
-- * 'ceEndpointConfigName' - The name of an endpoint configuration. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpointConfig.html CreateEndpointConfig> .
createEndpoint
    :: Text -- ^ 'ceEndpointName'
    -> Text -- ^ 'ceEndpointConfigName'
    -> CreateEndpoint
createEndpoint pEndpointName_ pEndpointConfigName_ =
  CreateEndpoint'
    { _ceTags = Nothing
    , _ceEndpointName = pEndpointName_
    , _ceEndpointConfigName = pEndpointConfigName_
    }


-- | An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
ceTags :: Lens' CreateEndpoint [Tag]
ceTags = lens _ceTags (\ s a -> s{_ceTags = a}) . _Default . _Coerce

-- | The name of the endpoint. The name must be unique within an AWS Region in your AWS account.
ceEndpointName :: Lens' CreateEndpoint Text
ceEndpointName = lens _ceEndpointName (\ s a -> s{_ceEndpointName = a})

-- | The name of an endpoint configuration. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpointConfig.html CreateEndpointConfig> .
ceEndpointConfigName :: Lens' CreateEndpoint Text
ceEndpointConfigName = lens _ceEndpointConfigName (\ s a -> s{_ceEndpointConfigName = a})

instance AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "EndpointArn"))

instance Hashable CreateEndpoint where

instance NFData CreateEndpoint where

instance ToHeaders CreateEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEndpoint where
        toJSON CreateEndpoint'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _ceTags,
                  Just ("EndpointName" .= _ceEndpointName),
                  Just
                    ("EndpointConfigName" .= _ceEndpointConfigName)])

instance ToPath CreateEndpoint where
        toPath = const "/"

instance ToQuery CreateEndpoint where
        toQuery = const mempty

-- | /See:/ 'createEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { _cersResponseStatus :: !Int
  , _cersEndpointARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersResponseStatus' - -- | The response status code.
--
-- * 'cersEndpointARN' - The Amazon Resource Name (ARN) of the endpoint.
createEndpointResponse
    :: Int -- ^ 'cersResponseStatus'
    -> Text -- ^ 'cersEndpointARN'
    -> CreateEndpointResponse
createEndpointResponse pResponseStatus_ pEndpointARN_ =
  CreateEndpointResponse'
    {_cersResponseStatus = pResponseStatus_, _cersEndpointARN = pEndpointARN_}


-- | -- | The response status code.
cersResponseStatus :: Lens' CreateEndpointResponse Int
cersResponseStatus = lens _cersResponseStatus (\ s a -> s{_cersResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the endpoint.
cersEndpointARN :: Lens' CreateEndpointResponse Text
cersEndpointARN = lens _cersEndpointARN (\ s a -> s{_cersEndpointARN = a})

instance NFData CreateEndpointResponse where
