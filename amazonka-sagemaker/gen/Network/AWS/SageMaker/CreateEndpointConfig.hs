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
-- Module      : Network.AWS.SageMaker.CreateEndpointConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint configuration that Amazon SageMaker hosting services uses to deploy models. In the configuration, you identify one or more models, created using the @CreateModel@ API, to deploy and the resources that you want Amazon SageMaker to provision. Then you call the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
--
-- In the request, you define one or more @ProductionVariant@ s, each of which identifies a model. Each @ProductionVariant@ parameter also describes the resources that you want Amazon SageMaker to provision. This includes the number and type of ML compute instances to deploy.
--
-- If you are hosting multiple models, you also assign a @VariantWeight@ to specify how much traffic you want to allocate to each model. For example, suppose that you want to host two models, A and B, and you assign traffic weight 2 for model A and 1 for model B. Amazon SageMaker distributes two-thirds of the traffic to Model A, and one-third to model B.
--
module Network.AWS.SageMaker.CreateEndpointConfig
    (
    -- * Creating a Request
      createEndpointConfig
    , CreateEndpointConfig
    -- * Request Lenses
    , cecKMSKeyId
    , cecTags
    , cecEndpointConfigName
    , cecProductionVariants

    -- * Destructuring the Response
    , createEndpointConfigResponse
    , CreateEndpointConfigResponse
    -- * Response Lenses
    , cecrsResponseStatus
    , cecrsEndpointConfigARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createEndpointConfig' smart constructor.
data CreateEndpointConfig = CreateEndpointConfig'
  { _cecKMSKeyId           :: !(Maybe Text)
  , _cecTags               :: !(Maybe [Tag])
  , _cecEndpointConfigName :: !Text
  , _cecProductionVariants :: !(List1 ProductionVariant)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpointConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cecKMSKeyId' - The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
--
-- * 'cecTags' - An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cecEndpointConfigName' - The name of the endpoint configuration. You specify this name in a <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> request.
--
-- * 'cecProductionVariants' - An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
createEndpointConfig
    :: Text -- ^ 'cecEndpointConfigName'
    -> NonEmpty ProductionVariant -- ^ 'cecProductionVariants'
    -> CreateEndpointConfig
createEndpointConfig pEndpointConfigName_ pProductionVariants_ =
  CreateEndpointConfig'
    { _cecKMSKeyId = Nothing
    , _cecTags = Nothing
    , _cecEndpointConfigName = pEndpointConfigName_
    , _cecProductionVariants = _List1 # pProductionVariants_
    }


-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
cecKMSKeyId :: Lens' CreateEndpointConfig (Maybe Text)
cecKMSKeyId = lens _cecKMSKeyId (\ s a -> s{_cecKMSKeyId = a})

-- | An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cecTags :: Lens' CreateEndpointConfig [Tag]
cecTags = lens _cecTags (\ s a -> s{_cecTags = a}) . _Default . _Coerce

-- | The name of the endpoint configuration. You specify this name in a <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> request.
cecEndpointConfigName :: Lens' CreateEndpointConfig Text
cecEndpointConfigName = lens _cecEndpointConfigName (\ s a -> s{_cecEndpointConfigName = a})

-- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
cecProductionVariants :: Lens' CreateEndpointConfig (NonEmpty ProductionVariant)
cecProductionVariants = lens _cecProductionVariants (\ s a -> s{_cecProductionVariants = a}) . _List1

instance AWSRequest CreateEndpointConfig where
        type Rs CreateEndpointConfig =
             CreateEndpointConfigResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateEndpointConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "EndpointConfigArn"))

instance Hashable CreateEndpointConfig where

instance NFData CreateEndpointConfig where

instance ToHeaders CreateEndpointConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateEndpointConfig" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEndpointConfig where
        toJSON CreateEndpointConfig'{..}
          = object
              (catMaybes
                 [("KmsKeyId" .=) <$> _cecKMSKeyId,
                  ("Tags" .=) <$> _cecTags,
                  Just
                    ("EndpointConfigName" .= _cecEndpointConfigName),
                  Just
                    ("ProductionVariants" .= _cecProductionVariants)])

instance ToPath CreateEndpointConfig where
        toPath = const "/"

instance ToQuery CreateEndpointConfig where
        toQuery = const mempty

-- | /See:/ 'createEndpointConfigResponse' smart constructor.
data CreateEndpointConfigResponse = CreateEndpointConfigResponse'
  { _cecrsResponseStatus    :: !Int
  , _cecrsEndpointConfigARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpointConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cecrsResponseStatus' - -- | The response status code.
--
-- * 'cecrsEndpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
createEndpointConfigResponse
    :: Int -- ^ 'cecrsResponseStatus'
    -> Text -- ^ 'cecrsEndpointConfigARN'
    -> CreateEndpointConfigResponse
createEndpointConfigResponse pResponseStatus_ pEndpointConfigARN_ =
  CreateEndpointConfigResponse'
    { _cecrsResponseStatus = pResponseStatus_
    , _cecrsEndpointConfigARN = pEndpointConfigARN_
    }


-- | -- | The response status code.
cecrsResponseStatus :: Lens' CreateEndpointConfigResponse Int
cecrsResponseStatus = lens _cecrsResponseStatus (\ s a -> s{_cecrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
cecrsEndpointConfigARN :: Lens' CreateEndpointConfigResponse Text
cecrsEndpointConfigARN = lens _cecrsEndpointConfigARN (\ s a -> s{_cecrsEndpointConfigARN = a})

instance NFData CreateEndpointConfigResponse where
