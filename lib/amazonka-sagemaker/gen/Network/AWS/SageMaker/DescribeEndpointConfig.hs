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
-- Module      : Network.AWS.SageMaker.DescribeEndpointConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint configuration created using the @CreateEndpointConfig@ API.
--
--
module Network.AWS.SageMaker.DescribeEndpointConfig
    (
    -- * Creating a Request
      describeEndpointConfig
    , DescribeEndpointConfig
    -- * Request Lenses
    , decEndpointConfigName

    -- * Destructuring the Response
    , describeEndpointConfigResponse
    , DescribeEndpointConfigResponse
    -- * Response Lenses
    , decrsKMSKeyId
    , decrsResponseStatus
    , decrsEndpointConfigName
    , decrsEndpointConfigARN
    , decrsProductionVariants
    , decrsCreationTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeEndpointConfig' smart constructor.
newtype DescribeEndpointConfig = DescribeEndpointConfig'
  { _decEndpointConfigName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decEndpointConfigName' - The name of the endpoint configuration.
describeEndpointConfig
    :: Text -- ^ 'decEndpointConfigName'
    -> DescribeEndpointConfig
describeEndpointConfig pEndpointConfigName_ =
  DescribeEndpointConfig' {_decEndpointConfigName = pEndpointConfigName_}


-- | The name of the endpoint configuration.
decEndpointConfigName :: Lens' DescribeEndpointConfig Text
decEndpointConfigName = lens _decEndpointConfigName (\ s a -> s{_decEndpointConfigName = a})

instance AWSRequest DescribeEndpointConfig where
        type Rs DescribeEndpointConfig =
             DescribeEndpointConfigResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointConfigResponse' <$>
                   (x .?> "KmsKeyId") <*> (pure (fromEnum s)) <*>
                     (x .:> "EndpointConfigName")
                     <*> (x .:> "EndpointConfigArn")
                     <*> (x .:> "ProductionVariants")
                     <*> (x .:> "CreationTime"))

instance Hashable DescribeEndpointConfig where

instance NFData DescribeEndpointConfig where

instance ToHeaders DescribeEndpointConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeEndpointConfig" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEndpointConfig where
        toJSON DescribeEndpointConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("EndpointConfigName" .= _decEndpointConfigName)])

instance ToPath DescribeEndpointConfig where
        toPath = const "/"

instance ToQuery DescribeEndpointConfig where
        toQuery = const mempty

-- | /See:/ 'describeEndpointConfigResponse' smart constructor.
data DescribeEndpointConfigResponse = DescribeEndpointConfigResponse'
  { _decrsKMSKeyId           :: !(Maybe Text)
  , _decrsResponseStatus     :: !Int
  , _decrsEndpointConfigName :: !Text
  , _decrsEndpointConfigARN  :: !Text
  , _decrsProductionVariants :: !(List1 ProductionVariant)
  , _decrsCreationTime       :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsKMSKeyId' - AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- * 'decrsResponseStatus' - -- | The response status code.
--
-- * 'decrsEndpointConfigName' - Name of the Amazon SageMaker endpoint configuration.
--
-- * 'decrsEndpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- * 'decrsProductionVariants' - An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- * 'decrsCreationTime' - A timestamp that shows when the endpoint configuration was created.
describeEndpointConfigResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> Text -- ^ 'decrsEndpointConfigName'
    -> Text -- ^ 'decrsEndpointConfigARN'
    -> NonEmpty ProductionVariant -- ^ 'decrsProductionVariants'
    -> UTCTime -- ^ 'decrsCreationTime'
    -> DescribeEndpointConfigResponse
describeEndpointConfigResponse pResponseStatus_ pEndpointConfigName_ pEndpointConfigARN_ pProductionVariants_ pCreationTime_ =
  DescribeEndpointConfigResponse'
    { _decrsKMSKeyId = Nothing
    , _decrsResponseStatus = pResponseStatus_
    , _decrsEndpointConfigName = pEndpointConfigName_
    , _decrsEndpointConfigARN = pEndpointConfigARN_
    , _decrsProductionVariants = _List1 # pProductionVariants_
    , _decrsCreationTime = _Time # pCreationTime_
    }


-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
decrsKMSKeyId :: Lens' DescribeEndpointConfigResponse (Maybe Text)
decrsKMSKeyId = lens _decrsKMSKeyId (\ s a -> s{_decrsKMSKeyId = a})

-- | -- | The response status code.
decrsResponseStatus :: Lens' DescribeEndpointConfigResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a})

-- | Name of the Amazon SageMaker endpoint configuration.
decrsEndpointConfigName :: Lens' DescribeEndpointConfigResponse Text
decrsEndpointConfigName = lens _decrsEndpointConfigName (\ s a -> s{_decrsEndpointConfigName = a})

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
decrsEndpointConfigARN :: Lens' DescribeEndpointConfigResponse Text
decrsEndpointConfigARN = lens _decrsEndpointConfigARN (\ s a -> s{_decrsEndpointConfigARN = a})

-- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
decrsProductionVariants :: Lens' DescribeEndpointConfigResponse (NonEmpty ProductionVariant)
decrsProductionVariants = lens _decrsProductionVariants (\ s a -> s{_decrsProductionVariants = a}) . _List1

-- | A timestamp that shows when the endpoint configuration was created.
decrsCreationTime :: Lens' DescribeEndpointConfigResponse UTCTime
decrsCreationTime = lens _decrsCreationTime (\ s a -> s{_decrsCreationTime = a}) . _Time

instance NFData DescribeEndpointConfigResponse where
