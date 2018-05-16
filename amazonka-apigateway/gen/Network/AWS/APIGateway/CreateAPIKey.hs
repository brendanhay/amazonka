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
-- Module      : Network.AWS.APIGateway.CreateAPIKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an 'ApiKey' resource.
--
--
-- <http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html AWS CLI>
module Network.AWS.APIGateway.CreateAPIKey
    (
    -- * Creating a Request
      createAPIKey
    , CreateAPIKey
    -- * Request Lenses
    , cakEnabled
    , cakValue
    , cakCustomerId
    , cakGenerateDistinctId
    , cakName
    , cakStageKeys
    , cakDescription

    -- * Destructuring the Response
    , apiKey
    , APIKey
    -- * Response Lenses
    , akEnabled
    , akValue
    , akCustomerId
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to create an 'ApiKey' resource.
--
--
--
-- /See:/ 'createAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
  { _cakEnabled            :: !(Maybe Bool)
  , _cakValue              :: !(Maybe Text)
  , _cakCustomerId         :: !(Maybe Text)
  , _cakGenerateDistinctId :: !(Maybe Bool)
  , _cakName               :: !(Maybe Text)
  , _cakStageKeys          :: !(Maybe [StageKey])
  , _cakDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakEnabled' - Specifies whether the 'ApiKey' can be used by callers.
--
-- * 'cakValue' - Specifies a value of the API key.
--
-- * 'cakCustomerId' - An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- * 'cakGenerateDistinctId' - Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value.
--
-- * 'cakName' - The name of the 'ApiKey' .
--
-- * 'cakStageKeys' - DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
--
-- * 'cakDescription' - The description of the 'ApiKey' .
createAPIKey
    :: CreateAPIKey
createAPIKey =
  CreateAPIKey'
    { _cakEnabled = Nothing
    , _cakValue = Nothing
    , _cakCustomerId = Nothing
    , _cakGenerateDistinctId = Nothing
    , _cakName = Nothing
    , _cakStageKeys = Nothing
    , _cakDescription = Nothing
    }


-- | Specifies whether the 'ApiKey' can be used by callers.
cakEnabled :: Lens' CreateAPIKey (Maybe Bool)
cakEnabled = lens _cakEnabled (\ s a -> s{_cakEnabled = a})

-- | Specifies a value of the API key.
cakValue :: Lens' CreateAPIKey (Maybe Text)
cakValue = lens _cakValue (\ s a -> s{_cakValue = a})

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
cakCustomerId :: Lens' CreateAPIKey (Maybe Text)
cakCustomerId = lens _cakCustomerId (\ s a -> s{_cakCustomerId = a})

-- | Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value.
cakGenerateDistinctId :: Lens' CreateAPIKey (Maybe Bool)
cakGenerateDistinctId = lens _cakGenerateDistinctId (\ s a -> s{_cakGenerateDistinctId = a})

-- | The name of the 'ApiKey' .
cakName :: Lens' CreateAPIKey (Maybe Text)
cakName = lens _cakName (\ s a -> s{_cakName = a})

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
cakStageKeys :: Lens' CreateAPIKey [StageKey]
cakStageKeys = lens _cakStageKeys (\ s a -> s{_cakStageKeys = a}) . _Default . _Coerce

-- | The description of the 'ApiKey' .
cakDescription :: Lens' CreateAPIKey (Maybe Text)
cakDescription = lens _cakDescription (\ s a -> s{_cakDescription = a})

instance AWSRequest CreateAPIKey where
        type Rs CreateAPIKey = APIKey
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAPIKey where

instance NFData CreateAPIKey where

instance ToHeaders CreateAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateAPIKey where
        toJSON CreateAPIKey'{..}
          = object
              (catMaybes
                 [("enabled" .=) <$> _cakEnabled,
                  ("value" .=) <$> _cakValue,
                  ("customerId" .=) <$> _cakCustomerId,
                  ("generateDistinctId" .=) <$> _cakGenerateDistinctId,
                  ("name" .=) <$> _cakName,
                  ("stageKeys" .=) <$> _cakStageKeys,
                  ("description" .=) <$> _cakDescription])

instance ToPath CreateAPIKey where
        toPath = const "/apikeys"

instance ToQuery CreateAPIKey where
        toQuery = const mempty
