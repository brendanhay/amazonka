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
-- Module      : Network.AWS.Config.GetAggregateResourceConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration item that is aggregated for your specific resource in a specific source account and region.
--
--
module Network.AWS.Config.GetAggregateResourceConfig
    (
    -- * Creating a Request
      getAggregateResourceConfig
    , GetAggregateResourceConfig
    -- * Request Lenses
    , garcConfigurationAggregatorName
    , garcResourceIdentifier

    -- * Destructuring the Response
    , getAggregateResourceConfigResponse
    , GetAggregateResourceConfigResponse
    -- * Response Lenses
    , garcrsConfigurationItem
    , garcrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { _garcConfigurationAggregatorName :: !Text
  , _garcResourceIdentifier          :: !AggregateResourceIdentifier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garcConfigurationAggregatorName' - The name of the configuration aggregator.
--
-- * 'garcResourceIdentifier' - An object that identifies aggregate resource.
getAggregateResourceConfig
    :: Text -- ^ 'garcConfigurationAggregatorName'
    -> AggregateResourceIdentifier -- ^ 'garcResourceIdentifier'
    -> GetAggregateResourceConfig
getAggregateResourceConfig pConfigurationAggregatorName_ pResourceIdentifier_ =
  GetAggregateResourceConfig'
    { _garcConfigurationAggregatorName = pConfigurationAggregatorName_
    , _garcResourceIdentifier = pResourceIdentifier_
    }


-- | The name of the configuration aggregator.
garcConfigurationAggregatorName :: Lens' GetAggregateResourceConfig Text
garcConfigurationAggregatorName = lens _garcConfigurationAggregatorName (\ s a -> s{_garcConfigurationAggregatorName = a})

-- | An object that identifies aggregate resource.
garcResourceIdentifier :: Lens' GetAggregateResourceConfig AggregateResourceIdentifier
garcResourceIdentifier = lens _garcResourceIdentifier (\ s a -> s{_garcResourceIdentifier = a})

instance AWSRequest GetAggregateResourceConfig where
        type Rs GetAggregateResourceConfig =
             GetAggregateResourceConfigResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetAggregateResourceConfigResponse' <$>
                   (x .?> "ConfigurationItem") <*> (pure (fromEnum s)))

instance Hashable GetAggregateResourceConfig where

instance NFData GetAggregateResourceConfig where

instance ToHeaders GetAggregateResourceConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetAggregateResourceConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAggregateResourceConfig where
        toJSON GetAggregateResourceConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("ConfigurationAggregatorName" .=
                       _garcConfigurationAggregatorName),
                  Just
                    ("ResourceIdentifier" .= _garcResourceIdentifier)])

instance ToPath GetAggregateResourceConfig where
        toPath = const "/"

instance ToQuery GetAggregateResourceConfig where
        toQuery = const mempty

-- | /See:/ 'getAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { _garcrsConfigurationItem :: !(Maybe ConfigurationItem)
  , _garcrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateResourceConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garcrsConfigurationItem' - Returns a @ConfigurationItem@ object.
--
-- * 'garcrsResponseStatus' - -- | The response status code.
getAggregateResourceConfigResponse
    :: Int -- ^ 'garcrsResponseStatus'
    -> GetAggregateResourceConfigResponse
getAggregateResourceConfigResponse pResponseStatus_ =
  GetAggregateResourceConfigResponse'
    { _garcrsConfigurationItem = Nothing
    , _garcrsResponseStatus = pResponseStatus_
    }


-- | Returns a @ConfigurationItem@ object.
garcrsConfigurationItem :: Lens' GetAggregateResourceConfigResponse (Maybe ConfigurationItem)
garcrsConfigurationItem = lens _garcrsConfigurationItem (\ s a -> s{_garcrsConfigurationItem = a})

-- | -- | The response status code.
garcrsResponseStatus :: Lens' GetAggregateResourceConfigResponse Int
garcrsResponseStatus = lens _garcrsResponseStatus (\ s a -> s{_garcrsResponseStatus = a})

instance NFData GetAggregateResourceConfigResponse
         where
