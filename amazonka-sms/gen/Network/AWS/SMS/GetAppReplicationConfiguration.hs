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
-- Module      : Network.AWS.SMS.GetAppReplicationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an application replication configuration associatd with an application.
--
--
module Network.AWS.SMS.GetAppReplicationConfiguration
    (
    -- * Creating a Request
      getAppReplicationConfiguration
    , GetAppReplicationConfiguration
    -- * Request Lenses
    , garcAppId

    -- * Destructuring the Response
    , getAppReplicationConfigurationResponse
    , GetAppReplicationConfigurationResponse
    -- * Response Lenses
    , garcrsServerGroupReplicationConfigurations
    , garcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getAppReplicationConfiguration' smart constructor.
newtype GetAppReplicationConfiguration = GetAppReplicationConfiguration'
  { _garcAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garcAppId' - ID of the application associated with the replication configuration.
getAppReplicationConfiguration
    :: GetAppReplicationConfiguration
getAppReplicationConfiguration =
  GetAppReplicationConfiguration' {_garcAppId = Nothing}


-- | ID of the application associated with the replication configuration.
garcAppId :: Lens' GetAppReplicationConfiguration (Maybe Text)
garcAppId = lens _garcAppId (\ s a -> s{_garcAppId = a})

instance AWSRequest GetAppReplicationConfiguration
         where
        type Rs GetAppReplicationConfiguration =
             GetAppReplicationConfigurationResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetAppReplicationConfigurationResponse' <$>
                   (x .?> "serverGroupReplicationConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetAppReplicationConfiguration
         where

instance NFData GetAppReplicationConfiguration where

instance ToHeaders GetAppReplicationConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetAppReplicationConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAppReplicationConfiguration where
        toJSON GetAppReplicationConfiguration'{..}
          = object (catMaybes [("appId" .=) <$> _garcAppId])

instance ToPath GetAppReplicationConfiguration where
        toPath = const "/"

instance ToQuery GetAppReplicationConfiguration where
        toQuery = const mempty

-- | /See:/ 'getAppReplicationConfigurationResponse' smart constructor.
data GetAppReplicationConfigurationResponse = GetAppReplicationConfigurationResponse'
  { _garcrsServerGroupReplicationConfigurations :: !(Maybe [ServerGroupReplicationConfiguration])
  , _garcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garcrsServerGroupReplicationConfigurations' - Replication configurations associated with server groups in this application.
--
-- * 'garcrsResponseStatus' - -- | The response status code.
getAppReplicationConfigurationResponse
    :: Int -- ^ 'garcrsResponseStatus'
    -> GetAppReplicationConfigurationResponse
getAppReplicationConfigurationResponse pResponseStatus_ =
  GetAppReplicationConfigurationResponse'
    { _garcrsServerGroupReplicationConfigurations = Nothing
    , _garcrsResponseStatus = pResponseStatus_
    }


-- | Replication configurations associated with server groups in this application.
garcrsServerGroupReplicationConfigurations :: Lens' GetAppReplicationConfigurationResponse [ServerGroupReplicationConfiguration]
garcrsServerGroupReplicationConfigurations = lens _garcrsServerGroupReplicationConfigurations (\ s a -> s{_garcrsServerGroupReplicationConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
garcrsResponseStatus :: Lens' GetAppReplicationConfigurationResponse Int
garcrsResponseStatus = lens _garcrsResponseStatus (\ s a -> s{_garcrsResponseStatus = a})

instance NFData
           GetAppReplicationConfigurationResponse
         where
