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
-- Module      : Network.AWS.SMS.PutAppReplicationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a replication configuration for an application.
--
--
module Network.AWS.SMS.PutAppReplicationConfiguration
    (
    -- * Creating a Request
      putAppReplicationConfiguration
    , PutAppReplicationConfiguration
    -- * Request Lenses
    , parcAppId
    , parcServerGroupReplicationConfigurations

    -- * Destructuring the Response
    , putAppReplicationConfigurationResponse
    , PutAppReplicationConfigurationResponse
    -- * Response Lenses
    , parcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'putAppReplicationConfiguration' smart constructor.
data PutAppReplicationConfiguration = PutAppReplicationConfiguration'
  { _parcAppId :: !(Maybe Text)
  , _parcServerGroupReplicationConfigurations :: !(Maybe [ServerGroupReplicationConfiguration])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parcAppId' - ID of the application tassociated with the replication configuration.
--
-- * 'parcServerGroupReplicationConfigurations' - Replication configurations for server groups in the application.
putAppReplicationConfiguration
    :: PutAppReplicationConfiguration
putAppReplicationConfiguration =
  PutAppReplicationConfiguration'
    {_parcAppId = Nothing, _parcServerGroupReplicationConfigurations = Nothing}


-- | ID of the application tassociated with the replication configuration.
parcAppId :: Lens' PutAppReplicationConfiguration (Maybe Text)
parcAppId = lens _parcAppId (\ s a -> s{_parcAppId = a})

-- | Replication configurations for server groups in the application.
parcServerGroupReplicationConfigurations :: Lens' PutAppReplicationConfiguration [ServerGroupReplicationConfiguration]
parcServerGroupReplicationConfigurations = lens _parcServerGroupReplicationConfigurations (\ s a -> s{_parcServerGroupReplicationConfigurations = a}) . _Default . _Coerce

instance AWSRequest PutAppReplicationConfiguration
         where
        type Rs PutAppReplicationConfiguration =
             PutAppReplicationConfigurationResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 PutAppReplicationConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutAppReplicationConfiguration
         where

instance NFData PutAppReplicationConfiguration where

instance ToHeaders PutAppReplicationConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.PutAppReplicationConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAppReplicationConfiguration where
        toJSON PutAppReplicationConfiguration'{..}
          = object
              (catMaybes
                 [("appId" .=) <$> _parcAppId,
                  ("serverGroupReplicationConfigurations" .=) <$>
                    _parcServerGroupReplicationConfigurations])

instance ToPath PutAppReplicationConfiguration where
        toPath = const "/"

instance ToQuery PutAppReplicationConfiguration where
        toQuery = const mempty

-- | /See:/ 'putAppReplicationConfigurationResponse' smart constructor.
newtype PutAppReplicationConfigurationResponse = PutAppReplicationConfigurationResponse'
  { _parcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parcrsResponseStatus' - -- | The response status code.
putAppReplicationConfigurationResponse
    :: Int -- ^ 'parcrsResponseStatus'
    -> PutAppReplicationConfigurationResponse
putAppReplicationConfigurationResponse pResponseStatus_ =
  PutAppReplicationConfigurationResponse'
    {_parcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
parcrsResponseStatus :: Lens' PutAppReplicationConfigurationResponse Int
parcrsResponseStatus = lens _parcrsResponseStatus (\ s a -> s{_parcrsResponseStatus = a})

instance NFData
           PutAppReplicationConfigurationResponse
         where
