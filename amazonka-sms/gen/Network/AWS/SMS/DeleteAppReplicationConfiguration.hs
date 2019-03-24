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
-- Module      : Network.AWS.SMS.DeleteAppReplicationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes existing replication configuration for an application.
--
--
module Network.AWS.SMS.DeleteAppReplicationConfiguration
    (
    -- * Creating a Request
      deleteAppReplicationConfiguration
    , DeleteAppReplicationConfiguration
    -- * Request Lenses
    , darcAppId

    -- * Destructuring the Response
    , deleteAppReplicationConfigurationResponse
    , DeleteAppReplicationConfigurationResponse
    -- * Response Lenses
    , darcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'deleteAppReplicationConfiguration' smart constructor.
newtype DeleteAppReplicationConfiguration = DeleteAppReplicationConfiguration'
  { _darcAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAppReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darcAppId' - ID of the application associated with the replication configuration.
deleteAppReplicationConfiguration
    :: DeleteAppReplicationConfiguration
deleteAppReplicationConfiguration =
  DeleteAppReplicationConfiguration' {_darcAppId = Nothing}


-- | ID of the application associated with the replication configuration.
darcAppId :: Lens' DeleteAppReplicationConfiguration (Maybe Text)
darcAppId = lens _darcAppId (\ s a -> s{_darcAppId = a})

instance AWSRequest DeleteAppReplicationConfiguration
         where
        type Rs DeleteAppReplicationConfiguration =
             DeleteAppReplicationConfigurationResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAppReplicationConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteAppReplicationConfiguration
         where

instance NFData DeleteAppReplicationConfiguration
         where

instance ToHeaders DeleteAppReplicationConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.DeleteAppReplicationConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAppReplicationConfiguration
         where
        toJSON DeleteAppReplicationConfiguration'{..}
          = object (catMaybes [("appId" .=) <$> _darcAppId])

instance ToPath DeleteAppReplicationConfiguration
         where
        toPath = const "/"

instance ToQuery DeleteAppReplicationConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'deleteAppReplicationConfigurationResponse' smart constructor.
newtype DeleteAppReplicationConfigurationResponse = DeleteAppReplicationConfigurationResponse'
  { _darcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAppReplicationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darcrsResponseStatus' - -- | The response status code.
deleteAppReplicationConfigurationResponse
    :: Int -- ^ 'darcrsResponseStatus'
    -> DeleteAppReplicationConfigurationResponse
deleteAppReplicationConfigurationResponse pResponseStatus_ =
  DeleteAppReplicationConfigurationResponse'
    {_darcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
darcrsResponseStatus :: Lens' DeleteAppReplicationConfigurationResponse Int
darcrsResponseStatus = lens _darcrsResponseStatus (\ s a -> s{_darcrsResponseStatus = a})

instance NFData
           DeleteAppReplicationConfigurationResponse
         where
