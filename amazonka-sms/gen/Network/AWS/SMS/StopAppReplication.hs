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
-- Module      : Network.AWS.SMS.StopAppReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replicating an application.
--
--
module Network.AWS.SMS.StopAppReplication
    (
    -- * Creating a Request
      stopAppReplication
    , StopAppReplication
    -- * Request Lenses
    , sAppId

    -- * Destructuring the Response
    , stopAppReplicationResponse
    , StopAppReplicationResponse
    -- * Response Lenses
    , sarrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'stopAppReplication' smart constructor.
newtype StopAppReplication = StopAppReplication'
  { _sAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopAppReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAppId' - ID of the application to stop replicating.
stopAppReplication
    :: StopAppReplication
stopAppReplication = StopAppReplication' {_sAppId = Nothing}


-- | ID of the application to stop replicating.
sAppId :: Lens' StopAppReplication (Maybe Text)
sAppId = lens _sAppId (\ s a -> s{_sAppId = a})

instance AWSRequest StopAppReplication where
        type Rs StopAppReplication =
             StopAppReplicationResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 StopAppReplicationResponse' <$> (pure (fromEnum s)))

instance Hashable StopAppReplication where

instance NFData StopAppReplication where

instance ToHeaders StopAppReplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.StopAppReplication"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopAppReplication where
        toJSON StopAppReplication'{..}
          = object (catMaybes [("appId" .=) <$> _sAppId])

instance ToPath StopAppReplication where
        toPath = const "/"

instance ToQuery StopAppReplication where
        toQuery = const mempty

-- | /See:/ 'stopAppReplicationResponse' smart constructor.
newtype StopAppReplicationResponse = StopAppReplicationResponse'
  { _sarrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopAppReplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarrsResponseStatus' - -- | The response status code.
stopAppReplicationResponse
    :: Int -- ^ 'sarrsResponseStatus'
    -> StopAppReplicationResponse
stopAppReplicationResponse pResponseStatus_ =
  StopAppReplicationResponse' {_sarrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sarrsResponseStatus :: Lens' StopAppReplicationResponse Int
sarrsResponseStatus = lens _sarrsResponseStatus (\ s a -> s{_sarrsResponseStatus = a})

instance NFData StopAppReplicationResponse where
