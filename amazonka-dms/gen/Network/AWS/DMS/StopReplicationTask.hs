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
-- Module      : Network.AWS.DMS.StopReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the replication task.
--
--
--
--
module Network.AWS.DMS.StopReplicationTask
    (
    -- * Creating a Request
      stopReplicationTask
    , StopReplicationTask
    -- * Request Lenses
    , sReplicationTaskARN

    -- * Destructuring the Response
    , stopReplicationTaskResponse
    , StopReplicationTaskResponse
    -- * Response Lenses
    , srsReplicationTask
    , srsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'stopReplicationTask' smart constructor.
newtype StopReplicationTask = StopReplicationTask'
  { _sReplicationTaskARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sReplicationTaskARN' - The Amazon Resource Name(ARN) of the replication task to be stopped.
stopReplicationTask
    :: Text -- ^ 'sReplicationTaskARN'
    -> StopReplicationTask
stopReplicationTask pReplicationTaskARN_ =
  StopReplicationTask' {_sReplicationTaskARN = pReplicationTaskARN_}


-- | The Amazon Resource Name(ARN) of the replication task to be stopped.
sReplicationTaskARN :: Lens' StopReplicationTask Text
sReplicationTaskARN = lens _sReplicationTaskARN (\ s a -> s{_sReplicationTaskARN = a})

instance AWSRequest StopReplicationTask where
        type Rs StopReplicationTask =
             StopReplicationTaskResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 StopReplicationTaskResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable StopReplicationTask where

instance NFData StopReplicationTask where

instance ToHeaders StopReplicationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.StopReplicationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopReplicationTask where
        toJSON StopReplicationTask'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationTaskArn" .= _sReplicationTaskARN)])

instance ToPath StopReplicationTask where
        toPath = const "/"

instance ToQuery StopReplicationTask where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'stopReplicationTaskResponse' smart constructor.
data StopReplicationTaskResponse = StopReplicationTaskResponse'
  { _srsReplicationTask :: !(Maybe ReplicationTask)
  , _srsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsReplicationTask' - The replication task stopped.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopReplicationTaskResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopReplicationTaskResponse
stopReplicationTaskResponse pResponseStatus_ =
  StopReplicationTaskResponse'
    {_srsReplicationTask = Nothing, _srsResponseStatus = pResponseStatus_}


-- | The replication task stopped.
srsReplicationTask :: Lens' StopReplicationTaskResponse (Maybe ReplicationTask)
srsReplicationTask = lens _srsReplicationTask (\ s a -> s{_srsReplicationTask = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopReplicationTaskResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopReplicationTaskResponse where
