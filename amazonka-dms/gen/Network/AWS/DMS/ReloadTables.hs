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
-- Module      : Network.AWS.DMS.ReloadTables
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reloads the target database table with the source data.
--
--
module Network.AWS.DMS.ReloadTables
    (
    -- * Creating a Request
      reloadTables
    , ReloadTables
    -- * Request Lenses
    , rtReplicationTaskARN
    , rtTablesToReload

    -- * Destructuring the Response
    , reloadTablesResponse
    , ReloadTablesResponse
    -- * Response Lenses
    , rtrsReplicationTaskARN
    , rtrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'reloadTables' smart constructor.
data ReloadTables = ReloadTables'
  { _rtReplicationTaskARN :: !Text
  , _rtTablesToReload     :: ![TableToReload]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReloadTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rtTablesToReload' - The name and schema of the table to be reloaded.
reloadTables
    :: Text -- ^ 'rtReplicationTaskARN'
    -> ReloadTables
reloadTables pReplicationTaskARN_ =
  ReloadTables'
    {_rtReplicationTaskARN = pReplicationTaskARN_, _rtTablesToReload = mempty}


-- | The Amazon Resource Name (ARN) of the replication instance.
rtReplicationTaskARN :: Lens' ReloadTables Text
rtReplicationTaskARN = lens _rtReplicationTaskARN (\ s a -> s{_rtReplicationTaskARN = a})

-- | The name and schema of the table to be reloaded.
rtTablesToReload :: Lens' ReloadTables [TableToReload]
rtTablesToReload = lens _rtTablesToReload (\ s a -> s{_rtTablesToReload = a}) . _Coerce

instance AWSRequest ReloadTables where
        type Rs ReloadTables = ReloadTablesResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ReloadTablesResponse' <$>
                   (x .?> "ReplicationTaskArn") <*> (pure (fromEnum s)))

instance Hashable ReloadTables where

instance NFData ReloadTables where

instance ToHeaders ReloadTables where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ReloadTables" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReloadTables where
        toJSON ReloadTables'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationTaskArn" .= _rtReplicationTaskARN),
                  Just ("TablesToReload" .= _rtTablesToReload)])

instance ToPath ReloadTables where
        toPath = const "/"

instance ToQuery ReloadTables where
        toQuery = const mempty

-- | /See:/ 'reloadTablesResponse' smart constructor.
data ReloadTablesResponse = ReloadTablesResponse'
  { _rtrsReplicationTaskARN :: !(Maybe Text)
  , _rtrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReloadTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'rtrsResponseStatus' - -- | The response status code.
reloadTablesResponse
    :: Int -- ^ 'rtrsResponseStatus'
    -> ReloadTablesResponse
reloadTablesResponse pResponseStatus_ =
  ReloadTablesResponse'
    {_rtrsReplicationTaskARN = Nothing, _rtrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the replication task.
rtrsReplicationTaskARN :: Lens' ReloadTablesResponse (Maybe Text)
rtrsReplicationTaskARN = lens _rtrsReplicationTaskARN (\ s a -> s{_rtrsReplicationTaskARN = a})

-- | -- | The response status code.
rtrsResponseStatus :: Lens' ReloadTablesResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a})

instance NFData ReloadTablesResponse where
