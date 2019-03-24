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
-- Module      : Network.AWS.IoTAnalytics.DescribeDatastore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data store.
--
--
module Network.AWS.IoTAnalytics.DescribeDatastore
    (
    -- * Creating a Request
      describeDatastore
    , DescribeDatastore
    -- * Request Lenses
    , dIncludeStatistics
    , dDatastoreName

    -- * Destructuring the Response
    , describeDatastoreResponse
    , DescribeDatastoreResponse
    -- * Response Lenses
    , drsDatastore
    , drsStatistics
    , drsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDatastore' smart constructor.
data DescribeDatastore = DescribeDatastore'
  { _dIncludeStatistics :: !(Maybe Bool)
  , _dDatastoreName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dIncludeStatistics' - If true, additional statistical information about the datastore is included in the response.
--
-- * 'dDatastoreName' - The name of the data store
describeDatastore
    :: Text -- ^ 'dDatastoreName'
    -> DescribeDatastore
describeDatastore pDatastoreName_ =
  DescribeDatastore'
    {_dIncludeStatistics = Nothing, _dDatastoreName = pDatastoreName_}


-- | If true, additional statistical information about the datastore is included in the response.
dIncludeStatistics :: Lens' DescribeDatastore (Maybe Bool)
dIncludeStatistics = lens _dIncludeStatistics (\ s a -> s{_dIncludeStatistics = a})

-- | The name of the data store
dDatastoreName :: Lens' DescribeDatastore Text
dDatastoreName = lens _dDatastoreName (\ s a -> s{_dDatastoreName = a})

instance AWSRequest DescribeDatastore where
        type Rs DescribeDatastore = DescribeDatastoreResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatastoreResponse' <$>
                   (x .?> "datastore") <*> (x .?> "statistics") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDatastore where

instance NFData DescribeDatastore where

instance ToHeaders DescribeDatastore where
        toHeaders = const mempty

instance ToPath DescribeDatastore where
        toPath DescribeDatastore'{..}
          = mconcat ["/datastores/", toBS _dDatastoreName]

instance ToQuery DescribeDatastore where
        toQuery DescribeDatastore'{..}
          = mconcat
              ["includeStatistics" =: _dIncludeStatistics]

-- | /See:/ 'describeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { _drsDatastore      :: !(Maybe Datastore)
  , _drsStatistics     :: !(Maybe DatastoreStatistics)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatastoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDatastore' - Information about the data store.
--
-- * 'drsStatistics' - Additional statistical information about the data store. Included if the 'includeStatistics' parameter is set to true in the request.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeDatastoreResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeDatastoreResponse
describeDatastoreResponse pResponseStatus_ =
  DescribeDatastoreResponse'
    { _drsDatastore = Nothing
    , _drsStatistics = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Information about the data store.
drsDatastore :: Lens' DescribeDatastoreResponse (Maybe Datastore)
drsDatastore = lens _drsDatastore (\ s a -> s{_drsDatastore = a})

-- | Additional statistical information about the data store. Included if the 'includeStatistics' parameter is set to true in the request.
drsStatistics :: Lens' DescribeDatastoreResponse (Maybe DatastoreStatistics)
drsStatistics = lens _drsStatistics (\ s a -> s{_drsStatistics = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeDatastoreResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeDatastoreResponse where
