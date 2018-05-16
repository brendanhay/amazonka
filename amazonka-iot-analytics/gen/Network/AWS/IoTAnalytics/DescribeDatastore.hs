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
    , dDatastoreName

    -- * Destructuring the Response
    , describeDatastoreResponse
    , DescribeDatastoreResponse
    -- * Response Lenses
    , drsDatastore
    , drsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDatastore' smart constructor.
newtype DescribeDatastore = DescribeDatastore'
  { _dDatastoreName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDatastoreName' - The name of the data store
describeDatastore
    :: Text -- ^ 'dDatastoreName'
    -> DescribeDatastore
describeDatastore pDatastoreName_ =
  DescribeDatastore' {_dDatastoreName = pDatastoreName_}


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
                   (x .?> "datastore") <*> (pure (fromEnum s)))

instance Hashable DescribeDatastore where

instance NFData DescribeDatastore where

instance ToHeaders DescribeDatastore where
        toHeaders = const mempty

instance ToPath DescribeDatastore where
        toPath DescribeDatastore'{..}
          = mconcat ["/datastores/", toBS _dDatastoreName]

instance ToQuery DescribeDatastore where
        toQuery = const mempty

-- | /See:/ 'describeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { _drsDatastore      :: !(Maybe Datastore)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatastoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDatastore' - Information about the data store.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeDatastoreResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeDatastoreResponse
describeDatastoreResponse pResponseStatus_ =
  DescribeDatastoreResponse'
    {_drsDatastore = Nothing, _drsResponseStatus = pResponseStatus_}


-- | Information about the data store.
drsDatastore :: Lens' DescribeDatastoreResponse (Maybe Datastore)
drsDatastore = lens _drsDatastore (\ s a -> s{_drsDatastore = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeDatastoreResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeDatastoreResponse where
