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
-- Module      : Network.AWS.IoTAnalytics.CreateDatastore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data store, which is a repository for messages.
--
--
module Network.AWS.IoTAnalytics.CreateDatastore
    (
    -- * Creating a Request
      createDatastore
    , CreateDatastore
    -- * Request Lenses
    , cdRetentionPeriod
    , cdDatastoreName

    -- * Destructuring the Response
    , createDatastoreResponse
    , CreateDatastoreResponse
    -- * Response Lenses
    , cdrsDatastoreARN
    , cdrsDatastoreName
    , cdrsRetentionPeriod
    , cdrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatastore' smart constructor.
data CreateDatastore = CreateDatastore'
  { _cdRetentionPeriod :: !(Maybe RetentionPeriod)
  , _cdDatastoreName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdRetentionPeriod' - How long, in days, message data is kept for the data store.
--
-- * 'cdDatastoreName' - The name of the data store.
createDatastore
    :: Text -- ^ 'cdDatastoreName'
    -> CreateDatastore
createDatastore pDatastoreName_ =
  CreateDatastore'
    {_cdRetentionPeriod = Nothing, _cdDatastoreName = pDatastoreName_}


-- | How long, in days, message data is kept for the data store.
cdRetentionPeriod :: Lens' CreateDatastore (Maybe RetentionPeriod)
cdRetentionPeriod = lens _cdRetentionPeriod (\ s a -> s{_cdRetentionPeriod = a})

-- | The name of the data store.
cdDatastoreName :: Lens' CreateDatastore Text
cdDatastoreName = lens _cdDatastoreName (\ s a -> s{_cdDatastoreName = a})

instance AWSRequest CreateDatastore where
        type Rs CreateDatastore = CreateDatastoreResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatastoreResponse' <$>
                   (x .?> "datastoreArn") <*> (x .?> "datastoreName")
                     <*> (x .?> "retentionPeriod")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDatastore where

instance NFData CreateDatastore where

instance ToHeaders CreateDatastore where
        toHeaders = const mempty

instance ToJSON CreateDatastore where
        toJSON CreateDatastore'{..}
          = object
              (catMaybes
                 [("retentionPeriod" .=) <$> _cdRetentionPeriod,
                  Just ("datastoreName" .= _cdDatastoreName)])

instance ToPath CreateDatastore where
        toPath = const "/datastores"

instance ToQuery CreateDatastore where
        toQuery = const mempty

-- | /See:/ 'createDatastoreResponse' smart constructor.
data CreateDatastoreResponse = CreateDatastoreResponse'
  { _cdrsDatastoreARN    :: !(Maybe Text)
  , _cdrsDatastoreName   :: !(Maybe Text)
  , _cdrsRetentionPeriod :: !(Maybe RetentionPeriod)
  , _cdrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatastoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDatastoreARN' - The ARN of the data store.
--
-- * 'cdrsDatastoreName' - The name of the data store.
--
-- * 'cdrsRetentionPeriod' - How long, in days, message data is kept for the data store.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDatastoreResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDatastoreResponse
createDatastoreResponse pResponseStatus_ =
  CreateDatastoreResponse'
    { _cdrsDatastoreARN = Nothing
    , _cdrsDatastoreName = Nothing
    , _cdrsRetentionPeriod = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the data store.
cdrsDatastoreARN :: Lens' CreateDatastoreResponse (Maybe Text)
cdrsDatastoreARN = lens _cdrsDatastoreARN (\ s a -> s{_cdrsDatastoreARN = a})

-- | The name of the data store.
cdrsDatastoreName :: Lens' CreateDatastoreResponse (Maybe Text)
cdrsDatastoreName = lens _cdrsDatastoreName (\ s a -> s{_cdrsDatastoreName = a})

-- | How long, in days, message data is kept for the data store.
cdrsRetentionPeriod :: Lens' CreateDatastoreResponse (Maybe RetentionPeriod)
cdrsRetentionPeriod = lens _cdrsRetentionPeriod (\ s a -> s{_cdrsRetentionPeriod = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDatastoreResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDatastoreResponse where
