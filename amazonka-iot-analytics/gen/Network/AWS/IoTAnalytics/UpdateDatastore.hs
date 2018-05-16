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
-- Module      : Network.AWS.IoTAnalytics.UpdateDatastore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data store.
--
--
module Network.AWS.IoTAnalytics.UpdateDatastore
    (
    -- * Creating a Request
      updateDatastore
    , UpdateDatastore
    -- * Request Lenses
    , udRetentionPeriod
    , udDatastoreName

    -- * Destructuring the Response
    , updateDatastoreResponse
    , UpdateDatastoreResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDatastore' smart constructor.
data UpdateDatastore = UpdateDatastore'
  { _udRetentionPeriod :: !(Maybe RetentionPeriod)
  , _udDatastoreName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udRetentionPeriod' - How long, in days, message data is kept for the data store.
--
-- * 'udDatastoreName' - The name of the data store to be updated.
updateDatastore
    :: Text -- ^ 'udDatastoreName'
    -> UpdateDatastore
updateDatastore pDatastoreName_ =
  UpdateDatastore'
    {_udRetentionPeriod = Nothing, _udDatastoreName = pDatastoreName_}


-- | How long, in days, message data is kept for the data store.
udRetentionPeriod :: Lens' UpdateDatastore (Maybe RetentionPeriod)
udRetentionPeriod = lens _udRetentionPeriod (\ s a -> s{_udRetentionPeriod = a})

-- | The name of the data store to be updated.
udDatastoreName :: Lens' UpdateDatastore Text
udDatastoreName = lens _udDatastoreName (\ s a -> s{_udDatastoreName = a})

instance AWSRequest UpdateDatastore where
        type Rs UpdateDatastore = UpdateDatastoreResponse
        request = putJSON ioTAnalytics
        response = receiveNull UpdateDatastoreResponse'

instance Hashable UpdateDatastore where

instance NFData UpdateDatastore where

instance ToHeaders UpdateDatastore where
        toHeaders = const mempty

instance ToJSON UpdateDatastore where
        toJSON UpdateDatastore'{..}
          = object
              (catMaybes
                 [("retentionPeriod" .=) <$> _udRetentionPeriod])

instance ToPath UpdateDatastore where
        toPath UpdateDatastore'{..}
          = mconcat ["/datastores/", toBS _udDatastoreName]

instance ToQuery UpdateDatastore where
        toQuery = const mempty

-- | /See:/ 'updateDatastoreResponse' smart constructor.
data UpdateDatastoreResponse =
  UpdateDatastoreResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatastoreResponse' with the minimum fields required to make a request.
--
updateDatastoreResponse
    :: UpdateDatastoreResponse
updateDatastoreResponse = UpdateDatastoreResponse'


instance NFData UpdateDatastoreResponse where
