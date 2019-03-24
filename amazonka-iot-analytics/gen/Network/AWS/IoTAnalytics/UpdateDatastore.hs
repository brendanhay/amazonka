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
    , uRetentionPeriod
    , uDatastoreName

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
  { _uRetentionPeriod :: !(Maybe RetentionPeriod)
  , _uDatastoreName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uRetentionPeriod' - How long, in days, message data is kept for the data store.
--
-- * 'uDatastoreName' - The name of the data store to be updated.
updateDatastore
    :: Text -- ^ 'uDatastoreName'
    -> UpdateDatastore
updateDatastore pDatastoreName_ =
  UpdateDatastore'
    {_uRetentionPeriod = Nothing, _uDatastoreName = pDatastoreName_}


-- | How long, in days, message data is kept for the data store.
uRetentionPeriod :: Lens' UpdateDatastore (Maybe RetentionPeriod)
uRetentionPeriod = lens _uRetentionPeriod (\ s a -> s{_uRetentionPeriod = a})

-- | The name of the data store to be updated.
uDatastoreName :: Lens' UpdateDatastore Text
uDatastoreName = lens _uDatastoreName (\ s a -> s{_uDatastoreName = a})

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
                 [("retentionPeriod" .=) <$> _uRetentionPeriod])

instance ToPath UpdateDatastore where
        toPath UpdateDatastore'{..}
          = mconcat ["/datastores/", toBS _uDatastoreName]

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
