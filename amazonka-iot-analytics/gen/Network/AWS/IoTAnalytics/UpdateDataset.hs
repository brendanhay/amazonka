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
-- Module      : Network.AWS.IoTAnalytics.UpdateDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data set.
--
--
module Network.AWS.IoTAnalytics.UpdateDataset
    (
    -- * Creating a Request
      updateDataset
    , UpdateDataset
    -- * Request Lenses
    , udTriggers
    , udDatasetName
    , udActions

    -- * Destructuring the Response
    , updateDatasetResponse
    , UpdateDatasetResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { _udTriggers    :: !(Maybe [DatasetTrigger])
  , _udDatasetName :: !Text
  , _udActions     :: !(List1 DatasetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udTriggers' - A list of "DatasetTrigger" objects. The list can be empty or can contain up to five __DataSetTrigger__ objects.
--
-- * 'udDatasetName' - The name of the data set to update.
--
-- * 'udActions' - A list of "DatasetAction" objects. Only one action is supported at this time.
updateDataset
    :: Text -- ^ 'udDatasetName'
    -> NonEmpty DatasetAction -- ^ 'udActions'
    -> UpdateDataset
updateDataset pDatasetName_ pActions_ =
  UpdateDataset'
    { _udTriggers = Nothing
    , _udDatasetName = pDatasetName_
    , _udActions = _List1 # pActions_
    }


-- | A list of "DatasetTrigger" objects. The list can be empty or can contain up to five __DataSetTrigger__ objects.
udTriggers :: Lens' UpdateDataset [DatasetTrigger]
udTriggers = lens _udTriggers (\ s a -> s{_udTriggers = a}) . _Default . _Coerce

-- | The name of the data set to update.
udDatasetName :: Lens' UpdateDataset Text
udDatasetName = lens _udDatasetName (\ s a -> s{_udDatasetName = a})

-- | A list of "DatasetAction" objects. Only one action is supported at this time.
udActions :: Lens' UpdateDataset (NonEmpty DatasetAction)
udActions = lens _udActions (\ s a -> s{_udActions = a}) . _List1

instance AWSRequest UpdateDataset where
        type Rs UpdateDataset = UpdateDatasetResponse
        request = putJSON ioTAnalytics
        response = receiveNull UpdateDatasetResponse'

instance Hashable UpdateDataset where

instance NFData UpdateDataset where

instance ToHeaders UpdateDataset where
        toHeaders = const mempty

instance ToJSON UpdateDataset where
        toJSON UpdateDataset'{..}
          = object
              (catMaybes
                 [("triggers" .=) <$> _udTriggers,
                  Just ("actions" .= _udActions)])

instance ToPath UpdateDataset where
        toPath UpdateDataset'{..}
          = mconcat ["/datasets/", toBS _udDatasetName]

instance ToQuery UpdateDataset where
        toQuery = const mempty

-- | /See:/ 'updateDatasetResponse' smart constructor.
data UpdateDatasetResponse =
  UpdateDatasetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatasetResponse' with the minimum fields required to make a request.
--
updateDatasetResponse
    :: UpdateDatasetResponse
updateDatasetResponse = UpdateDatasetResponse'


instance NFData UpdateDatasetResponse where
