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
-- Module      : Network.AWS.IoTAnalytics.DeleteDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified data set.
--
--
-- You do not have to delete the content of the data set before you perform this operation.
--
module Network.AWS.IoTAnalytics.DeleteDataset
    (
    -- * Creating a Request
      deleteDataset
    , DeleteDataset
    -- * Request Lenses
    , dDatasetName

    -- * Destructuring the Response
    , deleteDatasetResponse
    , DeleteDatasetResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDataset' smart constructor.
newtype DeleteDataset = DeleteDataset'
  { _dDatasetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDatasetName' - The name of the data set to delete.
deleteDataset
    :: Text -- ^ 'dDatasetName'
    -> DeleteDataset
deleteDataset pDatasetName_ = DeleteDataset' {_dDatasetName = pDatasetName_}


-- | The name of the data set to delete.
dDatasetName :: Lens' DeleteDataset Text
dDatasetName = lens _dDatasetName (\ s a -> s{_dDatasetName = a})

instance AWSRequest DeleteDataset where
        type Rs DeleteDataset = DeleteDatasetResponse
        request = delete ioTAnalytics
        response = receiveNull DeleteDatasetResponse'

instance Hashable DeleteDataset where

instance NFData DeleteDataset where

instance ToHeaders DeleteDataset where
        toHeaders = const mempty

instance ToPath DeleteDataset where
        toPath DeleteDataset'{..}
          = mconcat ["/datasets/", toBS _dDatasetName]

instance ToQuery DeleteDataset where
        toQuery = const mempty

-- | /See:/ 'deleteDatasetResponse' smart constructor.
data DeleteDatasetResponse =
  DeleteDatasetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatasetResponse' with the minimum fields required to make a request.
--
deleteDatasetResponse
    :: DeleteDatasetResponse
deleteDatasetResponse = DeleteDatasetResponse'


instance NFData DeleteDatasetResponse where
