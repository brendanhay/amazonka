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
-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a 'DataSource', rendering it unusable.
--
-- After using the 'DeleteDataSource' operation, you can use the
-- GetDataSource operation to verify that the status of the 'DataSource'
-- changed to DELETED.
--
-- Caution
--
-- The results of the 'DeleteDataSource' operation are irreversible.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteDataSource.html AWS API Reference> for DeleteDataSource.
module Network.AWS.MachineLearning.DeleteDataSource
    (
    -- * Creating a Request
      deleteDataSource
    , DeleteDataSource
    -- * Request Lenses
    , ddsDataSourceId

    -- * Destructuring the Response
    , deleteDataSourceResponse
    , DeleteDataSourceResponse
    -- * Response Lenses
    , ddsrsDataSourceId
    , ddsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteDataSource' smart constructor.
newtype DeleteDataSource = DeleteDataSource'
    { _ddsDataSourceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsDataSourceId'
deleteDataSource
    :: Text -- ^ 'ddsDataSourceId'
    -> DeleteDataSource
deleteDataSource pDataSourceId_ =
    DeleteDataSource'
    { _ddsDataSourceId = pDataSourceId_
    }

-- | A user-supplied ID that uniquely identifies the 'DataSource'.
ddsDataSourceId :: Lens' DeleteDataSource Text
ddsDataSourceId = lens _ddsDataSourceId (\ s a -> s{_ddsDataSourceId = a});

instance AWSRequest DeleteDataSource where
        type Rs DeleteDataSource = DeleteDataSourceResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDataSourceResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDataSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteDataSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDataSource where
        toJSON DeleteDataSource'{..}
          = object
              (catMaybes
                 [Just ("DataSourceId" .= _ddsDataSourceId)])

instance ToPath DeleteDataSource where
        toPath = const "/"

instance ToQuery DeleteDataSource where
        toQuery = const mempty

-- | Represents the output of a DeleteDataSource operation.
--
-- /See:/ 'deleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
    { _ddsrsDataSourceId :: !(Maybe Text)
    , _ddsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsDataSourceId'
--
-- * 'ddsrsStatus'
deleteDataSourceResponse
    :: Int -- ^ 'ddsrsStatus'
    -> DeleteDataSourceResponse
deleteDataSourceResponse pStatus_ =
    DeleteDataSourceResponse'
    { _ddsrsDataSourceId = Nothing
    , _ddsrsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the 'DataSource'. This value
-- should be identical to the value of the 'DataSourceID' in the request.
ddsrsDataSourceId :: Lens' DeleteDataSourceResponse (Maybe Text)
ddsrsDataSourceId = lens _ddsrsDataSourceId (\ s a -> s{_ddsrsDataSourceId = a});

-- | The response status code.
ddsrsStatus :: Lens' DeleteDataSourceResponse Int
ddsrsStatus = lens _ddsrsStatus (\ s a -> s{_ddsrsStatus = a});
