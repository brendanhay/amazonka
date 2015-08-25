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
-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the 'DataSourceName' of a 'DataSource'.
--
-- You can use the GetDataSource operation to view the contents of the
-- updated data element.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateDataSource.html AWS API Reference> for UpdateDataSource.
module Network.AWS.MachineLearning.UpdateDataSource
    (
    -- * Creating a Request
      updateDataSource
    , UpdateDataSource
    -- * Request Lenses
    , udsDataSourceId
    , udsDataSourceName

    -- * Destructuring the Response
    , updateDataSourceResponse
    , UpdateDataSourceResponse
    -- * Response Lenses
    , udsrsDataSourceId
    , udsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
    { _udsDataSourceId   :: !Text
    , _udsDataSourceName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsDataSourceId'
--
-- * 'udsDataSourceName'
updateDataSource
    :: Text -- ^ 'udsDataSourceId'
    -> Text -- ^ 'udsDataSourceName'
    -> UpdateDataSource
updateDataSource pDataSourceId_ pDataSourceName_ =
    UpdateDataSource'
    { _udsDataSourceId = pDataSourceId_
    , _udsDataSourceName = pDataSourceName_
    }

-- | The ID assigned to the 'DataSource' during creation.
udsDataSourceId :: Lens' UpdateDataSource Text
udsDataSourceId = lens _udsDataSourceId (\ s a -> s{_udsDataSourceId = a});

-- | A new user-supplied name or description of the 'DataSource' that will
-- replace the current description.
udsDataSourceName :: Lens' UpdateDataSource Text
udsDataSourceName = lens _udsDataSourceName (\ s a -> s{_udsDataSourceName = a});

instance AWSRequest UpdateDataSource where
        type Rs UpdateDataSource = UpdateDataSourceResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDataSourceResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateDataSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateDataSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDataSource where
        toJSON UpdateDataSource'{..}
          = object
              (catMaybes
                 [Just ("DataSourceId" .= _udsDataSourceId),
                  Just ("DataSourceName" .= _udsDataSourceName)])

instance ToPath UpdateDataSource where
        toPath = const "/"

instance ToQuery UpdateDataSource where
        toQuery = const mempty

-- | Represents the output of an UpdateDataSource operation.
--
-- You can see the updated content by using the GetBatchPrediction
-- operation.
--
-- /See:/ 'updateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
    { _udsrsDataSourceId :: !(Maybe Text)
    , _udsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsrsDataSourceId'
--
-- * 'udsrsStatus'
updateDataSourceResponse
    :: Int -- ^ 'udsrsStatus'
    -> UpdateDataSourceResponse
updateDataSourceResponse pStatus_ =
    UpdateDataSourceResponse'
    { _udsrsDataSourceId = Nothing
    , _udsrsStatus = pStatus_
    }

-- | The ID assigned to the 'DataSource' during creation. This value should
-- be identical to the value of the 'DataSourceID' in the request.
udsrsDataSourceId :: Lens' UpdateDataSourceResponse (Maybe Text)
udsrsDataSourceId = lens _udsrsDataSourceId (\ s a -> s{_udsrsDataSourceId = a});

-- | The response status code.
udsrsStatus :: Lens' UpdateDataSourceResponse Int
udsrsStatus = lens _udsrsStatus (\ s a -> s{_udsrsStatus = a});
