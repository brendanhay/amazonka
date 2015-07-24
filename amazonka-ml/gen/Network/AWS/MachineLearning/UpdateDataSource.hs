{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the @DataSourceName@ of a @DataSource@.
--
-- You can use the GetDataSource operation to view the contents of the
-- updated data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateDataSource.html>
module Network.AWS.MachineLearning.UpdateDataSource
    (
    -- * Request
      UpdateDataSource
    -- ** Request constructor
    , updateDataSource
    -- ** Request lenses
    , udsDataSourceId
    , udsDataSourceName

    -- * Response
    , UpdateDataSourceResponse
    -- ** Response constructor
    , updateDataSourceResponse
    -- ** Response lenses
    , udsrsDataSourceId
    , udsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateDataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsDataSourceId'
--
-- * 'udsDataSourceName'
data UpdateDataSource = UpdateDataSource'
    { _udsDataSourceId   :: !Text
    , _udsDataSourceName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDataSource' smart constructor.
updateDataSource :: Text -> Text -> UpdateDataSource
updateDataSource pDataSourceId_ pDataSourceName_ =
    UpdateDataSource'
    { _udsDataSourceId = pDataSourceId_
    , _udsDataSourceName = pDataSourceName_
    }

-- | The ID assigned to the @DataSource@ during creation.
udsDataSourceId :: Lens' UpdateDataSource Text
udsDataSourceId = lens _udsDataSourceId (\ s a -> s{_udsDataSourceId = a});

-- | A new user-supplied name or description of the @DataSource@ that will
-- replace the current description.
udsDataSourceName :: Lens' UpdateDataSource Text
udsDataSourceName = lens _udsDataSourceName (\ s a -> s{_udsDataSourceName = a});

instance AWSRequest UpdateDataSource where
        type Sv UpdateDataSource = MachineLearning
        type Rs UpdateDataSource = UpdateDataSourceResponse
        request = postJSON
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
              ["DataSourceId" .= _udsDataSourceId,
               "DataSourceName" .= _udsDataSourceName]

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsrsDataSourceId'
--
-- * 'udsrsStatus'
data UpdateDataSourceResponse = UpdateDataSourceResponse'
    { _udsrsDataSourceId :: !(Maybe Text)
    , _udsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDataSourceResponse' smart constructor.
updateDataSourceResponse :: Int -> UpdateDataSourceResponse
updateDataSourceResponse pStatus_ =
    UpdateDataSourceResponse'
    { _udsrsDataSourceId = Nothing
    , _udsrsStatus = pStatus_
    }

-- | The ID assigned to the @DataSource@ during creation. This value should
-- be identical to the value of the @DataSourceID@ in the request.
udsrsDataSourceId :: Lens' UpdateDataSourceResponse (Maybe Text)
udsrsDataSourceId = lens _udsrsDataSourceId (\ s a -> s{_udsrsDataSourceId = a});

-- | FIXME: Undocumented member.
udsrsStatus :: Lens' UpdateDataSourceResponse Int
udsrsStatus = lens _udsrsStatus (\ s a -> s{_udsrsStatus = a});
