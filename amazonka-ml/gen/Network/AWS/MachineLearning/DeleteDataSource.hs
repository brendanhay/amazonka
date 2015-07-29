{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @DataSource@, rendering it unusable.
--
-- After using the @DeleteDataSource@ operation, you can use the
-- GetDataSource operation to verify that the status of the @DataSource@
-- changed to DELETED.
--
-- Caution
--
-- The results of the @DeleteDataSource@ operation are irreversible.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteDataSource.html>
module Network.AWS.MachineLearning.DeleteDataSource
    (
    -- * Request
      DeleteDataSource
    -- ** Request constructor
    , deleteDataSource
    -- ** Request lenses
    , ddsDataSourceId

    -- * Response
    , DeleteDataSourceResponse
    -- ** Response constructor
    , deleteDataSourceResponse
    -- ** Response lenses
    , ddsrsDataSourceId
    , ddsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteDataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsDataSourceId'
newtype DeleteDataSource = DeleteDataSource'
    { _ddsDataSourceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDataSource' smart constructor.
deleteDataSource :: Text -> DeleteDataSource
deleteDataSource pDataSourceId_ =
    DeleteDataSource'
    { _ddsDataSourceId = pDataSourceId_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@.
ddsDataSourceId :: Lens' DeleteDataSource Text
ddsDataSourceId = lens _ddsDataSourceId (\ s a -> s{_ddsDataSourceId = a});

instance AWSRequest DeleteDataSource where
        type Sv DeleteDataSource = MachineLearning
        type Rs DeleteDataSource = DeleteDataSourceResponse
        request = postJSON
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
          = object ["DataSourceId" .= _ddsDataSourceId]

instance ToPath DeleteDataSource where
        toPath = const mempty

instance ToQuery DeleteDataSource where
        toQuery = const mempty

-- | Represents the output of a DeleteDataSource operation.
--
-- /See:/ 'deleteDataSourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrsDataSourceId'
--
-- * 'ddsrsStatus'
data DeleteDataSourceResponse = DeleteDataSourceResponse'
    { _ddsrsDataSourceId :: !(Maybe Text)
    , _ddsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDataSourceResponse' smart constructor.
deleteDataSourceResponse :: Int -> DeleteDataSourceResponse
deleteDataSourceResponse pStatus_ =
    DeleteDataSourceResponse'
    { _ddsrsDataSourceId = Nothing
    , _ddsrsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
ddsrsDataSourceId :: Lens' DeleteDataSourceResponse (Maybe Text)
ddsrsDataSourceId = lens _ddsrsDataSourceId (\ s a -> s{_ddsrsDataSourceId = a});

-- | FIXME: Undocumented member.
ddsrsStatus :: Lens' DeleteDataSourceResponse Int
ddsrsStatus = lens _ddsrsStatus (\ s a -> s{_ddsrsStatus = a});
