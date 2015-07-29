{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specific dataset. The dataset will be deleted permanently,
-- and the action can\'t be undone. Datasets that this dataset was merged
-- with will no longer report the merge. Any subsequent operation on this
-- dataset will result in a ResourceNotFoundException.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DeleteDataset.html>
module Network.AWS.CognitoSync.DeleteDataset
    (
    -- * Request
      DeleteDataset
    -- ** Request constructor
    , deleteDataset
    -- ** Request lenses
    , delIdentityPoolId
    , delIdentityId
    , delDatasetName

    -- * Response
    , DeleteDatasetResponse
    -- ** Response constructor
    , deleteDatasetResponse
    -- ** Response lenses
    , drsDataset
    , drsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to delete the specific dataset.
--
-- /See:/ 'deleteDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delIdentityPoolId'
--
-- * 'delIdentityId'
--
-- * 'delDatasetName'
data DeleteDataset = DeleteDataset'
    { _delIdentityPoolId :: !Text
    , _delIdentityId     :: !Text
    , _delDatasetName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDataset' smart constructor.
deleteDataset :: Text -> Text -> Text -> DeleteDataset
deleteDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ =
    DeleteDataset'
    { _delIdentityPoolId = pIdentityPoolId_
    , _delIdentityId = pIdentityId_
    , _delDatasetName = pDatasetName_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
delIdentityPoolId :: Lens' DeleteDataset Text
delIdentityPoolId = lens _delIdentityPoolId (\ s a -> s{_delIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
delIdentityId :: Lens' DeleteDataset Text
delIdentityId = lens _delIdentityId (\ s a -> s{_delIdentityId = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
delDatasetName :: Lens' DeleteDataset Text
delDatasetName = lens _delDatasetName (\ s a -> s{_delDatasetName = a});

instance AWSRequest DeleteDataset where
        type Sv DeleteDataset = CognitoSync
        type Rs DeleteDataset = DeleteDatasetResponse
        request = delete
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDatasetResponse' <$>
                   (x .?> "Dataset") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDataset where
        toPath DeleteDataset'{..}
          = ["identitypools", toBS _delIdentityPoolId,
             "identities", toBS _delIdentityId, "datasets",
             toBS _delDatasetName]

instance ToQuery DeleteDataset where
        toQuery = const mempty

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'deleteDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsDataset'
--
-- * 'drsStatus'
data DeleteDatasetResponse = DeleteDatasetResponse'
    { _drsDataset :: !(Maybe Dataset)
    , _drsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDatasetResponse' smart constructor.
deleteDatasetResponse :: Int -> DeleteDatasetResponse
deleteDatasetResponse pStatus_ =
    DeleteDatasetResponse'
    { _drsDataset = Nothing
    , _drsStatus = pStatus_
    }

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
drsDataset :: Lens' DeleteDatasetResponse (Maybe Dataset)
drsDataset = lens _drsDataset (\ s a -> s{_drsDataset = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteDatasetResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
