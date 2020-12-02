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
-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specific dataset. The dataset will be deleted permanently, and the action can't be undone. Datasets that this dataset was merged with will no longer report the merge. Any subsequent operation on this dataset will result in a ResourceNotFoundException.
--
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
--
module Network.AWS.CognitoSync.DeleteDataset
    (
    -- * Creating a Request
      deleteDataset
    , DeleteDataset
    -- * Request Lenses
    , delIdentityPoolId
    , delIdentityId
    , delDatasetName

    -- * Destructuring the Response
    , deleteDatasetResponse
    , DeleteDatasetResponse
    -- * Response Lenses
    , drsDataset
    , drsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to delete the specific dataset.
--
-- /See:/ 'deleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { _delIdentityPoolId :: !Text
  , _delIdentityId     :: !Text
  , _delDatasetName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'delIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'delDatasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
deleteDataset
    :: Text -- ^ 'delIdentityPoolId'
    -> Text -- ^ 'delIdentityId'
    -> Text -- ^ 'delDatasetName'
    -> DeleteDataset
deleteDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ =
  DeleteDataset'
    { _delIdentityPoolId = pIdentityPoolId_
    , _delIdentityId = pIdentityId_
    , _delDatasetName = pDatasetName_
    }


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
delIdentityPoolId :: Lens' DeleteDataset Text
delIdentityPoolId = lens _delIdentityPoolId (\ s a -> s{_delIdentityPoolId = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
delIdentityId :: Lens' DeleteDataset Text
delIdentityId = lens _delIdentityId (\ s a -> s{_delIdentityId = a})

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
delDatasetName :: Lens' DeleteDataset Text
delDatasetName = lens _delDatasetName (\ s a -> s{_delDatasetName = a})

instance AWSRequest DeleteDataset where
        type Rs DeleteDataset = DeleteDatasetResponse
        request = delete cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDatasetResponse' <$>
                   (x .?> "Dataset") <*> (pure (fromEnum s)))

instance Hashable DeleteDataset where

instance NFData DeleteDataset where

instance ToHeaders DeleteDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDataset where
        toPath DeleteDataset'{..}
          = mconcat
              ["/identitypools/", toBS _delIdentityPoolId,
               "/identities/", toBS _delIdentityId, "/datasets/",
               toBS _delDatasetName]

instance ToQuery DeleteDataset where
        toQuery = const mempty

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'deleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { _drsDataset        :: !(Maybe Dataset)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDataset' - A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteDatasetResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteDatasetResponse
deleteDatasetResponse pResponseStatus_ =
  DeleteDatasetResponse'
    {_drsDataset = Nothing, _drsResponseStatus = pResponseStatus_}


-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
drsDataset :: Lens' DeleteDatasetResponse (Maybe Dataset)
drsDataset = lens _drsDataset (\ s a -> s{_drsDataset = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteDatasetResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteDatasetResponse where
