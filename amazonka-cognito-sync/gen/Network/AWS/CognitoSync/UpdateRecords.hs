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
-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts updates to records and adds and deletes records for a dataset and user.
--
--
-- The sync count in the record patch is your last known sync count for that record. The server will reject an UpdateRecords request with a ResourceConflictException if you try to patch a record with a new value but a stale sync count.
--
-- For example, if the sync count on the server is 5 for a key called highScore and you try and submit a new highScore with sync count of 4, the request will be rejected. To obtain the current sync count for a record, call ListRecords. On a successful update of the record, the response returns the new sync count for that record. You should present that sync count the next time you try to update that same record. When the record does not exist, specify the sync count as 0.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
--
module Network.AWS.CognitoSync.UpdateRecords
    (
    -- * Creating a Request
      updateRecords
    , UpdateRecords
    -- * Request Lenses
    , urRecordPatches
    , urDeviceId
    , urClientContext
    , urIdentityPoolId
    , urIdentityId
    , urDatasetName
    , urSyncSessionToken

    -- * Destructuring the Response
    , updateRecordsResponse
    , UpdateRecordsResponse
    -- * Response Lenses
    , urrsRecords
    , urrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to post updates to records or add and delete records for a dataset and user.
--
-- /See:/ 'updateRecords' smart constructor.
data UpdateRecords = UpdateRecords'
  { _urRecordPatches    :: !(Maybe [RecordPatch])
  , _urDeviceId         :: !(Maybe Text)
  , _urClientContext    :: !(Maybe Text)
  , _urIdentityPoolId   :: !Text
  , _urIdentityId       :: !Text
  , _urDatasetName      :: !Text
  , _urSyncSessionToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRecords' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urRecordPatches' - A list of patch operations.
--
-- * 'urDeviceId' - The unique ID generated for this device by Cognito.
--
-- * 'urClientContext' - Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
--
-- * 'urIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'urIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'urDatasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- * 'urSyncSessionToken' - The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
updateRecords
    :: Text -- ^ 'urIdentityPoolId'
    -> Text -- ^ 'urIdentityId'
    -> Text -- ^ 'urDatasetName'
    -> Text -- ^ 'urSyncSessionToken'
    -> UpdateRecords
updateRecords pIdentityPoolId_ pIdentityId_ pDatasetName_ pSyncSessionToken_ =
  UpdateRecords'
    { _urRecordPatches = Nothing
    , _urDeviceId = Nothing
    , _urClientContext = Nothing
    , _urIdentityPoolId = pIdentityPoolId_
    , _urIdentityId = pIdentityId_
    , _urDatasetName = pDatasetName_
    , _urSyncSessionToken = pSyncSessionToken_
    }


-- | A list of patch operations.
urRecordPatches :: Lens' UpdateRecords [RecordPatch]
urRecordPatches = lens _urRecordPatches (\ s a -> s{_urRecordPatches = a}) . _Default . _Coerce

-- | The unique ID generated for this device by Cognito.
urDeviceId :: Lens' UpdateRecords (Maybe Text)
urDeviceId = lens _urDeviceId (\ s a -> s{_urDeviceId = a})

-- | Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
urClientContext :: Lens' UpdateRecords (Maybe Text)
urClientContext = lens _urClientContext (\ s a -> s{_urClientContext = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
urIdentityPoolId :: Lens' UpdateRecords Text
urIdentityPoolId = lens _urIdentityPoolId (\ s a -> s{_urIdentityPoolId = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
urIdentityId :: Lens' UpdateRecords Text
urIdentityId = lens _urIdentityId (\ s a -> s{_urIdentityId = a})

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
urDatasetName :: Lens' UpdateRecords Text
urDatasetName = lens _urDatasetName (\ s a -> s{_urDatasetName = a})

-- | The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
urSyncSessionToken :: Lens' UpdateRecords Text
urSyncSessionToken = lens _urSyncSessionToken (\ s a -> s{_urSyncSessionToken = a})

instance AWSRequest UpdateRecords where
        type Rs UpdateRecords = UpdateRecordsResponse
        request = postJSON cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRecordsResponse' <$>
                   (x .?> "Records" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable UpdateRecords where

instance NFData UpdateRecords where

instance ToHeaders UpdateRecords where
        toHeaders UpdateRecords'{..}
          = mconcat
              ["x-amz-Client-Context" =# _urClientContext,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON UpdateRecords where
        toJSON UpdateRecords'{..}
          = object
              (catMaybes
                 [("RecordPatches" .=) <$> _urRecordPatches,
                  ("DeviceId" .=) <$> _urDeviceId,
                  Just ("SyncSessionToken" .= _urSyncSessionToken)])

instance ToPath UpdateRecords where
        toPath UpdateRecords'{..}
          = mconcat
              ["/identitypools/", toBS _urIdentityPoolId,
               "/identities/", toBS _urIdentityId, "/datasets/",
               toBS _urDatasetName]

instance ToQuery UpdateRecords where
        toQuery = const mempty

-- | Returned for a successful UpdateRecordsRequest.
--
-- /See:/ 'updateRecordsResponse' smart constructor.
data UpdateRecordsResponse = UpdateRecordsResponse'
  { _urrsRecords        :: !(Maybe [Record])
  , _urrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRecordsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsRecords' - A list of records that have been updated.
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateRecordsResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateRecordsResponse
updateRecordsResponse pResponseStatus_ =
  UpdateRecordsResponse'
    {_urrsRecords = Nothing, _urrsResponseStatus = pResponseStatus_}


-- | A list of records that have been updated.
urrsRecords :: Lens' UpdateRecordsResponse [Record]
urrsRecords = lens _urrsRecords (\ s a -> s{_urrsRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateRecordsResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateRecordsResponse where
