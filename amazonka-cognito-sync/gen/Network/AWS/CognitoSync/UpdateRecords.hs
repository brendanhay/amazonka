{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Posts updates to records and adds and deletes records for a dataset and
-- user.
--
-- The sync count in the record patch is your last known sync count for
-- that record. The server will reject an UpdateRecords request with a
-- ResourceConflictException if you try to patch a record with a new value
-- but a stale sync count.
--
-- For example, if the sync count on the server is 5 for a key called
-- highScore and you try and submit a new highScore with sync count of 4,
-- the request will be rejected. To obtain the current sync count for a
-- record, call ListRecords. On a successful update of the record, the
-- response returns the new sync count for that record. You should present
-- that sync count the next time you try to update that same record. When
-- the record does not exist, specify the sync count as 0.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_UpdateRecords.html>
module Network.AWS.CognitoSync.UpdateRecords
    (
    -- * Request
      UpdateRecords
    -- ** Request constructor
    , updateRecords
    -- ** Request lenses
    , urRecordPatches
    , urDeviceId
    , urClientContext
    , urIdentityPoolId
    , urIdentityId
    , urDatasetName
    , urSyncSessionToken

    -- * Response
    , UpdateRecordsResponse
    -- ** Response constructor
    , updateRecordsResponse
    -- ** Response lenses
    , urrsRecords
    , urrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to post updates to records or add and delete records for a
-- dataset and user.
--
-- /See:/ 'updateRecords' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urRecordPatches'
--
-- * 'urDeviceId'
--
-- * 'urClientContext'
--
-- * 'urIdentityPoolId'
--
-- * 'urIdentityId'
--
-- * 'urDatasetName'
--
-- * 'urSyncSessionToken'
data UpdateRecords = UpdateRecords'
    { _urRecordPatches    :: !(Maybe [RecordPatch])
    , _urDeviceId         :: !(Maybe Text)
    , _urClientContext    :: !(Maybe Text)
    , _urIdentityPoolId   :: !Text
    , _urIdentityId       :: !Text
    , _urDatasetName      :: !Text
    , _urSyncSessionToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRecords' smart constructor.
updateRecords :: Text -> Text -> Text -> Text -> UpdateRecords
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
urRecordPatches = lens _urRecordPatches (\ s a -> s{_urRecordPatches = a}) . _Default . _Coerce;

-- | The unique ID generated for this device by Cognito.
urDeviceId :: Lens' UpdateRecords (Maybe Text)
urDeviceId = lens _urDeviceId (\ s a -> s{_urDeviceId = a});

-- | Intended to supply a device ID that will populate the lastModifiedBy
-- field referenced in other methods. The ClientContext field is not yet
-- implemented.
urClientContext :: Lens' UpdateRecords (Maybe Text)
urClientContext = lens _urClientContext (\ s a -> s{_urClientContext = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
urIdentityPoolId :: Lens' UpdateRecords Text
urIdentityPoolId = lens _urIdentityPoolId (\ s a -> s{_urIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
urIdentityId :: Lens' UpdateRecords Text
urIdentityId = lens _urIdentityId (\ s a -> s{_urIdentityId = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
urDatasetName :: Lens' UpdateRecords Text
urDatasetName = lens _urDatasetName (\ s a -> s{_urDatasetName = a});

-- | The SyncSessionToken returned by a previous call to ListRecords for this
-- dataset and identity.
urSyncSessionToken :: Lens' UpdateRecords Text
urSyncSessionToken = lens _urSyncSessionToken (\ s a -> s{_urSyncSessionToken = a});

instance AWSRequest UpdateRecords where
        type Sv UpdateRecords = CognitoSync
        type Rs UpdateRecords = UpdateRecordsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRecordsResponse' <$>
                   (x .?> "Records" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders UpdateRecords where
        toHeaders UpdateRecords'{..}
          = mconcat
              ["x-amz-Client-Context" =# _urClientContext,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON UpdateRecords where
        toJSON UpdateRecords'{..}
          = object
              ["RecordPatches" .= _urRecordPatches,
               "DeviceId" .= _urDeviceId,
               "SyncSessionToken" .= _urSyncSessionToken]

instance ToPath UpdateRecords where
        toPath UpdateRecords'{..}
          = mconcat
              ["/identitypools/", toPath _urIdentityPoolId,
               "/identities/", toPath _urIdentityId, "/datasets/",
               toPath _urDatasetName]

instance ToQuery UpdateRecords where
        toQuery = const mempty

-- | Returned for a successful UpdateRecordsRequest.
--
-- /See:/ 'updateRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urrsRecords'
--
-- * 'urrsStatus'
data UpdateRecordsResponse = UpdateRecordsResponse'
    { _urrsRecords :: !(Maybe [Record])
    , _urrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRecordsResponse' smart constructor.
updateRecordsResponse :: Int -> UpdateRecordsResponse
updateRecordsResponse pStatus_ =
    UpdateRecordsResponse'
    { _urrsRecords = Nothing
    , _urrsStatus = pStatus_
    }

-- | A list of records that have been updated.
urrsRecords :: Lens' UpdateRecordsResponse [Record]
urrsRecords = lens _urrsRecords (\ s a -> s{_urrsRecords = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
urrsStatus :: Lens' UpdateRecordsResponse Int
urrsStatus = lens _urrsStatus (\ s a -> s{_urrsStatus = a});
