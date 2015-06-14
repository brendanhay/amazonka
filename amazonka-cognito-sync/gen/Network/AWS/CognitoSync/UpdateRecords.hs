{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Posts updates to records and adds and deletes records for a dataset and
-- user.
--
-- UpdateRecords can only be called with temporary user credentials
-- provided by Cognito Identity. You cannot make this API call with
-- developer credentials.
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
    , urClientContext
    , urIdentityPoolId
    , urIdentityId
    , urDatasetName
    , urSyncSessionToken
    , urDeviceId

    -- * Response
    , UpdateRecordsResponse
    -- ** Response constructor
    , updateRecordsResponse
    -- ** Response lenses
    , urrRecords
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'updateRecords' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urRecordPatches'
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
--
-- * 'urDeviceId'
data UpdateRecords = UpdateRecords'{_urRecordPatches :: [RecordPatch], _urClientContext :: Maybe Text, _urIdentityPoolId :: Text, _urIdentityId :: Text, _urDatasetName :: Text, _urSyncSessionToken :: Text, _urDeviceId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateRecords' smart constructor.
updateRecords :: Text -> Text -> Text -> Text -> Text -> UpdateRecords
updateRecords pIdentityPoolId pIdentityId pDatasetName pSyncSessionToken pDeviceId = UpdateRecords'{_urRecordPatches = mempty, _urClientContext = Nothing, _urIdentityPoolId = pIdentityPoolId, _urIdentityId = pIdentityId, _urDatasetName = pDatasetName, _urSyncSessionToken = pSyncSessionToken, _urDeviceId = pDeviceId};

-- | A list of patch operations.
urRecordPatches :: Lens' UpdateRecords [RecordPatch]
urRecordPatches = lens _urRecordPatches (\ s a -> s{_urRecordPatches = a});

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

-- | The unique ID generated for this device by Cognito.
urDeviceId :: Lens' UpdateRecords Text
urDeviceId = lens _urDeviceId (\ s a -> s{_urDeviceId = a});

instance AWSRequest UpdateRecords where
        type Sv UpdateRecords = CognitoSync
        type Rs UpdateRecords = UpdateRecordsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRecordsResponse' <$>
                   x .?> "Records" .!@ mempty)

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
               "SyncSessionToken" .= _urSyncSessionToken,
               "DeviceId" .= _urDeviceId]

instance ToPath UpdateRecords where
        toPath UpdateRecords'{..}
          = mconcat
              ["/identitypools/", toText _urIdentityPoolId,
               "/identities/", toText _urIdentityId, "/datasets/",
               toText _urDatasetName]

instance ToQuery UpdateRecords where
        toQuery = const mempty

-- | /See:/ 'updateRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urrRecords'
newtype UpdateRecordsResponse = UpdateRecordsResponse'{_urrRecords :: [Record]} deriving (Eq, Read, Show)

-- | 'UpdateRecordsResponse' smart constructor.
updateRecordsResponse :: UpdateRecordsResponse
updateRecordsResponse = UpdateRecordsResponse'{_urrRecords = mempty};

-- | A list of records that have been updated.
urrRecords :: Lens' UpdateRecordsResponse [Record]
urrRecords = lens _urrRecords (\ s a -> s{_urrRecords = a});
