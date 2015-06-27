{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.ListRecords
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

-- | Gets paginated records, optionally changed after a particular sync count
-- for a dataset and identity. With Amazon Cognito Sync, each identity has
-- access only to its own data. Thus, the credentials used to make this API
-- call need to have access to the identity data.
--
-- ListRecords can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use Cognito
-- Identity credentials to make this API call.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_ListRecords.html>
module Network.AWS.CognitoSync.ListRecords
    (
    -- * Request
      ListRecords
    -- ** Request constructor
    , listRecords
    -- ** Request lenses
    , lrLastSyncCount
    , lrNextToken
    , lrSyncSessionToken
    , lrMaxResults
    , lrIdentityPoolId
    , lrIdentityId
    , lrDatasetName

    -- * Response
    , ListRecordsResponse
    -- ** Response constructor
    , listRecordsResponse
    -- ** Response lenses
    , lrrDatasetDeletedAfterRequestedSyncCount
    , lrrDatasetExists
    , lrrCount
    , lrrRecords
    , lrrNextToken
    , lrrSyncSessionToken
    , lrrMergedDatasetNames
    , lrrLastModifiedBy
    , lrrDatasetSyncCount
    , lrrStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for a list of records.
--
-- /See:/ 'listRecords' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrLastSyncCount'
--
-- * 'lrNextToken'
--
-- * 'lrSyncSessionToken'
--
-- * 'lrMaxResults'
--
-- * 'lrIdentityPoolId'
--
-- * 'lrIdentityId'
--
-- * 'lrDatasetName'
data ListRecords = ListRecords'
    { _lrLastSyncCount    :: !(Maybe Integer)
    , _lrNextToken        :: !(Maybe Text)
    , _lrSyncSessionToken :: !(Maybe Text)
    , _lrMaxResults       :: !(Maybe Int)
    , _lrIdentityPoolId   :: !Text
    , _lrIdentityId       :: !Text
    , _lrDatasetName      :: !Text
    } deriving (Eq,Read,Show)

-- | 'ListRecords' smart constructor.
listRecords :: Text -> Text -> Text -> ListRecords
listRecords pIdentityPoolId pIdentityId pDatasetName =
    ListRecords'
    { _lrLastSyncCount = Nothing
    , _lrNextToken = Nothing
    , _lrSyncSessionToken = Nothing
    , _lrMaxResults = Nothing
    , _lrIdentityPoolId = pIdentityPoolId
    , _lrIdentityId = pIdentityId
    , _lrDatasetName = pDatasetName
    }

-- | The last server sync count for this record.
lrLastSyncCount :: Lens' ListRecords (Maybe Integer)
lrLastSyncCount = lens _lrLastSyncCount (\ s a -> s{_lrLastSyncCount = a});

-- | A pagination token for obtaining the next page of results.
lrNextToken :: Lens' ListRecords (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a});

-- | A token containing a session ID, identity ID, and expiration.
lrSyncSessionToken :: Lens' ListRecords (Maybe Text)
lrSyncSessionToken = lens _lrSyncSessionToken (\ s a -> s{_lrSyncSessionToken = a});

-- | The maximum number of results to be returned.
lrMaxResults :: Lens' ListRecords (Maybe Int)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrIdentityPoolId :: Lens' ListRecords Text
lrIdentityPoolId = lens _lrIdentityPoolId (\ s a -> s{_lrIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrIdentityId :: Lens' ListRecords Text
lrIdentityId = lens _lrIdentityId (\ s a -> s{_lrIdentityId = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
lrDatasetName :: Lens' ListRecords Text
lrDatasetName = lens _lrDatasetName (\ s a -> s{_lrDatasetName = a});

instance AWSRequest ListRecords where
        type Sv ListRecords = CognitoSync
        type Rs ListRecords = ListRecordsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListRecordsResponse' <$>
                   (x .?> "DatasetDeletedAfterRequestedSyncCount") <*>
                     (x .?> "DatasetExists")
                     <*> (x .?> "Count")
                     <*> (x .?> "Records" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (x .?> "SyncSessionToken")
                     <*> (x .?> "MergedDatasetNames" .!@ mempty)
                     <*> (x .?> "LastModifiedBy")
                     <*> (x .?> "DatasetSyncCount")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListRecords where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRecords where
        toPath ListRecords'{..}
          = mconcat
              ["/identitypools/", toText _lrIdentityPoolId,
               "/identities/", toText _lrIdentityId, "/datasets/",
               toText _lrDatasetName, "/records"]

instance ToQuery ListRecords where
        toQuery ListRecords'{..}
          = mconcat
              ["lastSyncCount" =: _lrLastSyncCount,
               "nextToken" =: _lrNextToken,
               "syncSessionToken" =: _lrSyncSessionToken,
               "maxResults" =: _lrMaxResults]

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'listRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrDatasetDeletedAfterRequestedSyncCount'
--
-- * 'lrrDatasetExists'
--
-- * 'lrrCount'
--
-- * 'lrrRecords'
--
-- * 'lrrNextToken'
--
-- * 'lrrSyncSessionToken'
--
-- * 'lrrMergedDatasetNames'
--
-- * 'lrrLastModifiedBy'
--
-- * 'lrrDatasetSyncCount'
--
-- * 'lrrStatus'
data ListRecordsResponse = ListRecordsResponse'
    { _lrrDatasetDeletedAfterRequestedSyncCount :: !(Maybe Bool)
    , _lrrDatasetExists                         :: !(Maybe Bool)
    , _lrrCount                                 :: !(Maybe Int)
    , _lrrRecords                               :: !(Maybe [Record])
    , _lrrNextToken                             :: !(Maybe Text)
    , _lrrSyncSessionToken                      :: !(Maybe Text)
    , _lrrMergedDatasetNames                    :: !(Maybe [Text])
    , _lrrLastModifiedBy                        :: !(Maybe Text)
    , _lrrDatasetSyncCount                      :: !(Maybe Integer)
    , _lrrStatus                                :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListRecordsResponse' smart constructor.
listRecordsResponse :: Int -> ListRecordsResponse
listRecordsResponse pStatus =
    ListRecordsResponse'
    { _lrrDatasetDeletedAfterRequestedSyncCount = Nothing
    , _lrrDatasetExists = Nothing
    , _lrrCount = Nothing
    , _lrrRecords = Nothing
    , _lrrNextToken = Nothing
    , _lrrSyncSessionToken = Nothing
    , _lrrMergedDatasetNames = Nothing
    , _lrrLastModifiedBy = Nothing
    , _lrrDatasetSyncCount = Nothing
    , _lrrStatus = pStatus
    }

-- | A boolean value specifying whether to delete the dataset locally.
lrrDatasetDeletedAfterRequestedSyncCount :: Lens' ListRecordsResponse (Maybe Bool)
lrrDatasetDeletedAfterRequestedSyncCount = lens _lrrDatasetDeletedAfterRequestedSyncCount (\ s a -> s{_lrrDatasetDeletedAfterRequestedSyncCount = a});

-- | Indicates whether the dataset exists.
lrrDatasetExists :: Lens' ListRecordsResponse (Maybe Bool)
lrrDatasetExists = lens _lrrDatasetExists (\ s a -> s{_lrrDatasetExists = a});

-- | Total number of records.
lrrCount :: Lens' ListRecordsResponse (Maybe Int)
lrrCount = lens _lrrCount (\ s a -> s{_lrrCount = a});

-- | A list of all records.
lrrRecords :: Lens' ListRecordsResponse [Record]
lrrRecords = lens _lrrRecords (\ s a -> s{_lrrRecords = a}) . _Default;

-- | A pagination token for obtaining the next page of results.
lrrNextToken :: Lens' ListRecordsResponse (Maybe Text)
lrrNextToken = lens _lrrNextToken (\ s a -> s{_lrrNextToken = a});

-- | A token containing a session ID, identity ID, and expiration.
lrrSyncSessionToken :: Lens' ListRecordsResponse (Maybe Text)
lrrSyncSessionToken = lens _lrrSyncSessionToken (\ s a -> s{_lrrSyncSessionToken = a});

-- | Names of merged datasets.
lrrMergedDatasetNames :: Lens' ListRecordsResponse [Text]
lrrMergedDatasetNames = lens _lrrMergedDatasetNames (\ s a -> s{_lrrMergedDatasetNames = a}) . _Default;

-- | The user\/device that made the last change to this record.
lrrLastModifiedBy :: Lens' ListRecordsResponse (Maybe Text)
lrrLastModifiedBy = lens _lrrLastModifiedBy (\ s a -> s{_lrrLastModifiedBy = a});

-- | Server sync count for this dataset.
lrrDatasetSyncCount :: Lens' ListRecordsResponse (Maybe Integer)
lrrDatasetSyncCount = lens _lrrDatasetSyncCount (\ s a -> s{_lrrDatasetSyncCount = a});

-- | FIXME: Undocumented member.
lrrStatus :: Lens' ListRecordsResponse Int
lrrStatus = lens _lrrStatus (\ s a -> s{_lrrStatus = a});
