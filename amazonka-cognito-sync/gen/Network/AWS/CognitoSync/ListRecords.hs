{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListRecords
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets paginated records, optionally changed after a particular sync count
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
    , lrrqLastSyncCount
    , lrrqNextToken
    , lrrqSyncSessionToken
    , lrrqMaxResults
    , lrrqIdentityPoolId
    , lrrqIdentityId
    , lrrqDatasetName

    -- * Response
    , ListRecordsResponse
    -- ** Response constructor
    , listRecordsResponse
    -- ** Response lenses
    , lrrsDatasetDeletedAfterRequestedSyncCount
    , lrrsDatasetExists
    , lrrsCount
    , lrrsRecords
    , lrrsNextToken
    , lrrsSyncSessionToken
    , lrrsMergedDatasetNames
    , lrrsLastModifiedBy
    , lrrsDatasetSyncCount
    , lrrsStatus
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
-- * 'lrrqLastSyncCount'
--
-- * 'lrrqNextToken'
--
-- * 'lrrqSyncSessionToken'
--
-- * 'lrrqMaxResults'
--
-- * 'lrrqIdentityPoolId'
--
-- * 'lrrqIdentityId'
--
-- * 'lrrqDatasetName'
data ListRecords = ListRecords'
    { _lrrqLastSyncCount    :: !(Maybe Integer)
    , _lrrqNextToken        :: !(Maybe Text)
    , _lrrqSyncSessionToken :: !(Maybe Text)
    , _lrrqMaxResults       :: !(Maybe Int)
    , _lrrqIdentityPoolId   :: !Text
    , _lrrqIdentityId       :: !Text
    , _lrrqDatasetName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRecords' smart constructor.
listRecords :: Text -> Text -> Text -> ListRecords
listRecords pIdentityPoolId pIdentityId pDatasetName =
    ListRecords'
    { _lrrqLastSyncCount = Nothing
    , _lrrqNextToken = Nothing
    , _lrrqSyncSessionToken = Nothing
    , _lrrqMaxResults = Nothing
    , _lrrqIdentityPoolId = pIdentityPoolId
    , _lrrqIdentityId = pIdentityId
    , _lrrqDatasetName = pDatasetName
    }

-- | The last server sync count for this record.
lrrqLastSyncCount :: Lens' ListRecords (Maybe Integer)
lrrqLastSyncCount = lens _lrrqLastSyncCount (\ s a -> s{_lrrqLastSyncCount = a});

-- | A pagination token for obtaining the next page of results.
lrrqNextToken :: Lens' ListRecords (Maybe Text)
lrrqNextToken = lens _lrrqNextToken (\ s a -> s{_lrrqNextToken = a});

-- | A token containing a session ID, identity ID, and expiration.
lrrqSyncSessionToken :: Lens' ListRecords (Maybe Text)
lrrqSyncSessionToken = lens _lrrqSyncSessionToken (\ s a -> s{_lrrqSyncSessionToken = a});

-- | The maximum number of results to be returned.
lrrqMaxResults :: Lens' ListRecords (Maybe Int)
lrrqMaxResults = lens _lrrqMaxResults (\ s a -> s{_lrrqMaxResults = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrrqIdentityPoolId :: Lens' ListRecords Text
lrrqIdentityPoolId = lens _lrrqIdentityPoolId (\ s a -> s{_lrrqIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrrqIdentityId :: Lens' ListRecords Text
lrrqIdentityId = lens _lrrqIdentityId (\ s a -> s{_lrrqIdentityId = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
lrrqDatasetName :: Lens' ListRecords Text
lrrqDatasetName = lens _lrrqDatasetName (\ s a -> s{_lrrqDatasetName = a});

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
              ["/identitypools/", toText _lrrqIdentityPoolId,
               "/identities/", toText _lrrqIdentityId, "/datasets/",
               toText _lrrqDatasetName, "/records"]

instance ToQuery ListRecords where
        toQuery ListRecords'{..}
          = mconcat
              ["lastSyncCount" =: _lrrqLastSyncCount,
               "nextToken" =: _lrrqNextToken,
               "syncSessionToken" =: _lrrqSyncSessionToken,
               "maxResults" =: _lrrqMaxResults]

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'listRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrsDatasetDeletedAfterRequestedSyncCount'
--
-- * 'lrrsDatasetExists'
--
-- * 'lrrsCount'
--
-- * 'lrrsRecords'
--
-- * 'lrrsNextToken'
--
-- * 'lrrsSyncSessionToken'
--
-- * 'lrrsMergedDatasetNames'
--
-- * 'lrrsLastModifiedBy'
--
-- * 'lrrsDatasetSyncCount'
--
-- * 'lrrsStatus'
data ListRecordsResponse = ListRecordsResponse'
    { _lrrsDatasetDeletedAfterRequestedSyncCount :: !(Maybe Bool)
    , _lrrsDatasetExists                         :: !(Maybe Bool)
    , _lrrsCount                                 :: !(Maybe Int)
    , _lrrsRecords                               :: !(Maybe [Record])
    , _lrrsNextToken                             :: !(Maybe Text)
    , _lrrsSyncSessionToken                      :: !(Maybe Text)
    , _lrrsMergedDatasetNames                    :: !(Maybe [Text])
    , _lrrsLastModifiedBy                        :: !(Maybe Text)
    , _lrrsDatasetSyncCount                      :: !(Maybe Integer)
    , _lrrsStatus                                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRecordsResponse' smart constructor.
listRecordsResponse :: Int -> ListRecordsResponse
listRecordsResponse pStatus =
    ListRecordsResponse'
    { _lrrsDatasetDeletedAfterRequestedSyncCount = Nothing
    , _lrrsDatasetExists = Nothing
    , _lrrsCount = Nothing
    , _lrrsRecords = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsSyncSessionToken = Nothing
    , _lrrsMergedDatasetNames = Nothing
    , _lrrsLastModifiedBy = Nothing
    , _lrrsDatasetSyncCount = Nothing
    , _lrrsStatus = pStatus
    }

-- | A boolean value specifying whether to delete the dataset locally.
lrrsDatasetDeletedAfterRequestedSyncCount :: Lens' ListRecordsResponse (Maybe Bool)
lrrsDatasetDeletedAfterRequestedSyncCount = lens _lrrsDatasetDeletedAfterRequestedSyncCount (\ s a -> s{_lrrsDatasetDeletedAfterRequestedSyncCount = a});

-- | Indicates whether the dataset exists.
lrrsDatasetExists :: Lens' ListRecordsResponse (Maybe Bool)
lrrsDatasetExists = lens _lrrsDatasetExists (\ s a -> s{_lrrsDatasetExists = a});

-- | Total number of records.
lrrsCount :: Lens' ListRecordsResponse (Maybe Int)
lrrsCount = lens _lrrsCount (\ s a -> s{_lrrsCount = a});

-- | A list of all records.
lrrsRecords :: Lens' ListRecordsResponse [Record]
lrrsRecords = lens _lrrsRecords (\ s a -> s{_lrrsRecords = a}) . _Default;

-- | A pagination token for obtaining the next page of results.
lrrsNextToken :: Lens' ListRecordsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | A token containing a session ID, identity ID, and expiration.
lrrsSyncSessionToken :: Lens' ListRecordsResponse (Maybe Text)
lrrsSyncSessionToken = lens _lrrsSyncSessionToken (\ s a -> s{_lrrsSyncSessionToken = a});

-- | Names of merged datasets.
lrrsMergedDatasetNames :: Lens' ListRecordsResponse [Text]
lrrsMergedDatasetNames = lens _lrrsMergedDatasetNames (\ s a -> s{_lrrsMergedDatasetNames = a}) . _Default;

-- | The user\/device that made the last change to this record.
lrrsLastModifiedBy :: Lens' ListRecordsResponse (Maybe Text)
lrrsLastModifiedBy = lens _lrrsLastModifiedBy (\ s a -> s{_lrrsLastModifiedBy = a});

-- | Server sync count for this dataset.
lrrsDatasetSyncCount :: Lens' ListRecordsResponse (Maybe Integer)
lrrsDatasetSyncCount = lens _lrrsDatasetSyncCount (\ s a -> s{_lrrsDatasetSyncCount = a});

-- | FIXME: Undocumented member.
lrrsStatus :: Lens' ListRecordsResponse Int
lrrsStatus = lens _lrrsStatus (\ s a -> s{_lrrsStatus = a});
