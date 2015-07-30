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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRecords' smart constructor.
listRecords :: Text -> Text -> Text -> ListRecords
listRecords pIdentityPoolId_ pIdentityId_ pDatasetName_ =
    ListRecords'
    { _lrLastSyncCount = Nothing
    , _lrNextToken = Nothing
    , _lrSyncSessionToken = Nothing
    , _lrMaxResults = Nothing
    , _lrIdentityPoolId = pIdentityPoolId_
    , _lrIdentityId = pIdentityId_
    , _lrDatasetName = pDatasetName_
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
              ["/identitypools/", toBS _lrIdentityPoolId,
               "/identities/", toBS _lrIdentityId, "/datasets/",
               toBS _lrDatasetName, "/records"]

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
listRecordsResponse pStatus_ =
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
    , _lrrsStatus = pStatus_
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
lrrsRecords = lens _lrrsRecords (\ s a -> s{_lrrsRecords = a}) . _Default . _Coerce;

-- | A pagination token for obtaining the next page of results.
lrrsNextToken :: Lens' ListRecordsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | A token containing a session ID, identity ID, and expiration.
lrrsSyncSessionToken :: Lens' ListRecordsResponse (Maybe Text)
lrrsSyncSessionToken = lens _lrrsSyncSessionToken (\ s a -> s{_lrrsSyncSessionToken = a});

-- | Names of merged datasets.
lrrsMergedDatasetNames :: Lens' ListRecordsResponse [Text]
lrrsMergedDatasetNames = lens _lrrsMergedDatasetNames (\ s a -> s{_lrrsMergedDatasetNames = a}) . _Default . _Coerce;

-- | The user\/device that made the last change to this record.
lrrsLastModifiedBy :: Lens' ListRecordsResponse (Maybe Text)
lrrsLastModifiedBy = lens _lrrsLastModifiedBy (\ s a -> s{_lrrsLastModifiedBy = a});

-- | Server sync count for this dataset.
lrrsDatasetSyncCount :: Lens' ListRecordsResponse (Maybe Integer)
lrrsDatasetSyncCount = lens _lrrsDatasetSyncCount (\ s a -> s{_lrrsDatasetSyncCount = a});

-- | FIXME: Undocumented member.
lrrsStatus :: Lens' ListRecordsResponse Int
lrrsStatus = lens _lrrsStatus (\ s a -> s{_lrrsStatus = a});
