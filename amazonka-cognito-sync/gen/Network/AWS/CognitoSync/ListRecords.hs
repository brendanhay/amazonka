{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.ListRecords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets paginated records, optionally changed after a particular sync count
-- for a dataset and identity. The credentials used to make this API call need
-- to have access to the identity data. With Amazon Cognito Sync, each
-- identity has access only to its own data. You should use Amazon Cognito
-- Identity service to retrieve the credentials necessary to make this API
-- call.
module Network.AWS.CognitoSync.ListRecords
    (
    -- * Request
      ListRecords
    -- ** Request constructor
    , listRecords
    -- ** Request lenses
    , lrDatasetName
    , lrIdentityId
    , lrIdentityPoolId
    , lrLastSyncCount
    , lrMaxResults
    , lrNextToken
    , lrSyncSessionToken

    -- * Response
    , ListRecordsResponse
    -- ** Response constructor
    , listRecordsResponse
    -- ** Response lenses
    , lrrCount
    , lrrDatasetDeletedAfterRequestedSyncCount
    , lrrDatasetExists
    , lrrDatasetSyncCount
    , lrrLastModifiedBy
    , lrrMergedDatasetNames
    , lrrNextToken
    , lrrRecords
    , lrrSyncSessionToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data ListRecords = ListRecords
    { _lrDatasetName      :: Text
    , _lrIdentityId       :: Text
    , _lrIdentityPoolId   :: Text
    , _lrLastSyncCount    :: Maybe Integer
    , _lrMaxResults       :: Maybe Int
    , _lrNextToken        :: Maybe Text
    , _lrSyncSessionToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListRecords' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrDatasetName' @::@ 'Text'
--
-- * 'lrIdentityId' @::@ 'Text'
--
-- * 'lrIdentityPoolId' @::@ 'Text'
--
-- * 'lrLastSyncCount' @::@ 'Maybe' 'Integer'
--
-- * 'lrMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lrSyncSessionToken' @::@ 'Maybe' 'Text'
--
listRecords :: Text -- ^ 'lrIdentityPoolId'
            -> Text -- ^ 'lrIdentityId'
            -> Text -- ^ 'lrDatasetName'
            -> ListRecords
listRecords p1 p2 p3 = ListRecords
    { _lrIdentityPoolId   = p1
    , _lrIdentityId       = p2
    , _lrDatasetName      = p3
    , _lrLastSyncCount    = Nothing
    , _lrNextToken        = Nothing
    , _lrMaxResults       = Nothing
    , _lrSyncSessionToken = Nothing
    }

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- '_' (underscore), '-' (dash), and '.' (dot).
lrDatasetName :: Lens' ListRecords Text
lrDatasetName = lens _lrDatasetName (\s a -> s { _lrDatasetName = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrIdentityId :: Lens' ListRecords Text
lrIdentityId = lens _lrIdentityId (\s a -> s { _lrIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
lrIdentityPoolId :: Lens' ListRecords Text
lrIdentityPoolId = lens _lrIdentityPoolId (\s a -> s { _lrIdentityPoolId = a })

-- | The last server sync count for this record.
lrLastSyncCount :: Lens' ListRecords (Maybe Integer)
lrLastSyncCount = lens _lrLastSyncCount (\s a -> s { _lrLastSyncCount = a })

-- | The maximum number of results to be returned.
lrMaxResults :: Lens' ListRecords (Maybe Int)
lrMaxResults = lens _lrMaxResults (\s a -> s { _lrMaxResults = a })

-- | A pagination token for obtaining the next page of results.
lrNextToken :: Lens' ListRecords (Maybe Text)
lrNextToken = lens _lrNextToken (\s a -> s { _lrNextToken = a })

-- | A token containing a session ID, identity ID, and expiration.
lrSyncSessionToken :: Lens' ListRecords (Maybe Text)
lrSyncSessionToken =
    lens _lrSyncSessionToken (\s a -> s { _lrSyncSessionToken = a })

data ListRecordsResponse = ListRecordsResponse
    { _lrrCount                                 :: Maybe Int
    , _lrrDatasetDeletedAfterRequestedSyncCount :: Maybe Bool
    , _lrrDatasetExists                         :: Maybe Bool
    , _lrrDatasetSyncCount                      :: Maybe Integer
    , _lrrLastModifiedBy                        :: Maybe Text
    , _lrrMergedDatasetNames                    :: [Text]
    , _lrrNextToken                             :: Maybe Text
    , _lrrRecords                               :: [Record]
    , _lrrSyncSessionToken                      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListRecordsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrCount' @::@ 'Maybe' 'Int'
--
-- * 'lrrDatasetDeletedAfterRequestedSyncCount' @::@ 'Maybe' 'Bool'
--
-- * 'lrrDatasetExists' @::@ 'Maybe' 'Bool'
--
-- * 'lrrDatasetSyncCount' @::@ 'Maybe' 'Integer'
--
-- * 'lrrLastModifiedBy' @::@ 'Maybe' 'Text'
--
-- * 'lrrMergedDatasetNames' @::@ ['Text']
--
-- * 'lrrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lrrRecords' @::@ ['Record']
--
-- * 'lrrSyncSessionToken' @::@ 'Maybe' 'Text'
--
listRecordsResponse :: ListRecordsResponse
listRecordsResponse = ListRecordsResponse
    { _lrrRecords                               = mempty
    , _lrrNextToken                             = Nothing
    , _lrrCount                                 = Nothing
    , _lrrDatasetSyncCount                      = Nothing
    , _lrrLastModifiedBy                        = Nothing
    , _lrrMergedDatasetNames                    = mempty
    , _lrrDatasetExists                         = Nothing
    , _lrrDatasetDeletedAfterRequestedSyncCount = Nothing
    , _lrrSyncSessionToken                      = Nothing
    }

-- | Total number of records.
lrrCount :: Lens' ListRecordsResponse (Maybe Int)
lrrCount = lens _lrrCount (\s a -> s { _lrrCount = a })

-- | A boolean value specifying whether to delete the dataset locally.
lrrDatasetDeletedAfterRequestedSyncCount :: Lens' ListRecordsResponse (Maybe Bool)
lrrDatasetDeletedAfterRequestedSyncCount =
    lens _lrrDatasetDeletedAfterRequestedSyncCount
        (\s a -> s { _lrrDatasetDeletedAfterRequestedSyncCount = a })

-- | Indicates whether the dataset exists.
lrrDatasetExists :: Lens' ListRecordsResponse (Maybe Bool)
lrrDatasetExists = lens _lrrDatasetExists (\s a -> s { _lrrDatasetExists = a })

-- | Server sync count for this dataset.
lrrDatasetSyncCount :: Lens' ListRecordsResponse (Maybe Integer)
lrrDatasetSyncCount =
    lens _lrrDatasetSyncCount (\s a -> s { _lrrDatasetSyncCount = a })

-- | The user/device that made the last change to this record.
lrrLastModifiedBy :: Lens' ListRecordsResponse (Maybe Text)
lrrLastModifiedBy =
    lens _lrrLastModifiedBy (\s a -> s { _lrrLastModifiedBy = a })

-- | Names of merged datasets.
lrrMergedDatasetNames :: Lens' ListRecordsResponse [Text]
lrrMergedDatasetNames =
    lens _lrrMergedDatasetNames (\s a -> s { _lrrMergedDatasetNames = a })

-- | A pagination token for obtaining the next page of results.
lrrNextToken :: Lens' ListRecordsResponse (Maybe Text)
lrrNextToken = lens _lrrNextToken (\s a -> s { _lrrNextToken = a })

-- | A list of all records.
lrrRecords :: Lens' ListRecordsResponse [Record]
lrrRecords = lens _lrrRecords (\s a -> s { _lrrRecords = a })

-- | A token containing a session ID, identity ID, and expiration.
lrrSyncSessionToken :: Lens' ListRecordsResponse (Maybe Text)
lrrSyncSessionToken =
    lens _lrrSyncSessionToken (\s a -> s { _lrrSyncSessionToken = a })

instance AWSRequest ListRecords where
    type Sv ListRecords = CognitoSync
    type Rs ListRecords = ListRecordsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListRecordsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath ListRecords where
    toPath ListRecords{..} = mconcat
        [ "/identitypools/"
        , toText _lrIdentityPoolId
        , "/identities/"
        , toText _lrIdentityId
        , "/datasets/"
        , toText _lrDatasetName
        , "/records"
        ]

instance ToHeaders ListRecords

instance ToQuery ListRecords where
    toQuery ListRecords{..} = mconcat
        [ "lastSyncCount"    =? _lrLastSyncCount
        , "nextToken"        =? _lrNextToken
        , "maxResults"       =? _lrMaxResults
        , "syncSessionToken" =? _lrSyncSessionToken
        ]

instance ToJSON ListRecords where
    toJSON = genericToJSON jsonOptions
