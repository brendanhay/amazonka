{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.ListRecords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets paginated records, optionally changed after a particular sync count
-- for a dataset and identity.
module Network.AWS.CognitoSync.V2014_06_30.ListRecords where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListRecords' request.
listRecords :: Text -- ^ '_lrrDatasetName'
            -> Text -- ^ '_lrrIdentityId'
            -> Text -- ^ '_lrrIdentityPoolId'
            -> ListRecords
listRecords p1 p2 p3 = ListRecords
    { _lrrDatasetName = p1
    , _lrrIdentityId = p2
    , _lrrIdentityPoolId = p3
    , _lrrLastSyncCount = Nothing
    , _lrrMaxResults = Nothing
    , _lrrNextToken = Nothing
    , _lrrSyncSessionToken = Nothing
    }

data ListRecords = ListRecords
    { _lrrDatasetName :: Text
      -- ^ A string of up to 128 characters. Allowed characters are a-z,
      -- A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    , _lrrIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _lrrIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _lrrLastSyncCount :: Maybe Integer
      -- ^ The last server sync count for this record.
    , _lrrMaxResults :: Maybe Integer
      -- ^ The maximum number of results to be returned.
    , _lrrNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    , _lrrSyncSessionToken :: Maybe Text
      -- ^ A token containing a session ID, identity ID, and expiration.
    } deriving (Generic)

makeLenses ''ListRecords

instance ToPath ListRecords where
    toPath ListRecords{..} = mconcat
        [ "/identitypools/"
        , toBS _lrrIdentityPoolId
        , "/identities/"
        , toBS _lrrIdentityId
        , "/datasets/"
        , toBS _lrrDatasetName
        , "/records"
        ]

instance ToQuery ListRecords where
    toQuery ListRecords{..} = mconcat
        [ "lastSyncCount" =? _lrrLastSyncCount
        , "maxResults" =? _lrrMaxResults
        , "nextToken" =? _lrrNextToken
        , "syncSessionToken" =? _lrrSyncSessionToken
        ]

instance ToHeaders ListRecords

instance ToJSON ListRecords

data ListRecordsResponse = ListRecordsResponse
    { _lrsDatasetDeletedAfterRequestedSyncCount :: Maybe Bool
      -- ^ A boolean value specifying whether to delete the dataset locally.
    , _lrsDatasetExists :: Maybe Bool
      -- ^ Indicates whether the dataset exists.
    , _lrsCount :: Maybe Integer
      -- ^ Total number of records.
    , _lrsDatasetSyncCount :: Maybe Integer
      -- ^ Server sync count for this dataset.
    , _lrsMergedDatasetNames :: [Text]
      -- ^ Names of merged datasets.
    , _lrsRecords :: [Record]
      -- ^ A list of all records.
    , _lrsNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    , _lrsSyncSessionToken :: Maybe Text
      -- ^ A token containing a session ID, identity ID, and expiration.
    , _lrsLastModifiedBy :: Maybe Text
    } deriving (Generic)

makeLenses ''ListRecordsResponse

instance FromJSON ListRecordsResponse

instance AWSRequest ListRecords where
    type Sv ListRecords = CognitoSync
    type Rs ListRecords = ListRecordsResponse

    request = get
    response _ = jsonResponse
