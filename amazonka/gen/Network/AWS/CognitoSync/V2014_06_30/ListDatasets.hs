{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.ListDatasets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists datasets for an identity.
module Network.AWS.CognitoSync.V2014_06_30.ListDatasets where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListDatasets' request.
listDatasets :: Text -- ^ '_ldrIdentityId'
             -> ListDatasets
listDatasets p1 = ListDatasets
    { _ldrIdentityId = p1
    , _ldrIdentityPoolId = Nothing
    , _ldrMaxResults = Nothing
    , _ldrNextToken = Nothing
    }

data ListDatasets = ListDatasets
    { _ldrIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _ldrIdentityPoolId :: Maybe Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _ldrMaxResults :: Maybe Integer
      -- ^ The maximum number of results to be returned.
    , _ldrNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    } deriving (Generic)

makeLenses ''ListDatasets

instance ToPath ListDatasets where
    toPath ListDatasets{..} = mconcat
        [ "/identitypools/"
        , toBS _ldrIdentityPoolId
        , "/identities/"
        , toBS _ldrIdentityId
        , "/datasets"
        ]

instance ToQuery ListDatasets where
    toQuery ListDatasets{..} = mconcat
        [ "maxResults" =? _ldrMaxResults
        , "nextToken" =? _ldrNextToken
        ]

instance ToHeaders ListDatasets

instance ToJSON ListDatasets

data ListDatasetsResponse = ListDatasetsResponse
    { _ldsDatasets :: [Dataset]
      -- ^ A set of datasets.
    , _ldsCount :: Maybe Integer
      -- ^ Number of datasets returned.
    , _ldsNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    } deriving (Generic)

makeLenses ''ListDatasetsResponse

instance FromJSON ListDatasetsResponse

instance AWSRequest ListDatasets where
    type Sv ListDatasets = CognitoSync
    type Rs ListDatasets = ListDatasetsResponse

    request = get
    response _ = jsonResponse
