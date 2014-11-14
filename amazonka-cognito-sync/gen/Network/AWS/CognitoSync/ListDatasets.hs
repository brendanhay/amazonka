{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists datasets for an identity. The credentials used to make this API call
-- need to have access to the identity data. With Amazon Cognito Sync, each
-- identity has access only to its own data. You should use Amazon Cognito
-- Identity service to retrieve the credentials necessary to make this API
-- call.
module Network.AWS.CognitoSync.ListDatasets
    (
    -- * Request
      ListDatasets
    -- ** Request constructor
    , listDatasets
    -- ** Request lenses
    , ldIdentityId
    , ldIdentityPoolId
    , ldMaxResults
    , ldNextToken

    -- * Response
    , ListDatasetsResponse
    -- ** Response constructor
    , listDatasetsResponse
    -- ** Response lenses
    , ldrCount
    , ldrDatasets
    , ldrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoSync.Types

data ListDatasets = ListDatasets
    { _ldIdentityId     :: Text
    , _ldIdentityPoolId :: Text
    , _ldMaxResults     :: Maybe Int
    , _ldNextToken      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDatasets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldIdentityId' @::@ 'Text'
--
-- * 'ldIdentityPoolId' @::@ 'Text'
--
-- * 'ldMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'ldNextToken' @::@ 'Maybe' 'Text'
--
listDatasets :: Text -- ^ 'ldIdentityPoolId'
             -> Text -- ^ 'ldIdentityId'
             -> ListDatasets
listDatasets p1 p2 = ListDatasets
    { _ldIdentityPoolId = p1
    , _ldIdentityId     = p2
    , _ldNextToken      = Nothing
    , _ldMaxResults     = Nothing
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldIdentityId :: Lens' ListDatasets Text
ldIdentityId = lens _ldIdentityId (\s a -> s { _ldIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldIdentityPoolId :: Lens' ListDatasets Text
ldIdentityPoolId = lens _ldIdentityPoolId (\s a -> s { _ldIdentityPoolId = a })

-- | The maximum number of results to be returned.
ldMaxResults :: Lens' ListDatasets (Maybe Int)
ldMaxResults = lens _ldMaxResults (\s a -> s { _ldMaxResults = a })

-- | A pagination token for obtaining the next page of results.
ldNextToken :: Lens' ListDatasets (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s { _ldNextToken = a })

instance ToPath ListDatasets where
    toPath ListDatasets{..} = mconcat
        [ "/identitypools/"
        , toText _ldIdentityPoolId
        , "/identities/"
        , toText _ldIdentityId
        , "/datasets"
        ]

instance ToQuery ListDatasets where
    toQuery ListDatasets{..} = mconcat
        [ "nextToken"  =? _ldNextToken
        , "maxResults" =? _ldMaxResults
        ]

instance ToHeaders ListDatasets

data ListDatasetsResponse = ListDatasetsResponse
    { _ldrCount     :: Maybe Int
    , _ldrDatasets  :: [Dataset]
    , _ldrNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListDatasetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrCount' @::@ 'Maybe' 'Int'
--
-- * 'ldrDatasets' @::@ ['Dataset']
--
-- * 'ldrNextToken' @::@ 'Maybe' 'Text'
--
listDatasetsResponse :: ListDatasetsResponse
listDatasetsResponse = ListDatasetsResponse
    { _ldrDatasets  = mempty
    , _ldrCount     = Nothing
    , _ldrNextToken = Nothing
    }

-- | Number of datasets returned.
ldrCount :: Lens' ListDatasetsResponse (Maybe Int)
ldrCount = lens _ldrCount (\s a -> s { _ldrCount = a })

-- | A set of datasets.
ldrDatasets :: Lens' ListDatasetsResponse [Dataset]
ldrDatasets = lens _ldrDatasets (\s a -> s { _ldrDatasets = a })

-- | A pagination token for obtaining the next page of results.
ldrNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\s a -> s { _ldrNextToken = a })

instance AWSRequest ListDatasets where
    type Sv ListDatasets = CognitoSync
    type Rs ListDatasets = ListDatasetsResponse

    request  = get
    response = jsonResponse $ \h o -> ListDatasetsResponse
        <$> o .: "Count"
        <*> o .: "Datasets"
        <*> o .: "NextToken"
