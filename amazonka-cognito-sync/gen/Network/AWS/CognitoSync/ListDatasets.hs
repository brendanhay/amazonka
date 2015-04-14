{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists datasets for an identity. With Amazon Cognito Sync, each identity has
-- access only to its own data. Thus, the credentials used to make this API call
-- need to have access to the identity data.
--
-- 'ListDatasets' can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use the Cognito
-- Identity credentials to make this API call.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_ListDatasets.html>
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data ListDatasets = ListDatasets
    { _ldIdentityId     :: Text
    , _ldIdentityPoolId :: Text
    , _ldMaxResults     :: Maybe Int
    , _ldNextToken      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

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
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
ldIdentityId :: Lens' ListDatasets Text
ldIdentityId = lens _ldIdentityId (\s a -> s { _ldIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
ldIdentityPoolId :: Lens' ListDatasets Text
ldIdentityPoolId = lens _ldIdentityPoolId (\s a -> s { _ldIdentityPoolId = a })

-- | The maximum number of results to be returned.
ldMaxResults :: Lens' ListDatasets (Maybe Int)
ldMaxResults = lens _ldMaxResults (\s a -> s { _ldMaxResults = a })

-- | A pagination token for obtaining the next page of results.
ldNextToken :: Lens' ListDatasets (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s { _ldNextToken = a })

data ListDatasetsResponse = ListDatasetsResponse
    { _ldrCount     :: Maybe Int
    , _ldrDatasets  :: List "Datasets" Dataset
    , _ldrNextToken :: Maybe Text
    } deriving (Eq, Read, Show)

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
ldrDatasets = lens _ldrDatasets (\s a -> s { _ldrDatasets = a }) . _List

-- | A pagination token for obtaining the next page of results.
ldrNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\s a -> s { _ldrNextToken = a })

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

instance ToJSON ListDatasets where
    toJSON = const (toJSON Empty)

instance AWSRequest ListDatasets where
    type Sv ListDatasets = CognitoSync
    type Rs ListDatasets = ListDatasetsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListDatasetsResponse where
    parseJSON = withObject "ListDatasetsResponse" $ \o -> ListDatasetsResponse
        <$> o .:? "Count"
        <*> o .:? "Datasets" .!= mempty
        <*> o .:? "NextToken"
