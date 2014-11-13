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

-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets a list of identity pools registered with Cognito.
module Network.AWS.CognitoSync.ListIdentityPoolUsage
    (
    -- * Request
      ListIdentityPoolUsage
    -- ** Request constructor
    , listIdentityPoolUsage
    -- ** Request lenses
    , lipuMaxResults
    , lipuNextToken

    -- * Response
    , ListIdentityPoolUsageResponse
    -- ** Response constructor
    , listIdentityPoolUsageResponse
    -- ** Response lenses
    , lipurCount
    , lipurIdentityPoolUsages
    , lipurMaxResults
    , lipurNextToken
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoSync.Types

data ListIdentityPoolUsage = ListIdentityPoolUsage
    { _lipuMaxResults :: Maybe Int
    , _lipuNextToken  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListIdentityPoolUsage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipuMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lipuNextToken' @::@ 'Maybe' 'Text'
--
listIdentityPoolUsage :: ListIdentityPoolUsage
listIdentityPoolUsage = ListIdentityPoolUsage
    { _lipuNextToken  = Nothing
    , _lipuMaxResults = Nothing
    }

-- | The maximum number of results to be returned.
lipuMaxResults :: Lens' ListIdentityPoolUsage (Maybe Int)
lipuMaxResults = lens _lipuMaxResults (\s a -> s { _lipuMaxResults = a })

-- | A pagination token for obtaining the next page of results.
lipuNextToken :: Lens' ListIdentityPoolUsage (Maybe Text)
lipuNextToken = lens _lipuNextToken (\s a -> s { _lipuNextToken = a })

instance ToPath ListIdentityPoolUsage where
    toPath = const "/identitypools"

instance ToQuery ListIdentityPoolUsage where
    toQuery ListIdentityPoolUsage{..} = mconcat
        [ "nextToken"  =? _lipuNextToken
        , "maxResults" =? _lipuMaxResults
        ]

instance ToHeaders ListIdentityPoolUsage

data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse
    { _lipurCount              :: Maybe Int
    , _lipurIdentityPoolUsages :: [IdentityPoolUsage]
    , _lipurMaxResults         :: Maybe Int
    , _lipurNextToken          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListIdentityPoolUsageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipurCount' @::@ 'Maybe' 'Int'
--
-- * 'lipurIdentityPoolUsages' @::@ ['IdentityPoolUsage']
--
-- * 'lipurMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lipurNextToken' @::@ 'Maybe' 'Text'
--
listIdentityPoolUsageResponse :: ListIdentityPoolUsageResponse
listIdentityPoolUsageResponse = ListIdentityPoolUsageResponse
    { _lipurIdentityPoolUsages = mempty
    , _lipurMaxResults         = Nothing
    , _lipurCount              = Nothing
    , _lipurNextToken          = Nothing
    }

-- | Total number of identities for the identity pool.
lipurCount :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipurCount = lens _lipurCount (\s a -> s { _lipurCount = a })

-- | Usage information for the identity pools.
lipurIdentityPoolUsages :: Lens' ListIdentityPoolUsageResponse [IdentityPoolUsage]
lipurIdentityPoolUsages =
    lens _lipurIdentityPoolUsages (\s a -> s { _lipurIdentityPoolUsages = a })

-- | The maximum number of results to be returned.
lipurMaxResults :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipurMaxResults = lens _lipurMaxResults (\s a -> s { _lipurMaxResults = a })

-- | A pagination token for obtaining the next page of results.
lipurNextToken :: Lens' ListIdentityPoolUsageResponse (Maybe Text)
lipurNextToken = lens _lipurNextToken (\s a -> s { _lipurNextToken = a })

-- FromJSON

instance AWSRequest ListIdentityPoolUsage where
    type Sv ListIdentityPoolUsage = CognitoSync
    type Rs ListIdentityPoolUsage = ListIdentityPoolUsageResponse

    request  = get'
    response = jsonResponse $ \h o -> ListIdentityPoolUsageResponse
        <$> o .: "Count"
        <*> o .: "IdentityPoolUsages"
        <*> o .: "MaxResults"
        <*> o .: "NextToken"
