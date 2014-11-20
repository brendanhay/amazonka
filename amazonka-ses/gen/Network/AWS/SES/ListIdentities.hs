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

-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListIdentities.html>
module Network.AWS.SES.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liIdentityType
    , liMaxItems
    , liNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirIdentities
    , lirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data ListIdentities = ListIdentities
    { _liIdentityType :: Maybe Text
    , _liMaxItems     :: Maybe Int
    , _liNextToken    :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListIdentities' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liIdentityType' @::@ 'Maybe' 'Text'
--
-- * 'liMaxItems' @::@ 'Maybe' 'Int'
--
-- * 'liNextToken' @::@ 'Maybe' 'Text'
--
listIdentities :: ListIdentities
listIdentities = ListIdentities
    { _liIdentityType = Nothing
    , _liNextToken    = Nothing
    , _liMaxItems     = Nothing
    }

-- | The type of the identities to list. Possible values are "EmailAddress"
-- and "Domain". If this parameter is omitted, then all identities will be
-- listed.
liIdentityType :: Lens' ListIdentities (Maybe Text)
liIdentityType = lens _liIdentityType (\s a -> s { _liIdentityType = a })

-- | The maximum number of identities per page. Possible values are 1-100
-- inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Int)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities :: List "Identities" Text
    , _lirNextToken  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListIdentitiesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirIdentities' @::@ ['Text']
--
-- * 'lirNextToken' @::@ 'Maybe' 'Text'
--
listIdentitiesResponse :: ListIdentitiesResponse
listIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities = mempty
    , _lirNextToken  = Nothing
    }

-- | A list of identities.
lirIdentities :: Lens' ListIdentitiesResponse [Text]
lirIdentities = lens _lirIdentities (\s a -> s { _lirIdentities = a }) . _List

-- | The token used for pagination.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\s a -> s { _lirNextToken = a })

instance ToPath ListIdentities where
    toPath = const "/"

instance ToQuery ListIdentities where
    toQuery ListIdentities{..} = mconcat
        [ "IdentityType" =? _liIdentityType
        , "MaxItems"     =? _liMaxItems
        , "NextToken"    =? _liNextToken
        ]

instance ToHeaders ListIdentities

query

instance AWSRequest ListIdentities where
    type Sv ListIdentities = SES
    type Rs ListIdentities = ListIdentitiesResponse

    request  = post "ListIdentities"
    response = xmlResponse

instance FromXML ListIdentitiesResponse where
    parseXML = withElement "ListIdentitiesResult" $ \x -> ListIdentitiesResponse
        <$> x .@  "Identities"
        <*> x .@? "NextToken"

instance AWSPager ListIdentities where
    page rq rs
        | stop (rq ^. liNextToken) = Nothing
        | otherwise = (\x -> rq & liNextToken ?~ x)
            <$> (rs ^. lirNextToken)
