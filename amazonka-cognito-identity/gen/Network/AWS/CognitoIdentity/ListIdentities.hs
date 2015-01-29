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

-- Module      : Network.AWS.CognitoIdentity.ListIdentities
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

-- | Lists the identities in a pool.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_ListIdentities.html>
module Network.AWS.CognitoIdentity.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liIdentityPoolId
    , liMaxResults
    , liNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirIdentities
    , lirIdentityPoolId
    , lirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data ListIdentities = ListIdentities
    { _liIdentityPoolId :: Text
    , _liMaxResults     :: Nat
    , _liNextToken      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListIdentities' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liIdentityPoolId' @::@ 'Text'
--
-- * 'liMaxResults' @::@ 'Natural'
--
-- * 'liNextToken' @::@ 'Maybe' 'Text'
--
listIdentities :: Text -- ^ 'liIdentityPoolId'
               -> Natural -- ^ 'liMaxResults'
               -> ListIdentities
listIdentities p1 p2 = ListIdentities
    { _liIdentityPoolId = p1
    , _liMaxResults     = withIso _Nat (const id) p2
    , _liNextToken      = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId = lens _liIdentityPoolId (\s a -> s { _liIdentityPoolId = a })

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Natural
liMaxResults = lens _liMaxResults (\s a -> s { _liMaxResults = a }) . _Nat

-- | A pagination token.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities     :: List "Identities" IdentityDescription
    , _lirIdentityPoolId :: Maybe Text
    , _lirNextToken      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListIdentitiesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirIdentities' @::@ ['IdentityDescription']
--
-- * 'lirIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'lirNextToken' @::@ 'Maybe' 'Text'
--
listIdentitiesResponse :: ListIdentitiesResponse
listIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentityPoolId = Nothing
    , _lirIdentities     = mempty
    , _lirNextToken      = Nothing
    }

-- | An object containing a set of identities and associated mappings.
lirIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirIdentities = lens _lirIdentities (\s a -> s { _lirIdentities = a }) . _List

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirIdentityPoolId =
    lens _lirIdentityPoolId (\s a -> s { _lirIdentityPoolId = a })

-- | A pagination token.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\s a -> s { _lirNextToken = a })

instance ToPath ListIdentities where
    toPath = const "/"

instance ToQuery ListIdentities where
    toQuery = const mempty

instance ToHeaders ListIdentities

instance ToJSON ListIdentities where
    toJSON ListIdentities{..} = object
        [ "IdentityPoolId" .= _liIdentityPoolId
        , "MaxResults"     .= _liMaxResults
        , "NextToken"      .= _liNextToken
        ]

instance AWSRequest ListIdentities where
    type Sv ListIdentities = CognitoIdentity
    type Rs ListIdentities = ListIdentitiesResponse

    request  = post "ListIdentities"
    response = jsonResponse

instance FromJSON ListIdentitiesResponse where
    parseJSON = withObject "ListIdentitiesResponse" $ \o -> ListIdentitiesResponse
        <$> o .:? "Identities" .!= mempty
        <*> o .:? "IdentityPoolId"
        <*> o .:? "NextToken"
