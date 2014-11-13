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

-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the identities in a pool.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoIdentity.Types

data ListIdentities = ListIdentities
    { _liIdentityPoolId :: Text
    , _liMaxResults     :: Natural
    , _liNextToken      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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
    , _liMaxResults     = p2
    , _liNextToken      = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId = lens _liIdentityPoolId (\s a -> s { _liIdentityPoolId = a })

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Natural
liMaxResults = lens _liMaxResults (\s a -> s { _liMaxResults = a })

-- | A pagination token.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })

instance ToPath ListIdentities where
    toPath = const "/"

instance ToQuery ListIdentities where
    toQuery = const mempty

instance ToHeaders ListIdentities

instance ToBody ListIdentities where
    toBody = toBody . encode . _liIdentityPoolId

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities     :: [IdentityDescription]
    , _lirIdentityPoolId :: Maybe Text
    , _lirNextToken      :: Maybe Text
    } deriving (Eq, Show, Generic)

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
lirIdentities = lens _lirIdentities (\s a -> s { _lirIdentities = a })

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirIdentityPoolId =
    lens _lirIdentityPoolId (\s a -> s { _lirIdentityPoolId = a })

-- | A pagination token.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\s a -> s { _lirNextToken = a })

-- FromJSON

instance AWSRequest ListIdentities where
    type Sv ListIdentities = CognitoIdentity
    type Rs ListIdentities = ListIdentitiesResponse

    request  = post'
    response = jsonResponse $ \h o -> ListIdentitiesResponse
        <$> o .: "Identities"
        <*> o .: "IdentityPoolId"
        <*> o .: "NextToken"
