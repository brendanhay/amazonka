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

-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all of the Cognito identity pools registered for your account.
module Network.AWS.CognitoIdentity.ListIdentityPools
    (
    -- * Request
      ListIdentityPools
    -- ** Request constructor
    , listIdentityPools
    -- ** Request lenses
    , lipMaxResults
    , lipNextToken

    -- * Response
    , ListIdentityPoolsResponse
    -- ** Response constructor
    , listIdentityPoolsResponse
    -- ** Response lenses
    , liprIdentityPools
    , liprNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoIdentity.Types

data ListIdentityPools = ListIdentityPools
    { _lipMaxResults :: Natural
    , _lipNextToken  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListIdentityPools' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipMaxResults' @::@ 'Natural'
--
-- * 'lipNextToken' @::@ 'Maybe' 'Text'
--
listIdentityPools :: Natural -- ^ 'lipMaxResults'
                  -> ListIdentityPools
listIdentityPools p1 = ListIdentityPools
    { _lipMaxResults = p1
    , _lipNextToken  = Nothing
    }

-- | The maximum number of identities to return.
lipMaxResults :: Lens' ListIdentityPools Natural
lipMaxResults = lens _lipMaxResults (\s a -> s { _lipMaxResults = a })

-- | A pagination token.
lipNextToken :: Lens' ListIdentityPools (Maybe Text)
lipNextToken = lens _lipNextToken (\s a -> s { _lipNextToken = a })

instance ToPath ListIdentityPools where
    toPath = const "/"

instance ToQuery ListIdentityPools where
    toQuery = const mempty

instance ToHeaders ListIdentityPools

instance ToBody ListIdentityPools where
    toBody = toBody . encode . _lipMaxResults

data ListIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools :: [IdentityPoolShortDescription]
    , _liprNextToken     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListIdentityPoolsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liprIdentityPools' @::@ ['IdentityPoolShortDescription']
--
-- * 'liprNextToken' @::@ 'Maybe' 'Text'
--
listIdentityPoolsResponse :: ListIdentityPoolsResponse
listIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools = mempty
    , _liprNextToken     = Nothing
    }

-- | The identity pools returned by the ListIdentityPools action.
liprIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprIdentityPools =
    lens _liprIdentityPools (\s a -> s { _liprIdentityPools = a })

-- | A pagination token.
liprNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprNextToken = lens _liprNextToken (\s a -> s { _liprNextToken = a })

-- FromJSON

instance AWSRequest ListIdentityPools where
    type Sv ListIdentityPools = CognitoIdentity
    type Rs ListIdentityPools = ListIdentityPoolsResponse

    request  = post'
    response = jsonResponse $ \h o -> ListIdentityPoolsResponse
        <$> o .: "IdentityPools"
        <*> o .: "NextToken"
