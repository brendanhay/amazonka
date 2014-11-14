{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned by GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link. The OpenId
-- token is valid for 15 minutes.
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Request
      GetOpenIdToken
    -- ** Request constructor
    , getOpenIdToken
    -- ** Request lenses
    , goitIdentityId
    , goitLogins

    -- * Response
    , GetOpenIdTokenResponse
    -- ** Response constructor
    , getOpenIdTokenResponse
    -- ** Response lenses
    , goitrIdentityId
    , goitrToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data GetOpenIdToken = GetOpenIdToken
    { _goitIdentityId :: Text
    , _goitLogins     :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'GetOpenIdToken' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitIdentityId' @::@ 'Text'
--
-- * 'goitLogins' @::@ 'HashMap' 'Text' 'Text'
--
getOpenIdToken :: Text -- ^ 'goitIdentityId'
               -> GetOpenIdToken
getOpenIdToken p1 = GetOpenIdToken
    { _goitIdentityId = p1
    , _goitLogins     = mempty
    }

-- | A unique identifier in the format REGION:GUID.
goitIdentityId :: Lens' GetOpenIdToken Text
goitIdentityId = lens _goitIdentityId (\s a -> s { _goitIdentityId = a })

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
goitLogins :: Lens' GetOpenIdToken (HashMap Text Text)
goitLogins = lens _goitLogins (\s a -> s { _goitLogins = a })
    . _Map

instance ToPath GetOpenIdToken where
    toPath = const "/"

instance ToQuery GetOpenIdToken where
    toQuery = const mempty

instance ToHeaders GetOpenIdToken

instance ToBody GetOpenIdToken where
    toBody = toBody . encode . _goitIdentityId

data GetOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId :: Maybe Text
    , _goitrToken      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetOpenIdTokenResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitrIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'goitrToken' @::@ 'Maybe' 'Text'
--
getOpenIdTokenResponse :: GetOpenIdTokenResponse
getOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId = Nothing
    , _goitrToken      = Nothing
    }

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrIdentityId = lens _goitrIdentityId (\s a -> s { _goitrIdentityId = a })

-- | An OpenID token, valid for 15 minutes.
goitrToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrToken = lens _goitrToken (\s a -> s { _goitrToken = a })

instance AWSRequest GetOpenIdToken where
    type Sv GetOpenIdToken = CognitoIdentity
    type Rs GetOpenIdToken = GetOpenIdTokenResponse

    request  = post
    response = jsonResponse $ \h o -> GetOpenIdTokenResponse
        <$> o .: "IdentityId"
        <*> o .: "Token"
