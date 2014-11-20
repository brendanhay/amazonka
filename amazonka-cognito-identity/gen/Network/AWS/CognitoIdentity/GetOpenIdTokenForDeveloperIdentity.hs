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

-- Module      : Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers (or retrieves) a Cognito IdentityId and an OpenID Connect token
-- for a user authenticated by your backend authentication process. Supplying
-- multiple logins will create an implicit linked account. You can only
-- specify one developer provider as part of the Logins map, which is linked
-- to the identity pool. The developer provider is the "domain" by which
-- Cognito will refer to your users. You can use
-- GetOpenIdTokenForDeveloperIdentity to create a new identity and to link new
-- logins (that is, user credentials issued by a public provider or developer
-- provider) to an existing identity. When you want to create a new identity,
-- the IdentityId should be null. When you want to associate a new login with
-- an existing authenticated/unauthenticated identity, you can do so by
-- providing the existing IdentityId. This API will create the identity in the
-- specified IdentityPoolId.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetOpenIdTokenForDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
    (
    -- * Request
      GetOpenIdTokenForDeveloperIdentity
    -- ** Request constructor
    , getOpenIdTokenForDeveloperIdentity
    -- ** Request lenses
    , goitfdiIdentityId
    , goitfdiIdentityPoolId
    , goitfdiLogins
    , goitfdiTokenDuration

    -- * Response
    , GetOpenIdTokenForDeveloperIdentityResponse
    -- ** Response constructor
    , getOpenIdTokenForDeveloperIdentityResponse
    -- ** Response lenses
    , goitfdirIdentityId
    , goitfdirToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity
    { _goitfdiIdentityId     :: Maybe Text
    , _goitfdiIdentityPoolId :: Text
    , _goitfdiLogins         :: Map Text Text
    , _goitfdiTokenDuration  :: Maybe Nat
    } deriving (Eq, Show)

-- | 'GetOpenIdTokenForDeveloperIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitfdiIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'goitfdiIdentityPoolId' @::@ 'Text'
--
-- * 'goitfdiLogins' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'goitfdiTokenDuration' @::@ 'Maybe' 'Natural'
--
getOpenIdTokenForDeveloperIdentity :: Text -- ^ 'goitfdiIdentityPoolId'
                                   -> GetOpenIdTokenForDeveloperIdentity
getOpenIdTokenForDeveloperIdentity p1 = GetOpenIdTokenForDeveloperIdentity
    { _goitfdiIdentityPoolId = p1
    , _goitfdiIdentityId     = Nothing
    , _goitfdiLogins         = mempty
    , _goitfdiTokenDuration  = Nothing
    }

-- | A unique identifier in the format REGION:GUID.
goitfdiIdentityId :: Lens' GetOpenIdTokenForDeveloperIdentity (Maybe Text)
goitfdiIdentityId =
    lens _goitfdiIdentityId (\s a -> s { _goitfdiIdentityId = a })

-- | An identity pool ID in the format REGION:GUID.
goitfdiIdentityPoolId :: Lens' GetOpenIdTokenForDeveloperIdentity Text
goitfdiIdentityPoolId =
    lens _goitfdiIdentityPoolId (\s a -> s { _goitfdiIdentityPoolId = a })

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. Each name-value pair represents a user from a public provider or
-- developer provider. If the user is from a developer provider, the
-- name-value pair will follow the syntax "developer_provider_name":
-- "developer_user_identifier". The developer provider is the "domain" by
-- which Cognito will refer to your users; you provided this domain while
-- creating/updating the identity pool. The developer user identifier is an
-- identifier from your backend that uniquely identifies a user. When you
-- create an identity pool, you can specify the supported logins.
goitfdiLogins :: Lens' GetOpenIdTokenForDeveloperIdentity (HashMap Text Text)
goitfdiLogins = lens _goitfdiLogins (\s a -> s { _goitfdiLogins = a }) . _Map

-- | The expiration time of the token, in seconds. You can specify a custom
-- expiration time for the token so that you can cache it. If you don't
-- provide an expiration time, the token is valid for 15 minutes. You can
-- exchange the token with Amazon STS for temporary AWS credentials, which
-- are valid for a maximum of one hour. The maximum token duration you can
-- set is 24 hours. You should take care in setting the expiration time for
-- a token, as there are significant security implications: an attacker
-- could use a leaked token to access your AWS resources for the token's
-- duration.
goitfdiTokenDuration :: Lens' GetOpenIdTokenForDeveloperIdentity (Maybe Natural)
goitfdiTokenDuration =
    lens _goitfdiTokenDuration (\s a -> s { _goitfdiTokenDuration = a })
        . mapping _Nat

data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse
    { _goitfdirIdentityId :: Maybe Text
    , _goitfdirToken      :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'GetOpenIdTokenForDeveloperIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitfdirIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'goitfdirToken' @::@ 'Maybe' 'Text'
--
getOpenIdTokenForDeveloperIdentityResponse :: GetOpenIdTokenForDeveloperIdentityResponse
getOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse
    { _goitfdirIdentityId = Nothing
    , _goitfdirToken      = Nothing
    }

-- | A unique identifier in the format REGION:GUID.
goitfdirIdentityId :: Lens' GetOpenIdTokenForDeveloperIdentityResponse (Maybe Text)
goitfdirIdentityId =
    lens _goitfdirIdentityId (\s a -> s { _goitfdirIdentityId = a })

-- | An OpenID token.
goitfdirToken :: Lens' GetOpenIdTokenForDeveloperIdentityResponse (Maybe Text)
goitfdirToken = lens _goitfdirToken (\s a -> s { _goitfdirToken = a })

instance ToPath GetOpenIdTokenForDeveloperIdentity where
    toPath = const "/"

instance ToQuery GetOpenIdTokenForDeveloperIdentity where
    toQuery = const mempty

instance ToHeaders GetOpenIdTokenForDeveloperIdentity

instance ToJSON GetOpenIdTokenForDeveloperIdentity where
    toJSON GetOpenIdTokenForDeveloperIdentity{..} = object
        [ "IdentityPoolId" .= _goitfdiIdentityPoolId
        , "IdentityId"     .= _goitfdiIdentityId
        , "Logins"         .= _goitfdiLogins
        , "TokenDuration"  .= _goitfdiTokenDuration
        ]

instance AWSRequest GetOpenIdTokenForDeveloperIdentity where
    type Sv GetOpenIdTokenForDeveloperIdentity = CognitoIdentity
    type Rs GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentityResponse

    request  = post "GetOpenIdTokenForDeveloperIdentity"
    response = jsonResponse

instance FromJSON GetOpenIdTokenForDeveloperIdentityResponse where
    parseJSON = withObject "GetOpenIdTokenForDeveloperIdentityResponse" $ \o -> GetOpenIdTokenForDeveloperIdentityResponse
        <$> o .:? "IdentityId"
        <*> o .:? "Token"
