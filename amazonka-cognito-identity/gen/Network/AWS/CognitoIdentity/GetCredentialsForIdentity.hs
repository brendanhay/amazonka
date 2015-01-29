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

-- Module      : Network.AWS.CognitoIdentity.GetCredentialsForIdentity
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

-- | Returns credentials for the the provided identity ID. Any provided logins
-- will be validated against supported login providers. If the token is for
-- cognito-identity.amazonaws.com, it will be passed through to AWS Security
-- Token Service with the appropriate role for the token.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html>
module Network.AWS.CognitoIdentity.GetCredentialsForIdentity
    (
    -- * Request
      GetCredentialsForIdentity
    -- ** Request constructor
    , getCredentialsForIdentity
    -- ** Request lenses
    , gcfiIdentityId
    , gcfiLogins

    -- * Response
    , GetCredentialsForIdentityResponse
    -- ** Response constructor
    , getCredentialsForIdentityResponse
    -- ** Response lenses
    , gcfirCredentials
    , gcfirIdentityId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data GetCredentialsForIdentity = GetCredentialsForIdentity
    { _gcfiIdentityId :: Text
    , _gcfiLogins     :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'GetCredentialsForIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfiIdentityId' @::@ 'Text'
--
-- * 'gcfiLogins' @::@ 'HashMap' 'Text' 'Text'
--
getCredentialsForIdentity :: Text -- ^ 'gcfiIdentityId'
                          -> GetCredentialsForIdentity
getCredentialsForIdentity p1 = GetCredentialsForIdentity
    { _gcfiIdentityId = p1
    , _gcfiLogins     = mempty
    }

-- | A unique identifier in the format REGION:GUID.
gcfiIdentityId :: Lens' GetCredentialsForIdentity Text
gcfiIdentityId = lens _gcfiIdentityId (\s a -> s { _gcfiIdentityId = a })

-- | A set of optional name-value pairs that map provider names to provider tokens.
gcfiLogins :: Lens' GetCredentialsForIdentity (HashMap Text Text)
gcfiLogins = lens _gcfiLogins (\s a -> s { _gcfiLogins = a }) . _Map

data GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse
    { _gcfirCredentials :: Maybe Credentials
    , _gcfirIdentityId  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GetCredentialsForIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfirCredentials' @::@ 'Maybe' 'Credentials'
--
-- * 'gcfirIdentityId' @::@ 'Maybe' 'Text'
--
getCredentialsForIdentityResponse :: GetCredentialsForIdentityResponse
getCredentialsForIdentityResponse = GetCredentialsForIdentityResponse
    { _gcfirIdentityId  = Nothing
    , _gcfirCredentials = Nothing
    }

-- | Credentials for the the provided identity ID.
gcfirCredentials :: Lens' GetCredentialsForIdentityResponse (Maybe Credentials)
gcfirCredentials = lens _gcfirCredentials (\s a -> s { _gcfirCredentials = a })

-- | A unique identifier in the format REGION:GUID.
gcfirIdentityId :: Lens' GetCredentialsForIdentityResponse (Maybe Text)
gcfirIdentityId = lens _gcfirIdentityId (\s a -> s { _gcfirIdentityId = a })

instance ToPath GetCredentialsForIdentity where
    toPath = const "/"

instance ToQuery GetCredentialsForIdentity where
    toQuery = const mempty

instance ToHeaders GetCredentialsForIdentity

instance ToJSON GetCredentialsForIdentity where
    toJSON GetCredentialsForIdentity{..} = object
        [ "IdentityId" .= _gcfiIdentityId
        , "Logins"     .= _gcfiLogins
        ]

instance AWSRequest GetCredentialsForIdentity where
    type Sv GetCredentialsForIdentity = CognitoIdentity
    type Rs GetCredentialsForIdentity = GetCredentialsForIdentityResponse

    request  = post "GetCredentialsForIdentity"
    response = jsonResponse

instance FromJSON GetCredentialsForIdentityResponse where
    parseJSON = withObject "GetCredentialsForIdentityResponse" $ \o -> GetCredentialsForIdentityResponse
        <$> o .:? "Credentials"
        <*> o .:? "IdentityId"
