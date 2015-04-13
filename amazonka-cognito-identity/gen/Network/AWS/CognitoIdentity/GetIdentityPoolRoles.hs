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

-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
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

-- | Gets the roles for an identity pool.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetIdentityPoolRoles.html>
module Network.AWS.CognitoIdentity.GetIdentityPoolRoles
    (
    -- * Request
      GetIdentityPoolRoles
    -- ** Request constructor
    , getIdentityPoolRoles
    -- ** Request lenses
    , giprIdentityPoolId

    -- * Response
    , GetIdentityPoolRolesResponse
    -- ** Response constructor
    , getIdentityPoolRolesResponse
    -- ** Response lenses
    , giprrIdentityPoolId
    , giprrRoles
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

newtype GetIdentityPoolRoles = GetIdentityPoolRoles
    { _giprIdentityPoolId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'GetIdentityPoolRoles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprIdentityPoolId' @::@ 'Maybe' 'Text'
--
getIdentityPoolRoles :: GetIdentityPoolRoles
getIdentityPoolRoles = GetIdentityPoolRoles
    { _giprIdentityPoolId = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
giprIdentityPoolId :: Lens' GetIdentityPoolRoles (Maybe Text)
giprIdentityPoolId =
    lens _giprIdentityPoolId (\s a -> s { _giprIdentityPoolId = a })

data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse
    { _giprrIdentityPoolId :: Maybe Text
    , _giprrRoles          :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'GetIdentityPoolRolesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprrIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'giprrRoles' @::@ 'HashMap' 'Text' 'Text'
--
getIdentityPoolRolesResponse :: GetIdentityPoolRolesResponse
getIdentityPoolRolesResponse = GetIdentityPoolRolesResponse
    { _giprrIdentityPoolId = Nothing
    , _giprrRoles          = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
giprrIdentityPoolId :: Lens' GetIdentityPoolRolesResponse (Maybe Text)
giprrIdentityPoolId =
    lens _giprrIdentityPoolId (\s a -> s { _giprrIdentityPoolId = a })

-- | The map of roles associated with this pool. Currently only authenticated and
-- unauthenticated roles are supported.
giprrRoles :: Lens' GetIdentityPoolRolesResponse (HashMap Text Text)
giprrRoles = lens _giprrRoles (\s a -> s { _giprrRoles = a }) . _Map

instance ToPath GetIdentityPoolRoles where
    toPath = const "/"

instance ToQuery GetIdentityPoolRoles where
    toQuery = const mempty

instance ToHeaders GetIdentityPoolRoles

instance ToJSON GetIdentityPoolRoles where
    toJSON GetIdentityPoolRoles{..} = object
        [ "IdentityPoolId" .= _giprIdentityPoolId
        ]

instance AWSRequest GetIdentityPoolRoles where
    type Sv GetIdentityPoolRoles = CognitoIdentity
    type Rs GetIdentityPoolRoles = GetIdentityPoolRolesResponse

    request  = post "GetIdentityPoolRoles"
    response = jsonResponse

instance FromJSON GetIdentityPoolRolesResponse where
    parseJSON = withObject "GetIdentityPoolRolesResponse" $ \o -> GetIdentityPoolRolesResponse
        <$> o .:? "IdentityPoolId"
        <*> o .:? "Roles" .!= mempty
