{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
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

-- | Gets the roles for an identity pool.
--
-- You must use AWS Developer credentials to call this API.
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
    , giprrRoles
    , giprrIdentityPoolId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'getIdentityPoolRoles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprIdentityPoolId'
newtype GetIdentityPoolRoles = GetIdentityPoolRoles'{_giprIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'GetIdentityPoolRoles' smart constructor.
getIdentityPoolRoles :: Text -> GetIdentityPoolRoles
getIdentityPoolRoles pIdentityPoolId = GetIdentityPoolRoles'{_giprIdentityPoolId = pIdentityPoolId};

-- | An identity pool ID in the format REGION:GUID.
giprIdentityPoolId :: Lens' GetIdentityPoolRoles Text
giprIdentityPoolId = lens _giprIdentityPoolId (\ s a -> s{_giprIdentityPoolId = a});

instance AWSRequest GetIdentityPoolRoles where
        type Sv GetIdentityPoolRoles = CognitoIdentity
        type Rs GetIdentityPoolRoles =
             GetIdentityPoolRolesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetIdentityPoolRolesResponse' <$>
                   x .?> "Roles" .!@ mempty <*> x .:> "IdentityPoolId")

instance ToHeaders GetIdentityPoolRoles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetIdentityPoolRoles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetIdentityPoolRoles where
        toJSON GetIdentityPoolRoles'{..}
          = object ["IdentityPoolId" .= _giprIdentityPoolId]

instance ToPath GetIdentityPoolRoles where
        toPath = const "/"

instance ToQuery GetIdentityPoolRoles where
        toQuery = const mempty

-- | /See:/ 'getIdentityPoolRolesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprrRoles'
--
-- * 'giprrIdentityPoolId'
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'{_giprrRoles :: HashMap Text Text, _giprrIdentityPoolId :: Text} deriving (Eq, Read, Show)

-- | 'GetIdentityPoolRolesResponse' smart constructor.
getIdentityPoolRolesResponse :: Text -> GetIdentityPoolRolesResponse
getIdentityPoolRolesResponse pIdentityPoolId = GetIdentityPoolRolesResponse'{_giprrRoles = mempty, _giprrIdentityPoolId = pIdentityPoolId};

-- | The map of roles associated with this pool. Currently only authenticated
-- and unauthenticated roles are supported.
giprrRoles :: Lens' GetIdentityPoolRolesResponse (HashMap Text Text)
giprrRoles = lens _giprrRoles (\ s a -> s{_giprrRoles = a}) . _Coerce;

-- | An identity pool ID in the format REGION:GUID.
giprrIdentityPoolId :: Lens' GetIdentityPoolRolesResponse Text
giprrIdentityPoolId = lens _giprrIdentityPoolId (\ s a -> s{_giprrIdentityPoolId = a});
