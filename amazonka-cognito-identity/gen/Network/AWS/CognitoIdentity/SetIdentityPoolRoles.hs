{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.SetIdentityPoolRoles
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

-- | Sets the roles for an identity pool. These roles are used when making
-- calls to @GetCredentialsForIdentity@ action.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_SetIdentityPoolRoles.html>
module Network.AWS.CognitoIdentity.SetIdentityPoolRoles
    (
    -- * Request
      SetIdentityPoolRoles
    -- ** Request constructor
    , setIdentityPoolRoles
    -- ** Request lenses
    , siprIdentityPoolId
    , siprRoles

    -- * Response
    , SetIdentityPoolRolesResponse
    -- ** Response constructor
    , setIdentityPoolRolesResponse
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @SetIdentityPoolRoles@ action.
--
-- /See:/ 'setIdentityPoolRoles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siprIdentityPoolId'
--
-- * 'siprRoles'
data SetIdentityPoolRoles = SetIdentityPoolRoles'
    { _siprIdentityPoolId :: !Text
    , _siprRoles          :: !(Map Text Text)
    } deriving (Eq,Read,Show)

-- | 'SetIdentityPoolRoles' smart constructor.
setIdentityPoolRoles :: Text -> SetIdentityPoolRoles
setIdentityPoolRoles pIdentityPoolId =
    SetIdentityPoolRoles'
    { _siprIdentityPoolId = pIdentityPoolId
    , _siprRoles = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
siprIdentityPoolId :: Lens' SetIdentityPoolRoles Text
siprIdentityPoolId = lens _siprIdentityPoolId (\ s a -> s{_siprIdentityPoolId = a});

-- | The map of roles associated with this pool. For a given role, the key
-- will be either \"authenticated\" or \"unauthenticated\" and the value
-- will be the Role ARN.
siprRoles :: Lens' SetIdentityPoolRoles (HashMap Text Text)
siprRoles = lens _siprRoles (\ s a -> s{_siprRoles = a}) . _Map;

instance AWSRequest SetIdentityPoolRoles where
        type Sv SetIdentityPoolRoles = CognitoIdentity
        type Rs SetIdentityPoolRoles =
             SetIdentityPoolRolesResponse
        request = postJSON
        response = receiveNull SetIdentityPoolRolesResponse'

instance ToHeaders SetIdentityPoolRoles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.SetIdentityPoolRoles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetIdentityPoolRoles where
        toJSON SetIdentityPoolRoles'{..}
          = object
              ["IdentityPoolId" .= _siprIdentityPoolId,
               "Roles" .= _siprRoles]

instance ToPath SetIdentityPoolRoles where
        toPath = const "/"

instance ToQuery SetIdentityPoolRoles where
        toQuery = const mempty

-- | /See:/ 'setIdentityPoolRolesResponse' smart constructor.
data SetIdentityPoolRolesResponse =
    SetIdentityPoolRolesResponse'
    deriving (Eq,Read,Show)

-- | 'SetIdentityPoolRolesResponse' smart constructor.
setIdentityPoolRolesResponse :: SetIdentityPoolRolesResponse
setIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
