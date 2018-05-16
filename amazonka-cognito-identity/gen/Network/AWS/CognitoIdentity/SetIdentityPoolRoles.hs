{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.SetIdentityPoolRoles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the roles for an identity pool. These roles are used when making calls to 'GetCredentialsForIdentity' action.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.SetIdentityPoolRoles
    (
    -- * Creating a Request
      setIdentityPoolRoles
    , SetIdentityPoolRoles
    -- * Request Lenses
    , siprRoleMappings
    , siprIdentityPoolId
    , siprRoles

    -- * Destructuring the Response
    , setIdentityPoolRolesResponse
    , SetIdentityPoolRolesResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the @SetIdentityPoolRoles@ action.
--
--
--
-- /See:/ 'setIdentityPoolRoles' smart constructor.
data SetIdentityPoolRoles = SetIdentityPoolRoles'
  { _siprRoleMappings   :: !(Maybe (Map Text RoleMapping))
  , _siprIdentityPoolId :: !Text
  , _siprRoles          :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityPoolRoles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siprRoleMappings' - How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id". Up to 25 rules can be specified per identity provider.
--
-- * 'siprIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'siprRoles' - The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
setIdentityPoolRoles
    :: Text -- ^ 'siprIdentityPoolId'
    -> SetIdentityPoolRoles
setIdentityPoolRoles pIdentityPoolId_ =
  SetIdentityPoolRoles'
    { _siprRoleMappings = Nothing
    , _siprIdentityPoolId = pIdentityPoolId_
    , _siprRoles = mempty
    }


-- | How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id". Up to 25 rules can be specified per identity provider.
siprRoleMappings :: Lens' SetIdentityPoolRoles (HashMap Text RoleMapping)
siprRoleMappings = lens _siprRoleMappings (\ s a -> s{_siprRoleMappings = a}) . _Default . _Map

-- | An identity pool ID in the format REGION:GUID.
siprIdentityPoolId :: Lens' SetIdentityPoolRoles Text
siprIdentityPoolId = lens _siprIdentityPoolId (\ s a -> s{_siprIdentityPoolId = a})

-- | The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
siprRoles :: Lens' SetIdentityPoolRoles (HashMap Text Text)
siprRoles = lens _siprRoles (\ s a -> s{_siprRoles = a}) . _Map

instance AWSRequest SetIdentityPoolRoles where
        type Rs SetIdentityPoolRoles =
             SetIdentityPoolRolesResponse
        request = postJSON cognitoIdentity
        response = receiveNull SetIdentityPoolRolesResponse'

instance Hashable SetIdentityPoolRoles where

instance NFData SetIdentityPoolRoles where

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
              (catMaybes
                 [("RoleMappings" .=) <$> _siprRoleMappings,
                  Just ("IdentityPoolId" .= _siprIdentityPoolId),
                  Just ("Roles" .= _siprRoles)])

instance ToPath SetIdentityPoolRoles where
        toPath = const "/"

instance ToQuery SetIdentityPoolRoles where
        toQuery = const mempty

-- | /See:/ 'setIdentityPoolRolesResponse' smart constructor.
data SetIdentityPoolRolesResponse =
  SetIdentityPoolRolesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityPoolRolesResponse' with the minimum fields required to make a request.
--
setIdentityPoolRolesResponse
    :: SetIdentityPoolRolesResponse
setIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'


instance NFData SetIdentityPoolRolesResponse where
