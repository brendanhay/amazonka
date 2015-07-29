{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the roles for an identity pool.
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
    , giprrsRoles
    , giprrsIdentityPoolId
    , giprrsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @GetIdentityPoolRoles@ action.
--
-- /See:/ 'getIdentityPoolRoles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprIdentityPoolId'
newtype GetIdentityPoolRoles = GetIdentityPoolRoles'
    { _giprIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityPoolRoles' smart constructor.
getIdentityPoolRoles :: Text -> GetIdentityPoolRoles
getIdentityPoolRoles pIdentityPoolId_ =
    GetIdentityPoolRoles'
    { _giprIdentityPoolId = pIdentityPoolId_
    }

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
                   (x .?> "Roles" .!@ mempty) <*>
                     (x .?> "IdentityPoolId")
                     <*> (pure (fromEnum s)))

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
        toPath = const mempty

instance ToQuery GetIdentityPoolRoles where
        toQuery = const mempty

-- | Returned in response to a successful @GetIdentityPoolRoles@ operation.
--
-- /See:/ 'getIdentityPoolRolesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprrsRoles'
--
-- * 'giprrsIdentityPoolId'
--
-- * 'giprrsStatus'
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'
    { _giprrsRoles          :: !(Maybe (Map Text Text))
    , _giprrsIdentityPoolId :: !(Maybe Text)
    , _giprrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityPoolRolesResponse' smart constructor.
getIdentityPoolRolesResponse :: Int -> GetIdentityPoolRolesResponse
getIdentityPoolRolesResponse pStatus_ =
    GetIdentityPoolRolesResponse'
    { _giprrsRoles = Nothing
    , _giprrsIdentityPoolId = Nothing
    , _giprrsStatus = pStatus_
    }

-- | The map of roles associated with this pool. Currently only authenticated
-- and unauthenticated roles are supported.
giprrsRoles :: Lens' GetIdentityPoolRolesResponse (HashMap Text Text)
giprrsRoles = lens _giprrsRoles (\ s a -> s{_giprrsRoles = a}) . _Default . _Map;

-- | An identity pool ID in the format REGION:GUID.
giprrsIdentityPoolId :: Lens' GetIdentityPoolRolesResponse (Maybe Text)
giprrsIdentityPoolId = lens _giprrsIdentityPoolId (\ s a -> s{_giprrsIdentityPoolId = a});

-- | FIXME: Undocumented member.
giprrsStatus :: Lens' GetIdentityPoolRolesResponse Int
giprrsStatus = lens _giprrsStatus (\ s a -> s{_giprrsStatus = a});
