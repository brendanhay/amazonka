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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
    (
    -- * Creating a Request
      adminAddUserToGroup
    , AdminAddUserToGroup
    -- * Request Lenses
    , aautgUserPoolId
    , aautgUsername
    , aautgGroupName

    -- * Destructuring the Response
    , adminAddUserToGroupResponse
    , AdminAddUserToGroupResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { _aautgUserPoolId :: !Text
  , _aautgUsername   :: !(Sensitive Text)
  , _aautgGroupName  :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminAddUserToGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aautgUserPoolId' - The user pool ID for the user pool.
--
-- * 'aautgUsername' - The username for the user.
--
-- * 'aautgGroupName' - The group name.
adminAddUserToGroup
    :: Text -- ^ 'aautgUserPoolId'
    -> Text -- ^ 'aautgUsername'
    -> Text -- ^ 'aautgGroupName'
    -> AdminAddUserToGroup
adminAddUserToGroup pUserPoolId_ pUsername_ pGroupName_ =
  AdminAddUserToGroup'
    { _aautgUserPoolId = pUserPoolId_
    , _aautgUsername = _Sensitive # pUsername_
    , _aautgGroupName = pGroupName_
    }


-- | The user pool ID for the user pool.
aautgUserPoolId :: Lens' AdminAddUserToGroup Text
aautgUserPoolId = lens _aautgUserPoolId (\ s a -> s{_aautgUserPoolId = a})

-- | The username for the user.
aautgUsername :: Lens' AdminAddUserToGroup Text
aautgUsername = lens _aautgUsername (\ s a -> s{_aautgUsername = a}) . _Sensitive

-- | The group name.
aautgGroupName :: Lens' AdminAddUserToGroup Text
aautgGroupName = lens _aautgGroupName (\ s a -> s{_aautgGroupName = a})

instance AWSRequest AdminAddUserToGroup where
        type Rs AdminAddUserToGroup =
             AdminAddUserToGroupResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull AdminAddUserToGroupResponse'

instance Hashable AdminAddUserToGroup where

instance NFData AdminAddUserToGroup where

instance ToHeaders AdminAddUserToGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminAddUserToGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminAddUserToGroup where
        toJSON AdminAddUserToGroup'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _aautgUserPoolId),
                  Just ("Username" .= _aautgUsername),
                  Just ("GroupName" .= _aautgGroupName)])

instance ToPath AdminAddUserToGroup where
        toPath = const "/"

instance ToQuery AdminAddUserToGroup where
        toQuery = const mempty

-- | /See:/ 'adminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse =
  AdminAddUserToGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminAddUserToGroupResponse' with the minimum fields required to make a request.
--
adminAddUserToGroupResponse
    :: AdminAddUserToGroupResponse
adminAddUserToGroupResponse = AdminAddUserToGroupResponse'


instance NFData AdminAddUserToGroupResponse where
