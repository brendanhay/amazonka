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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
    (
    -- * Creating a Request
      adminRemoveUserFromGroup
    , AdminRemoveUserFromGroup
    -- * Request Lenses
    , arufgUserPoolId
    , arufgUsername
    , arufgGroupName

    -- * Destructuring the Response
    , adminRemoveUserFromGroupResponse
    , AdminRemoveUserFromGroupResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminRemoveUserFromGroup' smart constructor.
data AdminRemoveUserFromGroup = AdminRemoveUserFromGroup'
  { _arufgUserPoolId :: !Text
  , _arufgUsername   :: !(Sensitive Text)
  , _arufgGroupName  :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminRemoveUserFromGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arufgUserPoolId' - The user pool ID for the user pool.
--
-- * 'arufgUsername' - The username for the user.
--
-- * 'arufgGroupName' - The group name.
adminRemoveUserFromGroup
    :: Text -- ^ 'arufgUserPoolId'
    -> Text -- ^ 'arufgUsername'
    -> Text -- ^ 'arufgGroupName'
    -> AdminRemoveUserFromGroup
adminRemoveUserFromGroup pUserPoolId_ pUsername_ pGroupName_ =
  AdminRemoveUserFromGroup'
    { _arufgUserPoolId = pUserPoolId_
    , _arufgUsername = _Sensitive # pUsername_
    , _arufgGroupName = pGroupName_
    }


-- | The user pool ID for the user pool.
arufgUserPoolId :: Lens' AdminRemoveUserFromGroup Text
arufgUserPoolId = lens _arufgUserPoolId (\ s a -> s{_arufgUserPoolId = a})

-- | The username for the user.
arufgUsername :: Lens' AdminRemoveUserFromGroup Text
arufgUsername = lens _arufgUsername (\ s a -> s{_arufgUsername = a}) . _Sensitive

-- | The group name.
arufgGroupName :: Lens' AdminRemoveUserFromGroup Text
arufgGroupName = lens _arufgGroupName (\ s a -> s{_arufgGroupName = a})

instance AWSRequest AdminRemoveUserFromGroup where
        type Rs AdminRemoveUserFromGroup =
             AdminRemoveUserFromGroupResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveNull AdminRemoveUserFromGroupResponse'

instance Hashable AdminRemoveUserFromGroup where

instance NFData AdminRemoveUserFromGroup where

instance ToHeaders AdminRemoveUserFromGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminRemoveUserFromGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminRemoveUserFromGroup where
        toJSON AdminRemoveUserFromGroup'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _arufgUserPoolId),
                  Just ("Username" .= _arufgUsername),
                  Just ("GroupName" .= _arufgGroupName)])

instance ToPath AdminRemoveUserFromGroup where
        toPath = const "/"

instance ToQuery AdminRemoveUserFromGroup where
        toQuery = const mempty

-- | /See:/ 'adminRemoveUserFromGroupResponse' smart constructor.
data AdminRemoveUserFromGroupResponse =
  AdminRemoveUserFromGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminRemoveUserFromGroupResponse' with the minimum fields required to make a request.
--
adminRemoveUserFromGroupResponse
    :: AdminRemoveUserFromGroupResponse
adminRemoveUserFromGroupResponse = AdminRemoveUserFromGroupResponse'


instance NFData AdminRemoveUserFromGroupResponse
         where
