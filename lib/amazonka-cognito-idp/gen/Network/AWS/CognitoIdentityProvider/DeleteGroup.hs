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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group. Currently only groups with no members can be deleted.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dgGroupName
    , dgUserPoolId

    -- * Destructuring the Response
    , deleteGroupResponse
    , DeleteGroupResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { _dgGroupName  :: !Text
  , _dgUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroupName' - The name of the group.
--
-- * 'dgUserPoolId' - The user pool ID for the user pool.
deleteGroup
    :: Text -- ^ 'dgGroupName'
    -> Text -- ^ 'dgUserPoolId'
    -> DeleteGroup
deleteGroup pGroupName_ pUserPoolId_ =
  DeleteGroup' {_dgGroupName = pGroupName_, _dgUserPoolId = pUserPoolId_}


-- | The name of the group.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\ s a -> s{_dgGroupName = a})

-- | The user pool ID for the user pool.
dgUserPoolId :: Lens' DeleteGroup Text
dgUserPoolId = lens _dgUserPoolId (\ s a -> s{_dgUserPoolId = a})

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull DeleteGroupResponse'

instance Hashable DeleteGroup where

instance NFData DeleteGroup where

instance ToHeaders DeleteGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteGroup where
        toJSON DeleteGroup'{..}
          = object
              (catMaybes
                 [Just ("GroupName" .= _dgGroupName),
                  Just ("UserPoolId" .= _dgUserPoolId)])

instance ToPath DeleteGroup where
        toPath = const "/"

instance ToQuery DeleteGroup where
        toQuery = const mempty

-- | /See:/ 'deleteGroupResponse' smart constructor.
data DeleteGroupResponse =
  DeleteGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
deleteGroupResponse
    :: DeleteGroupResponse
deleteGroupResponse = DeleteGroupResponse'


instance NFData DeleteGroupResponse where
