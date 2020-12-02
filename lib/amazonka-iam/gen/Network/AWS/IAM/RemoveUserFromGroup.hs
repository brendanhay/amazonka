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
-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
--
module Network.AWS.IAM.RemoveUserFromGroup
    (
    -- * Creating a Request
      removeUserFromGroup
    , RemoveUserFromGroup
    -- * Request Lenses
    , rufgGroupName
    , rufgUserName

    -- * Destructuring the Response
    , removeUserFromGroupResponse
    , RemoveUserFromGroupResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeUserFromGroup' smart constructor.
data RemoveUserFromGroup = RemoveUserFromGroup'
  { _rufgGroupName :: !Text
  , _rufgUserName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveUserFromGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rufgGroupName' - The name of the group to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'rufgUserName' - The name of the user to remove. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
removeUserFromGroup
    :: Text -- ^ 'rufgGroupName'
    -> Text -- ^ 'rufgUserName'
    -> RemoveUserFromGroup
removeUserFromGroup pGroupName_ pUserName_ =
  RemoveUserFromGroup'
    {_rufgGroupName = pGroupName_, _rufgUserName = pUserName_}


-- | The name of the group to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rufgGroupName :: Lens' RemoveUserFromGroup Text
rufgGroupName = lens _rufgGroupName (\ s a -> s{_rufgGroupName = a})

-- | The name of the user to remove. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rufgUserName :: Lens' RemoveUserFromGroup Text
rufgUserName = lens _rufgUserName (\ s a -> s{_rufgUserName = a})

instance AWSRequest RemoveUserFromGroup where
        type Rs RemoveUserFromGroup =
             RemoveUserFromGroupResponse
        request = postQuery iam
        response = receiveNull RemoveUserFromGroupResponse'

instance Hashable RemoveUserFromGroup where

instance NFData RemoveUserFromGroup where

instance ToHeaders RemoveUserFromGroup where
        toHeaders = const mempty

instance ToPath RemoveUserFromGroup where
        toPath = const "/"

instance ToQuery RemoveUserFromGroup where
        toQuery RemoveUserFromGroup'{..}
          = mconcat
              ["Action" =: ("RemoveUserFromGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _rufgGroupName,
               "UserName" =: _rufgUserName]

-- | /See:/ 'removeUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse =
  RemoveUserFromGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveUserFromGroupResponse' with the minimum fields required to make a request.
--
removeUserFromGroupResponse
    :: RemoveUserFromGroupResponse
removeUserFromGroupResponse = RemoveUserFromGroupResponse'


instance NFData RemoveUserFromGroupResponse where
