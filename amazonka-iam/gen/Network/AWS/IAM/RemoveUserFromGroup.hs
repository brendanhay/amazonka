{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveUserFromGroup.html>
module Network.AWS.IAM.RemoveUserFromGroup
    (
    -- * Request
      RemoveUserFromGroup
    -- ** Request constructor
    , removeUserFromGroup
    -- ** Request lenses
    , rufgrqGroupName
    , rufgrqUserName

    -- * Response
    , RemoveUserFromGroupResponse
    -- ** Response constructor
    , removeUserFromGroupResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeUserFromGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rufgrqGroupName'
--
-- * 'rufgrqUserName'
data RemoveUserFromGroup = RemoveUserFromGroup'
    { _rufgrqGroupName :: !Text
    , _rufgrqUserName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveUserFromGroup' smart constructor.
removeUserFromGroup :: Text -> Text -> RemoveUserFromGroup
removeUserFromGroup pGroupName_ pUserName_ =
    RemoveUserFromGroup'
    { _rufgrqGroupName = pGroupName_
    , _rufgrqUserName = pUserName_
    }

-- | The name of the group to update.
rufgrqGroupName :: Lens' RemoveUserFromGroup Text
rufgrqGroupName = lens _rufgrqGroupName (\ s a -> s{_rufgrqGroupName = a});

-- | The name of the user to remove.
rufgrqUserName :: Lens' RemoveUserFromGroup Text
rufgrqUserName = lens _rufgrqUserName (\ s a -> s{_rufgrqUserName = a});

instance AWSRequest RemoveUserFromGroup where
        type Sv RemoveUserFromGroup = IAM
        type Rs RemoveUserFromGroup =
             RemoveUserFromGroupResponse
        request = post
        response = receiveNull RemoveUserFromGroupResponse'

instance ToHeaders RemoveUserFromGroup where
        toHeaders = const mempty

instance ToPath RemoveUserFromGroup where
        toPath = const "/"

instance ToQuery RemoveUserFromGroup where
        toQuery RemoveUserFromGroup'{..}
          = mconcat
              ["Action" =: ("RemoveUserFromGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _rufgrqGroupName,
               "UserName" =: _rufgrqUserName]

-- | /See:/ 'removeUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse =
    RemoveUserFromGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveUserFromGroupResponse' smart constructor.
removeUserFromGroupResponse :: RemoveUserFromGroupResponse
removeUserFromGroupResponse = RemoveUserFromGroupResponse'
