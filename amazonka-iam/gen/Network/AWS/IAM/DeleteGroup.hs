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
-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified group. The group must not contain any users or
-- have any attached policies.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroup.html AWS API Reference> for DeleteGroup.
module Network.AWS.IAM.DeleteGroup
    (
    -- * Creating a Request
      DeleteGroup
    , deleteGroup
    -- * Request Lenses
    , dgGroupName

    -- * Destructuring the Response
    , DeleteGroupResponse
    , deleteGroupResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgGroupName'
newtype DeleteGroup = DeleteGroup'
    { _dgGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteGroup' smart constructor.
deleteGroup :: Text -> DeleteGroup
deleteGroup pGroupName_ = 
    DeleteGroup'
    { _dgGroupName = pGroupName_
    }

-- | The name of the group to delete.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\ s a -> s{_dgGroupName = a});

instance AWSRequest DeleteGroup where
        type Sv DeleteGroup = IAM
        type Rs DeleteGroup = DeleteGroupResponse
        request = postQuery
        response = receiveNull DeleteGroupResponse'

instance ToHeaders DeleteGroup where
        toHeaders = const mempty

instance ToPath DeleteGroup where
        toPath = const "/"

instance ToQuery DeleteGroup where
        toQuery DeleteGroup'{..}
          = mconcat
              ["Action" =: ("DeleteGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _dgGroupName]

-- | /See:/ 'deleteGroupResponse' smart constructor.
data DeleteGroupResponse =
    DeleteGroupResponse' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteGroupResponse' smart constructor.
deleteGroupResponse :: DeleteGroupResponse
deleteGroupResponse = DeleteGroupResponse'
