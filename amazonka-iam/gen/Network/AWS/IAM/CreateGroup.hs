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
-- Module      : Network.AWS.IAM.CreateGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group.
--
-- For information about the number of groups you can create, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /IAM User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroup.html AWS API Reference> for CreateGroup.
module Network.AWS.IAM.CreateGroup
    (
    -- * Creating a Request
      createGroup
    , CreateGroup
    -- * Request Lenses
    , cgPath
    , cgGroupName

    -- * Destructuring the Response
    , createGroupResponse
    , CreateGroupResponse
    -- * Response Lenses
    , cgrsResponseStatus
    , cgrsGroup
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
    { _cgPath      :: !(Maybe Text)
    , _cgGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgPath'
--
-- * 'cgGroupName'
createGroup
    :: Text -- ^ 'cgGroupName'
    -> CreateGroup
createGroup pGroupName_ =
    CreateGroup'
    { _cgPath = Nothing
    , _cgGroupName = pGroupName_
    }

-- | The path to the group. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cgPath :: Lens' CreateGroup (Maybe Text)
cgPath = lens _cgPath (\ s a -> s{_cgPath = a});

-- | The name of the group to create. Do not include the path in this value.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\ s a -> s{_cgGroupName = a});

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "CreateGroupResult"
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Group"))

instance ToHeaders CreateGroup where
        toHeaders = const mempty

instance ToPath CreateGroup where
        toPath = const "/"

instance ToQuery CreateGroup where
        toQuery CreateGroup'{..}
          = mconcat
              ["Action" =: ("CreateGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cgPath, "GroupName" =: _cgGroupName]

-- | Contains the response to a successful CreateGroup request.
--
-- /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
    { _cgrsResponseStatus :: !Int
    , _cgrsGroup          :: !Group
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsResponseStatus'
--
-- * 'cgrsGroup'
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> Group -- ^ 'cgrsGroup'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ pGroup_ =
    CreateGroupResponse'
    { _cgrsResponseStatus = pResponseStatus_
    , _cgrsGroup = pGroup_
    }

-- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a});

-- | Information about the group.
cgrsGroup :: Lens' CreateGroupResponse Group
cgrsGroup = lens _cgrsGroup (\ s a -> s{_cgrsGroup = a});
