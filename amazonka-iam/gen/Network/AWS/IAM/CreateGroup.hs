{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.CreateGroup
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

-- | Creates a new group.
--
-- For information about the number of groups you can create, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroup.html>
module Network.AWS.IAM.CreateGroup
    (
    -- * Request
      CreateGroup
    -- ** Request constructor
    , createGroup
    -- ** Request lenses
    , cgPath
    , cgGroupName

    -- * Response
    , CreateGroupResponse
    -- ** Response constructor
    , createGroupResponse
    -- ** Response lenses
    , cgrGroup
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'createGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgPath'
--
-- * 'cgGroupName'
data CreateGroup = CreateGroup'{_cgPath :: Maybe Text, _cgGroupName :: Text} deriving (Eq, Read, Show)

-- | 'CreateGroup' smart constructor.
createGroup :: Text -> CreateGroup
createGroup pGroupName = CreateGroup'{_cgPath = Nothing, _cgGroupName = pGroupName};

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
        type Sv CreateGroup = IAM
        type Rs CreateGroup = CreateGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateGroupResult"
              (\ s h x -> CreateGroupResponse' <$> x .@ "Group")

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

-- | /See:/ 'createGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgrGroup'
newtype CreateGroupResponse = CreateGroupResponse'{_cgrGroup :: Group} deriving (Eq, Read, Show)

-- | 'CreateGroupResponse' smart constructor.
createGroupResponse :: Group -> CreateGroupResponse
createGroupResponse pGroup = CreateGroupResponse'{_cgrGroup = pGroup};

-- | Information about the group.
cgrGroup :: Lens' CreateGroupResponse Group
cgrGroup = lens _cgrGroup (\ s a -> s{_cgrGroup = a});
