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
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified IAM group.
--
--
-- /Important:/ You should understand the implications of changing a group's path or name. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups> in the /IAM User Guide/ .
--
module Network.AWS.IAM.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugNewGroupName
    , ugNewPath
    , ugGroupName

    -- * Destructuring the Response
    , updateGroupResponse
    , UpdateGroupResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { _ugNewGroupName :: !(Maybe Text)
  , _ugNewPath      :: !(Maybe Text)
  , _ugGroupName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugNewGroupName' - New name for the IAM group. Only include this if changing the group's name. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'ugNewPath' - New path for the IAM group. Only include this if changing the group's path. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'ugGroupName' - Name of the IAM group to update. If you're changing the name of the group, this is the original name. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
updateGroup
    :: Text -- ^ 'ugGroupName'
    -> UpdateGroup
updateGroup pGroupName_ =
  UpdateGroup'
    { _ugNewGroupName = Nothing
    , _ugNewPath = Nothing
    , _ugGroupName = pGroupName_
    }


-- | New name for the IAM group. Only include this if changing the group's name. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
ugNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugNewGroupName = lens _ugNewGroupName (\ s a -> s{_ugNewGroupName = a})

-- | New path for the IAM group. Only include this if changing the group's path. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
ugNewPath :: Lens' UpdateGroup (Maybe Text)
ugNewPath = lens _ugNewPath (\ s a -> s{_ugNewPath = a})

-- | Name of the IAM group to update. If you're changing the name of the group, this is the original name. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\ s a -> s{_ugGroupName = a})

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = postQuery iam
        response = receiveNull UpdateGroupResponse'

instance Hashable UpdateGroup where

instance NFData UpdateGroup where

instance ToHeaders UpdateGroup where
        toHeaders = const mempty

instance ToPath UpdateGroup where
        toPath = const "/"

instance ToQuery UpdateGroup where
        toQuery UpdateGroup'{..}
          = mconcat
              ["Action" =: ("UpdateGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "NewGroupName" =: _ugNewGroupName,
               "NewPath" =: _ugNewPath, "GroupName" =: _ugGroupName]

-- | /See:/ 'updateGroupResponse' smart constructor.
data UpdateGroupResponse =
  UpdateGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
updateGroupResponse
    :: UpdateGroupResponse
updateGroupResponse = UpdateGroupResponse'


instance NFData UpdateGroupResponse where
