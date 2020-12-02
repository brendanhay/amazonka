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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group.
--
--
-- For information about the number of groups you can create, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
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

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgPath      :: !(Maybe Text)
  , _cgGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgPath' - The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'cgGroupName' - The name of the group to create. Do not include the path in this value. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-. The group name must be unique within the account. Group names are not distinguished by case. For example, you cannot create groups named both "ADMINS" and "admins".
createGroup
    :: Text -- ^ 'cgGroupName'
    -> CreateGroup
createGroup pGroupName_ =
  CreateGroup' {_cgPath = Nothing, _cgGroupName = pGroupName_}


-- | The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
cgPath :: Lens' CreateGroup (Maybe Text)
cgPath = lens _cgPath (\ s a -> s{_cgPath = a})

-- | The name of the group to create. Do not include the path in this value. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-. The group name must be unique within the account. Group names are not distinguished by case. For example, you cannot create groups named both "ADMINS" and "admins".
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\ s a -> s{_cgGroupName = a})

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateGroupResult"
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Group"))

instance Hashable CreateGroup where

instance NFData CreateGroup where

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

-- | Contains the response to a successful 'CreateGroup' request.
--
--
--
-- /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsResponseStatus :: !Int
  , _cgrsGroup          :: !Group
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsResponseStatus' - -- | The response status code.
--
-- * 'cgrsGroup' - A structure containing details about the new group.
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> Group -- ^ 'cgrsGroup'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ pGroup_ =
  CreateGroupResponse'
    {_cgrsResponseStatus = pResponseStatus_, _cgrsGroup = pGroup_}


-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

-- | A structure containing details about the new group.
cgrsGroup :: Lens' CreateGroupResponse Group
cgrsGroup = lens _cgrsGroup (\ s a -> s{_cgrsGroup = a})

instance NFData CreateGroupResponse where
