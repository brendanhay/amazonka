{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.TagRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM role. The role can be a regular role or a service-linked role. If a tag with the same key name already exists, then that tag is overwritten with the new value.
--
--
-- A tag consists of a key name and an associated value. By assigning tags to your resources, you can do the following:
--
--     * __Administrative grouping and discovery__ - Attach tags to resources to aid in organization and search. For example, you could search for all resources with the key name /Project/ and the value /MyImportantProject/ . Or search for all resources with the key name /Cost Center/ and the value /41200/ .
--
--     * __Access control__ - Reference tags in IAM user-based and resource-based policies. You can use tags to restrict access to only an IAM user or role that has a specified tag attached. You can also restrict access to only those resources that have a certain tag attached. For examples of policies that show how to use tags to control access, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control Access Using IAM Tags> in the /IAM User Guide/ .
--
--     * __Cost allocation__ - Use tags to help track which individuals and teams are using which AWS resources.
--
--
--
-- For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.TagRole
  ( -- * Creating a Request
    tagRole,
    TagRole,

    -- * Request Lenses
    trRoleName,
    trTags,

    -- * Destructuring the Response
    tagRoleResponse,
    TagRoleResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagRole' smart constructor.
data TagRole = TagRole' {_trRoleName :: !Text, _trTags :: ![Tag]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trRoleName' - The name of the role that you want to add tags to. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'trTags' - The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
tagRole ::
  -- | 'trRoleName'
  Text ->
  TagRole
tagRole pRoleName_ =
  TagRole' {_trRoleName = pRoleName_, _trTags = mempty}

-- | The name of the role that you want to add tags to. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
trRoleName :: Lens' TagRole Text
trRoleName = lens _trRoleName (\s a -> s {_trRoleName = a})

-- | The list of tags that you want to attach to the role. Each tag consists of a key name and an associated value. You can specify this with a JSON string.
trTags :: Lens' TagRole [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagRole where
  type Rs TagRole = TagRoleResponse
  request = postQuery iam
  response = receiveNull TagRoleResponse'

instance Hashable TagRole

instance NFData TagRole

instance ToHeaders TagRole where
  toHeaders = const mempty

instance ToPath TagRole where
  toPath = const "/"

instance ToQuery TagRole where
  toQuery TagRole' {..} =
    mconcat
      [ "Action" =: ("TagRole" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "RoleName" =: _trRoleName,
        "Tags" =: toQueryList "member" _trTags
      ]

-- | /See:/ 'tagRoleResponse' smart constructor.
data TagRoleResponse = TagRoleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagRoleResponse' with the minimum fields required to make a request.
tagRoleResponse ::
  TagRoleResponse
tagRoleResponse = TagRoleResponse'

instance NFData TagRoleResponse
