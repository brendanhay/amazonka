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
-- Module      : Network.AWS.IAM.TagUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM user. If a tag with the same key name already exists, then that tag is overwritten with the new value.
--
--
-- A tag consists of a key name and an associated value. By assigning tags to your resources, you can do the following:
--
--     * __Administrative grouping and discovery__ - Attach tags to resources to aid in organization and search. For example, you could search for all resources with the key name /Project/ and the value /MyImportantProject/ . Or search for all resources with the key name /Cost Center/ and the value /41200/ .
--
--     * __Access control__ - Reference tags in IAM user-based and resource-based policies. You can use tags to restrict access to only an IAM requesting user or to a role that has a specified tag attached. You can also restrict access to only those resources that have a certain tag attached. For examples of policies that show how to use tags to control access, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control Access Using IAM Tags> in the /IAM User Guide/ .
--
--     * __Cost allocation__ - Use tags to help track which individuals and teams are using which AWS resources.
--
--
--
-- For more information about tagging, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
module Network.AWS.IAM.TagUser
    (
    -- * Creating a Request
      tagUser
    , TagUser
    -- * Request Lenses
    , tuUserName
    , tuTags

    -- * Destructuring the Response
    , tagUserResponse
    , TagUserResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagUser' smart constructor.
data TagUser = TagUser'
  { _tuUserName :: !Text
  , _tuTags     :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuUserName' - The name of the user that you want to add tags to. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- * 'tuTags' - The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
tagUser
    :: Text -- ^ 'tuUserName'
    -> TagUser
tagUser pUserName_ = TagUser' {_tuUserName = pUserName_, _tuTags = mempty}


-- | The name of the user that you want to add tags to. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
tuUserName :: Lens' TagUser Text
tuUserName = lens _tuUserName (\ s a -> s{_tuUserName = a})

-- | The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
tuTags :: Lens' TagUser [Tag]
tuTags = lens _tuTags (\ s a -> s{_tuTags = a}) . _Coerce

instance AWSRequest TagUser where
        type Rs TagUser = TagUserResponse
        request = postQuery iam
        response = receiveNull TagUserResponse'

instance Hashable TagUser where

instance NFData TagUser where

instance ToHeaders TagUser where
        toHeaders = const mempty

instance ToPath TagUser where
        toPath = const "/"

instance ToQuery TagUser where
        toQuery TagUser'{..}
          = mconcat
              ["Action" =: ("TagUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _tuUserName,
               "Tags" =: toQueryList "member" _tuTags]

-- | /See:/ 'tagUserResponse' smart constructor.
data TagUserResponse =
  TagUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagUserResponse' with the minimum fields required to make a request.
--
tagUserResponse
    :: TagUserResponse
tagUserResponse = TagUserResponse'


instance NFData TagUserResponse where
