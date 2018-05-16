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
-- Module      : Network.AWS.EFS.CreateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or overwrites tags associated with a file system. Each tag is a key-value pair. If a tag key specified in the request already exists on the file system, this operation overwrites its value with the value provided in the request. If you add the @Name@ tag to your file system, Amazon EFS returns it in the response to the 'DescribeFileSystems' operation.
--
--
-- This operation requires permission for the @elasticfilesystem:CreateTags@ action.
--
module Network.AWS.EFS.CreateTags
    (
    -- * Creating a Request
      createTags
    , CreateTags
    -- * Request Lenses
    , ctFileSystemId
    , ctTags

    -- * Destructuring the Response
    , createTagsResponse
    , CreateTagsResponse
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctFileSystemId :: !Text
  , _ctTags         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctFileSystemId' - ID of the file system whose tags you want to modify (String). This operation modifies the tags only, not the file system.
--
-- * 'ctTags' - Array of @Tag@ objects to add. Each @Tag@ object is a key-value pair.
createTags
    :: Text -- ^ 'ctFileSystemId'
    -> CreateTags
createTags pFileSystemId_ =
  CreateTags' {_ctFileSystemId = pFileSystemId_, _ctTags = mempty}


-- | ID of the file system whose tags you want to modify (String). This operation modifies the tags only, not the file system.
ctFileSystemId :: Lens' CreateTags Text
ctFileSystemId = lens _ctFileSystemId (\ s a -> s{_ctFileSystemId = a})

-- | Array of @Tag@ objects to add. Each @Tag@ object is a key-value pair.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Coerce

instance AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        request = postJSON efs
        response = receiveNull CreateTagsResponse'

instance Hashable CreateTags where

instance NFData CreateTags where

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToJSON CreateTags where
        toJSON CreateTags'{..}
          = object (catMaybes [Just ("Tags" .= _ctTags)])

instance ToPath CreateTags where
        toPath CreateTags'{..}
          = mconcat
              ["/2015-02-01/create-tags/", toBS _ctFileSystemId]

instance ToQuery CreateTags where
        toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
  CreateTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
createTagsResponse
    :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'


instance NFData CreateTagsResponse where
