{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EFS.CreateTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates or overwrites tags associated with a file system. Each tag is a
-- key-value pair. If a tag key specified in the request already exists on
-- the file system, this operation overwrites its value with the value
-- provided in the request. If you add the \"Name\" tag to your file
-- system, Amazon EFS returns it in the response to the DescribeFileSystems
-- API.
--
-- This operation requires permission for the
-- @elasticfilesystem:CreateTags@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateTags.html>
module Network.AWS.EFS.CreateTags
    (
    -- * Request
      CreateTags
    -- ** Request constructor
    , createTags
    -- ** Request lenses
    , ctFileSystemId
    , ctTags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctFileSystemId'
--
-- * 'ctTags'
data CreateTags = CreateTags'
    { _ctFileSystemId :: !Text
    , _ctTags         :: ![Tag]
    } deriving (Eq,Read,Show)

-- | 'CreateTags' smart constructor.
createTags :: Text -> CreateTags
createTags pFileSystemId =
    CreateTags'
    { _ctFileSystemId = pFileSystemId
    , _ctTags = mempty
    }

-- | String. The ID of the file system whose tags you want to modify. This
-- operation modifies only the tags and not the file system.
ctFileSystemId :: Lens' CreateTags Text
ctFileSystemId = lens _ctFileSystemId (\ s a -> s{_ctFileSystemId = a});

-- | An array of @Tag@ objects to add. Each @Tag@ object is a key-value pair.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a});

instance AWSRequest CreateTags where
        type Sv CreateTags = EFS
        type Rs CreateTags = CreateTagsResponse
        request = postJSON
        response = receiveNull CreateTagsResponse'

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToJSON CreateTags where
        toJSON CreateTags'{..} = object ["Tags" .= _ctTags]

instance ToPath CreateTags where
        toPath CreateTags'{..}
          = mconcat
              ["/2015-02-01/create-tags/", toText _ctFileSystemId]

instance ToQuery CreateTags where
        toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
    CreateTagsResponse'
    deriving (Eq,Read,Show)

-- | 'CreateTagsResponse' smart constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'
