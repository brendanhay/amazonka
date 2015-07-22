{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or overwrites tags associated with a file system. Each tag is a
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
    , ctrqFileSystemId
    , ctrqTags

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
-- * 'ctrqFileSystemId'
--
-- * 'ctrqTags'
data CreateTags = CreateTags'
    { _ctrqFileSystemId :: !Text
    , _ctrqTags         :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTags' smart constructor.
createTags :: Text -> CreateTags
createTags pFileSystemId =
    CreateTags'
    { _ctrqFileSystemId = pFileSystemId
    , _ctrqTags = mempty
    }

-- | String. The ID of the file system whose tags you want to modify. This
-- operation modifies only the tags and not the file system.
ctrqFileSystemId :: Lens' CreateTags Text
ctrqFileSystemId = lens _ctrqFileSystemId (\ s a -> s{_ctrqFileSystemId = a});

-- | An array of @Tag@ objects to add. Each @Tag@ object is a key-value pair.
ctrqTags :: Lens' CreateTags [Tag]
ctrqTags = lens _ctrqTags (\ s a -> s{_ctrqTags = a});

instance AWSRequest CreateTags where
        type Sv CreateTags = EFS
        type Rs CreateTags = CreateTagsResponse
        request = postJSON
        response = receiveNull CreateTagsResponse'

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToJSON CreateTags where
        toJSON CreateTags'{..} = object ["Tags" .= _ctrqTags]

instance ToPath CreateTags where
        toPath CreateTags'{..}
          = mconcat
              ["/2015-02-01/create-tags/",
               toText _ctrqFileSystemId]

instance ToQuery CreateTags where
        toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
    CreateTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTagsResponse' smart constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'
