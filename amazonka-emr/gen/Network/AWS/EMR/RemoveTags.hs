{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an Amazon EMR resource. Tags make it easier to
-- associate clusters in various ways, such as grouping clusters to track
-- your Amazon EMR resource allocation costs. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
--
-- The following example removes the stack tag with value Prod from a
-- cluster:
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_RemoveTags.html>
module Network.AWS.EMR.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , removeTags
    -- ** Request lenses
    , rtResourceId
    , rtTagKeys

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    -- ** Response lenses
    , rtrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input identifies a cluster and a list of tags to remove.
--
-- /See:/ 'removeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtResourceId'
--
-- * 'rtTagKeys'
data RemoveTags = RemoveTags'
    { _rtResourceId :: !Text
    , _rtTagKeys    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTags' smart constructor.
removeTags :: Text -> RemoveTags
removeTags pResourceId_ =
    RemoveTags'
    { _rtResourceId = pResourceId_
    , _rtTagKeys = mempty
    }

-- | The Amazon EMR resource identifier from which tags will be removed. This
-- value must be a cluster identifier.
rtResourceId :: Lens' RemoveTags Text
rtResourceId = lens _rtResourceId (\ s a -> s{_rtResourceId = a});

-- | A list of tag keys to remove from a resource.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a});

instance AWSRequest RemoveTags where
        type Sv RemoveTags = EMR
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON "RemoveTags"
        response
          = receiveJSON
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance ToHeaders RemoveTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.RemoveTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTags where
        toJSON RemoveTags'{..}
          = object
              ["ResourceId" .= _rtResourceId,
               "TagKeys" .= _rtTagKeys]

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | This output indicates the result of removing tags from a resource.
--
-- /See:/ 'removeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrsStatus'
newtype RemoveTagsResponse = RemoveTagsResponse'
    { _rtrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsResponse' smart constructor.
removeTagsResponse :: Int -> RemoveTagsResponse
removeTagsResponse pStatus_ =
    RemoveTagsResponse'
    { _rtrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rtrsStatus :: Lens' RemoveTagsResponse Int
rtrsStatus = lens _rtrsStatus (\ s a -> s{_rtrsStatus = a});
