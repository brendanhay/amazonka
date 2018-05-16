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
-- Module      : Network.AWS.EMR.RemoveTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
--
--
-- The following example removes the stack tag with value Prod from a cluster:
--
module Network.AWS.EMR.RemoveTags
    (
    -- * Creating a Request
      removeTags
    , RemoveTags
    -- * Request Lenses
    , rtResourceId
    , rtTagKeys

    -- * Destructuring the Response
    , removeTagsResponse
    , RemoveTagsResponse
    -- * Response Lenses
    , rtrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input identifies a cluster and a list of tags to remove.
--
--
--
-- /See:/ 'removeTags' smart constructor.
data RemoveTags = RemoveTags'
  { _rtResourceId :: !Text
  , _rtTagKeys    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtResourceId' - The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
--
-- * 'rtTagKeys' - A list of tag keys to remove from a resource.
removeTags
    :: Text -- ^ 'rtResourceId'
    -> RemoveTags
removeTags pResourceId_ =
  RemoveTags' {_rtResourceId = pResourceId_, _rtTagKeys = mempty}


-- | The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
rtResourceId :: Lens' RemoveTags Text
rtResourceId = lens _rtResourceId (\ s a -> s{_rtResourceId = a})

-- | A list of tag keys to remove from a resource.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a}) . _Coerce

instance AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON emr
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance Hashable RemoveTags where

instance NFData RemoveTags where

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
              (catMaybes
                 [Just ("ResourceId" .= _rtResourceId),
                  Just ("TagKeys" .= _rtTagKeys)])

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | This output indicates the result of removing tags from a resource.
--
--
--
-- /See:/ 'removeTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { _rtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsResponseStatus' - -- | The response status code.
removeTagsResponse
    :: Int -- ^ 'rtrsResponseStatus'
    -> RemoveTagsResponse
removeTagsResponse pResponseStatus_ =
  RemoveTagsResponse' {_rtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtrsResponseStatus :: Lens' RemoveTagsResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a})

instance NFData RemoveTagsResponse where
