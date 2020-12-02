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
-- Module      : Network.AWS.ELBv2.RemoveTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified Elastic Load Balancing resource.
--
--
-- To list the current tags for your resources, use 'DescribeTags' .
--
module Network.AWS.ELBv2.RemoveTags
    (
    -- * Creating a Request
      removeTags
    , RemoveTags
    -- * Request Lenses
    , rtResourceARNs
    , rtTagKeys

    -- * Destructuring the Response
    , removeTagsResponse
    , RemoveTagsResponse
    -- * Response Lenses
    , rtrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeTags' smart constructor.
data RemoveTags = RemoveTags'
  { _rtResourceARNs :: ![Text]
  , _rtTagKeys      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtResourceARNs' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'rtTagKeys' - The tag keys for the tags to remove.
removeTags
    :: RemoveTags
removeTags = RemoveTags' {_rtResourceARNs = mempty, _rtTagKeys = mempty}


-- | The Amazon Resource Name (ARN) of the resource.
rtResourceARNs :: Lens' RemoveTags [Text]
rtResourceARNs = lens _rtResourceARNs (\ s a -> s{_rtResourceARNs = a}) . _Coerce

-- | The tag keys for the tags to remove.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a}) . _Coerce

instance AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "RemoveTagsResult"
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance Hashable RemoveTags where

instance NFData RemoveTags where

instance ToHeaders RemoveTags where
        toHeaders = const mempty

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery RemoveTags'{..}
          = mconcat
              ["Action" =: ("RemoveTags" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ResourceArns" =:
                 toQueryList "member" _rtResourceARNs,
               "TagKeys" =: toQueryList "member" _rtTagKeys]

-- | /See:/ 'removeTagsResponse' smart constructor.
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
