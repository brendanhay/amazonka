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
-- Module      : Network.AWS.DirectoryService.RemoveTagsFromResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a directory.
--
--
module Network.AWS.DirectoryService.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrResourceId
    , rtfrTagKeys

    -- * Destructuring the Response
    , removeTagsFromResourceResponse
    , RemoveTagsFromResourceResponse
    -- * Response Lenses
    , rtfrrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { _rtfrResourceId :: !Text
  , _rtfrTagKeys    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceId' - Identifier (ID) of the directory from which to remove the tag.
--
-- * 'rtfrTagKeys' - The tag key (name) of the tag to be removed.
removeTagsFromResource
    :: Text -- ^ 'rtfrResourceId'
    -> RemoveTagsFromResource
removeTagsFromResource pResourceId_ =
  RemoveTagsFromResource'
    {_rtfrResourceId = pResourceId_, _rtfrTagKeys = mempty}


-- | Identifier (ID) of the directory from which to remove the tag.
rtfrResourceId :: Lens' RemoveTagsFromResource Text
rtfrResourceId = lens _rtfrResourceId (\ s a -> s{_rtfrResourceId = a})

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a}) . _Coerce

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveTagsFromResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveTagsFromResource where

instance NFData RemoveTagsFromResource where

instance ToHeaders RemoveTagsFromResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.RemoveTagsFromResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _rtfrResourceId),
                  Just ("TagKeys" .= _rtfrTagKeys)])

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { _rtfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrrsResponseStatus' - -- | The response status code.
removeTagsFromResourceResponse
    :: Int -- ^ 'rtfrrsResponseStatus'
    -> RemoveTagsFromResourceResponse
removeTagsFromResourceResponse pResponseStatus_ =
  RemoveTagsFromResourceResponse' {_rtfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtfrrsResponseStatus :: Lens' RemoveTagsFromResourceResponse Int
rtfrrsResponseStatus = lens _rtfrrsResponseStatus (\ s a -> s{_rtfrrsResponseStatus = a})

instance NFData RemoveTagsFromResourceResponse where
