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
-- Module      : Network.AWS.DirectoryService.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.
--
--
module Network.AWS.DirectoryService.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceId
    , attrTags

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    -- * Response Lenses
    , attrrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceId :: !Text
  , _attrTags       :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceId' - Identifier (ID) for the directory to which to add the tag.
--
-- * 'attrTags' - The tags to be assigned to the directory.
addTagsToResource
    :: Text -- ^ 'attrResourceId'
    -> AddTagsToResource
addTagsToResource pResourceId_ =
  AddTagsToResource' {_attrResourceId = pResourceId_, _attrTags = mempty}


-- | Identifier (ID) for the directory to which to add the tag.
attrResourceId :: Lens' AddTagsToResource Text
attrResourceId = lens _attrResourceId (\ s a -> s{_attrResourceId = a})

-- | The tags to be assigned to the directory.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 AddTagsToResourceResponse' <$> (pure (fromEnum s)))

instance Hashable AddTagsToResource where

instance NFData AddTagsToResource where

instance ToHeaders AddTagsToResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.AddTagsToResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToResource where
        toJSON AddTagsToResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _attrResourceId),
                  Just ("Tags" .= _attrTags)])

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery = const mempty

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { _attrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrrsResponseStatus' - -- | The response status code.
addTagsToResourceResponse
    :: Int -- ^ 'attrrsResponseStatus'
    -> AddTagsToResourceResponse
addTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse' {_attrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
attrrsResponseStatus :: Lens' AddTagsToResourceResponse Int
attrrsResponseStatus = lens _attrrsResponseStatus (\ s a -> s{_attrrsResponseStatus = a})

instance NFData AddTagsToResourceResponse where
