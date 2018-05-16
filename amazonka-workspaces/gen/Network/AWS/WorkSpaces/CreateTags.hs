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
-- Module      : Network.AWS.WorkSpaces.CreateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified tags for the specified WorkSpace.
--
--
module Network.AWS.WorkSpaces.CreateTags
    (
    -- * Creating a Request
      createTags
    , CreateTags
    -- * Request Lenses
    , ctResourceId
    , ctTags

    -- * Destructuring the Response
    , createTagsResponse
    , CreateTagsResponse
    -- * Response Lenses
    , ctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctResourceId :: !Text
  , _ctTags       :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctResourceId' - The ID of the WorkSpace. To find this ID, use 'DescribeWorkspaces' .
--
-- * 'ctTags' - The tags. Each WorkSpace can have a maximum of 50 tags.
createTags
    :: Text -- ^ 'ctResourceId'
    -> CreateTags
createTags pResourceId_ =
  CreateTags' {_ctResourceId = pResourceId_, _ctTags = mempty}


-- | The ID of the WorkSpace. To find this ID, use 'DescribeWorkspaces' .
ctResourceId :: Lens' CreateTags Text
ctResourceId = lens _ctResourceId (\ s a -> s{_ctResourceId = a})

-- | The tags. Each WorkSpace can have a maximum of 50 tags.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Coerce

instance AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 CreateTagsResponse' <$> (pure (fromEnum s)))

instance Hashable CreateTags where

instance NFData CreateTags where

instance ToHeaders CreateTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.CreateTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTags where
        toJSON CreateTags'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _ctResourceId),
                  Just ("Tags" .= _ctTags)])

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { _ctrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTagsResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTagsResponse
createTagsResponse pResponseStatus_ =
  CreateTagsResponse' {_ctrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTagsResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTagsResponse where
