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
-- Module      : Network.AWS.SSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all tags from the specified resource.
--
--
module Network.AWS.SSM.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrResourceType
    , rtfrResourceId
    , rtfrTagKeys

    -- * Destructuring the Response
    , removeTagsFromResourceResponse
    , RemoveTagsFromResourceResponse
    -- * Response Lenses
    , rtfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { _rtfrResourceType :: !ResourceTypeForTagging
  , _rtfrResourceId   :: !Text
  , _rtfrTagKeys      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceType' - The type of resource of which you want to remove a tag.
--
-- * 'rtfrResourceId' - The resource ID for which you want to remove tags. Use the ID of the resource. Here are some examples: ManagedInstance: mi-012345abcde MaintenanceWindow: mw-012345abcde PatchBaseline: pb-012345abcde For the Document and Parameter values, use the name of the resource.
--
-- * 'rtfrTagKeys' - Tag keys that you want to remove from the specified resource.
removeTagsFromResource
    :: ResourceTypeForTagging -- ^ 'rtfrResourceType'
    -> Text -- ^ 'rtfrResourceId'
    -> RemoveTagsFromResource
removeTagsFromResource pResourceType_ pResourceId_ =
  RemoveTagsFromResource'
    { _rtfrResourceType = pResourceType_
    , _rtfrResourceId = pResourceId_
    , _rtfrTagKeys = mempty
    }


-- | The type of resource of which you want to remove a tag.
rtfrResourceType :: Lens' RemoveTagsFromResource ResourceTypeForTagging
rtfrResourceType = lens _rtfrResourceType (\ s a -> s{_rtfrResourceType = a})

-- | The resource ID for which you want to remove tags. Use the ID of the resource. Here are some examples: ManagedInstance: mi-012345abcde MaintenanceWindow: mw-012345abcde PatchBaseline: pb-012345abcde For the Document and Parameter values, use the name of the resource.
rtfrResourceId :: Lens' RemoveTagsFromResource Text
rtfrResourceId = lens _rtfrResourceId (\ s a -> s{_rtfrResourceId = a})

-- | Tag keys that you want to remove from the specified resource.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a}) . _Coerce

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = postJSON ssm
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
                    ("AmazonSSM.RemoveTagsFromResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceType" .= _rtfrResourceType),
                  Just ("ResourceId" .= _rtfrResourceId),
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
