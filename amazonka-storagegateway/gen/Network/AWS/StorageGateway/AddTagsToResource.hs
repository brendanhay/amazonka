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
-- Module      : Network.AWS.StorageGateway.AddTagsToResource
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:
--
-- -   Storage gateways of all types
--
-- -   Storage Volumes
--
-- -   Virtual Tapes
--
-- You can create a maximum of 10 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.
module Network.AWS.StorageGateway.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceARN
    , attrTags

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    -- * Response Lenses
    , attrrsResourceARN
    , attrrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | AddTagsToResourceInput
--
-- /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
    { _attrResourceARN :: !Text
    , _attrTags        :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceARN'
--
-- * 'attrTags'
addTagsToResource
    :: Text -- ^ 'attrResourceARN'
    -> AddTagsToResource
addTagsToResource pResourceARN_ =
    AddTagsToResource'
    { _attrResourceARN = pResourceARN_
    , _attrTags = mempty
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
attrResourceARN :: Lens' AddTagsToResource Text
attrResourceARN = lens _attrResourceARN (\ s a -> s{_attrResourceARN = a});

-- | The key-value pair that represents the tag you want to add to the resource. The value can be an empty string.
--
-- Valid characters for key and value are letters, spaces, and numbers representable in UTF-8 format, and the following special characters: + - = . _ : \/ \'.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce;

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 AddTagsToResourceResponse' <$>
                   (x .?> "ResourceARN") <*> (pure (fromEnum s)))

instance Hashable AddTagsToResource

instance NFData AddTagsToResource

instance ToHeaders AddTagsToResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.AddTagsToResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToResource where
        toJSON AddTagsToResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _attrResourceARN),
                  Just ("Tags" .= _attrTags)])

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery = const mempty

-- | AddTagsToResourceOutput
--
-- /See:/ 'addTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
    { _attrrsResourceARN    :: !(Maybe Text)
    , _attrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrrsResourceARN'
--
-- * 'attrrsResponseStatus'
addTagsToResourceResponse
    :: Int -- ^ 'attrrsResponseStatus'
    -> AddTagsToResourceResponse
addTagsToResourceResponse pResponseStatus_ =
    AddTagsToResourceResponse'
    { _attrrsResourceARN = Nothing
    , _attrrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
attrrsResourceARN :: Lens' AddTagsToResourceResponse (Maybe Text)
attrrsResourceARN = lens _attrrsResourceARN (\ s a -> s{_attrrsResourceARN = a});

-- | The response status code.
attrrsResponseStatus :: Lens' AddTagsToResourceResponse Int
attrrsResponseStatus = lens _attrrsResponseStatus (\ s a -> s{_attrrsResponseStatus = a});

instance NFData AddTagsToResourceResponse
