{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to the specified AWS Elemental MediaStore container. Tags are key:value pairs that you can associate with AWS resources. For example, the tag key might be "customer" and the tag value might be "companyA." You can specify one or more tags to add to each container. You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
module Network.AWS.MediaStore.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResource,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,

    -- * Response Lenses
    trrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResource :: !Text,
    _trTags :: !(List1 Tag)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResource' - The Amazon Resource Name (ARN) for the container.
--
-- * 'trTags' - An array of key:value pairs that you want to add to the container. You need to specify only the tags that you want to add or update. For example, suppose a container already has two tags (customer:CompanyA and priority:High). You want to change the priority tag and also add a third tag (type:Contract). For TagResource, you specify the following tags: priority:Medium, type:Contract. The result is that your container has three tags: customer:CompanyA, priority:Medium, and type:Contract.
tagResource ::
  -- | 'trResource'
  Text ->
  -- | 'trTags'
  NonEmpty Tag ->
  TagResource
tagResource pResource_ pTags_ =
  TagResource' {_trResource = pResource_, _trTags = _List1 # pTags_}

-- | The Amazon Resource Name (ARN) for the container.
trResource :: Lens' TagResource Text
trResource = lens _trResource (\s a -> s {_trResource = a})

-- | An array of key:value pairs that you want to add to the container. You need to specify only the tags that you want to add or update. For example, suppose a container already has two tags (customer:CompanyA and priority:High). You want to change the priority tag and also add a third tag (type:Contract). For TagResource, you specify the following tags: priority:Medium, type:Contract. The result is that your container has three tags: customer:CompanyA, priority:Medium, and type:Contract.
trTags :: Lens' TagResource (NonEmpty Tag)
trTags = lens _trTags (\s a -> s {_trTags = a}) . _List1

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON mediaStore
  response =
    receiveEmpty
      (\s h x -> TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("MediaStore_20170901.TagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object
      ( catMaybes
          [Just ("Resource" .= _trResource), Just ("Tags" .= _trTags)]
      )

instance ToPath TagResource where
  toPath = const "/"

instance ToQuery TagResource where
  toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { _trrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse ::
  -- | 'trrsResponseStatus'
  Int ->
  TagResourceResponse
tagResourceResponse pResponseStatus_ =
  TagResourceResponse' {_trrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\s a -> s {_trrsResponseStatus = a})

instance NFData TagResourceResponse
