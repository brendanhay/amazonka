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
-- Module      : Network.AWS.IoTAnalytics.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds to or modifies the tags of the given resource. Tags are metadata that can be used to manage a resource.
module Network.AWS.IoTAnalytics.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResourceARN,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,

    -- * Response Lenses
    trrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text,
    _trTags :: !(List1 Tag)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The ARN of the resource whose tags you want to modify.
--
-- * 'trTags' - The new or modified tags for the resource.
tagResource ::
  -- | 'trResourceARN'
  Text ->
  -- | 'trTags'
  NonEmpty Tag ->
  TagResource
tagResource pResourceARN_ pTags_ =
  TagResource'
    { _trResourceARN = pResourceARN_,
      _trTags = _List1 # pTags_
    }

-- | The ARN of the resource whose tags you want to modify.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

-- | The new or modified tags for the resource.
trTags :: Lens' TagResource (NonEmpty Tag)
trTags = lens _trTags (\s a -> s {_trTags = a}) . _List1

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON ioTAnalytics
  response =
    receiveEmpty
      (\s h x -> TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders = const mempty

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object (catMaybes [Just ("tags" .= _trTags)])

instance ToPath TagResource where
  toPath = const "/tags"

instance ToQuery TagResource where
  toQuery TagResource' {..} =
    mconcat ["resourceArn" =: _trResourceARN]

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
