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
-- Module      : Network.AWS.Connect.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified resource.
--
--
-- The supported resource types are users, routing profiles, and contact flows.
--
-- For sample policies that use tags, see <https://docs.aws.amazon.com/connect/latest/adminguide/security_iam_id-based-policy-examples.html Amazon Connect Identity-Based Policy Examples> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResourceARN,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text,
    _trTags :: !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'trTags' - One or more tags. For example, { "tags": {"key1":"value1", "key2":"value2"} }.
tagResource ::
  -- | 'trResourceARN'
  Text ->
  TagResource
tagResource pResourceARN_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTags = mempty}

-- | The Amazon Resource Name (ARN) of the resource.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

-- | One or more tags. For example, { "tags": {"key1":"value1", "key2":"value2"} }.
trTags :: Lens' TagResource (HashMap Text (Text))
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Map

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON connect
  response = receiveNull TagResourceResponse'

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object (catMaybes [Just ("tags" .= _trTags)])

instance ToPath TagResource where
  toPath TagResource' {..} = mconcat ["/tags/", toBS _trResourceARN]

instance ToQuery TagResource where
  toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
tagResourceResponse ::
  TagResourceResponse
tagResourceResponse = TagResourceResponse'

instance NFData TagResourceResponse
