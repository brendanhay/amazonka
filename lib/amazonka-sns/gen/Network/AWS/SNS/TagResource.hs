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
-- Module      : Network.AWS.SNS.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add tags to the specified Amazon SNS topic. For an overview, see <https://docs.aws.amazon.com/sns/latest/dg/sns-tags.html Amazon SNS Tags> in the /Amazon SNS Developer Guide/ .
--
--
-- When you use topic tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a topic isn't recommended.
--
--     * Tags don't have any semantic meaning. Amazon SNS interprets tags as character strings.
--
--     * Tags are case-sensitive.
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--     * Tagging actions are limited to 10 TPS per AWS account, per AWS region. If your application requires a higher throughput, file a <https://console.aws.amazon.com/support/home#/case/create?issueType=technical technical support request> .
module Network.AWS.SNS.TagResource
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text,
    _trTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The ARN of the topic to which to add tags.
--
-- * 'trTags' - The tags to be added to the specified topic. A tag consists of a required key and an optional value.
tagResource ::
  -- | 'trResourceARN'
  Text ->
  TagResource
tagResource pResourceARN_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTags = mempty}

-- | The ARN of the topic to which to add tags.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

-- | The tags to be added to the specified topic. A tag consists of a required key and an optional value.
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postQuery sns
  response =
    receiveXMLWrapper
      "TagResourceResult"
      (\s h x -> TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders = const mempty

instance ToPath TagResource where
  toPath = const "/"

instance ToQuery TagResource where
  toQuery TagResource' {..} =
    mconcat
      [ "Action" =: ("TagResource" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "ResourceArn" =: _trResourceARN,
        "Tags" =: toQueryList "member" _trTags
      ]

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
