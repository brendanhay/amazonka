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
-- Module      : Network.AWS.ResourceGroups.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a resource group with the specified ARN. Existing tags on a resource group are not changed if they are not specified in the request parameters.
--
--
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
module Network.AWS.ResourceGroups.Tag
  ( -- * Creating a Request
    tag,
    Tag,

    -- * Request Lenses
    tagARN,
    tagTags,

    -- * Destructuring the Response
    tagResponse,
    TagResponse,

    -- * Response Lenses
    tagrsARN,
    tagrsTags,
    tagrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagARN :: !Text, _tagTags :: !(Map Text (Text))}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagARN' - The ARN of the resource group to which to add tags.
--
-- * 'tagTags' - The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
tag ::
  -- | 'tagARN'
  Text ->
  Tag
tag pARN_ = Tag' {_tagARN = pARN_, _tagTags = mempty}

-- | The ARN of the resource group to which to add tags.
tagARN :: Lens' Tag Text
tagARN = lens _tagARN (\s a -> s {_tagARN = a})

-- | The tags to add to the specified resource group. A tag is a string-to-string map of key-value pairs.
tagTags :: Lens' Tag (HashMap Text (Text))
tagTags = lens _tagTags (\s a -> s {_tagTags = a}) . _Map

instance AWSRequest Tag where
  type Rs Tag = TagResponse
  request = putJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          TagResponse'
            <$> (x .?> "Arn")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable Tag

instance NFData Tag

instance ToHeaders Tag where
  toHeaders = const mempty

instance ToJSON Tag where
  toJSON Tag' {..} = object (catMaybes [Just ("Tags" .= _tagTags)])

instance ToPath Tag where
  toPath Tag' {..} = mconcat ["/resources/", toBS _tagARN, "/tags"]

instance ToQuery Tag where
  toQuery = const mempty

-- | /See:/ 'tagResponse' smart constructor.
data TagResponse = TagResponse'
  { _tagrsARN :: !(Maybe Text),
    _tagrsTags :: !(Maybe (Map Text (Text))),
    _tagrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagrsARN' - The ARN of the tagged resource.
--
-- * 'tagrsTags' - The tags that have been added to the specified resource group.
--
-- * 'tagrsResponseStatus' - -- | The response status code.
tagResponse ::
  -- | 'tagrsResponseStatus'
  Int ->
  TagResponse
tagResponse pResponseStatus_ =
  TagResponse'
    { _tagrsARN = Nothing,
      _tagrsTags = Nothing,
      _tagrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the tagged resource.
tagrsARN :: Lens' TagResponse (Maybe Text)
tagrsARN = lens _tagrsARN (\s a -> s {_tagrsARN = a})

-- | The tags that have been added to the specified resource group.
tagrsTags :: Lens' TagResponse (HashMap Text (Text))
tagrsTags = lens _tagrsTags (\s a -> s {_tagrsTags = a}) . _Default . _Map

-- | -- | The response status code.
tagrsResponseStatus :: Lens' TagResponse Int
tagrsResponseStatus = lens _tagrsResponseStatus (\s a -> s {_tagrsResponseStatus = a})

instance NFData TagResponse
