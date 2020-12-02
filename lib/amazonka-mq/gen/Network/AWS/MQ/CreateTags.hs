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
-- Module      : Network.AWS.MQ.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a tag to a resource.
module Network.AWS.MQ.CreateTags
  ( -- * Creating a Request
    createTags,
    CreateTags,

    -- * Request Lenses
    ctTags,
    ctResourceARN,

    -- * Destructuring the Response
    createTagsResponse,
    CreateTagsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctTags ::
      !(Maybe (Map Text (Text))),
    _ctResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctTags' - The key-value pair for the resource tag.
--
-- * 'ctResourceARN' - The Amazon Resource Name (ARN) of the resource tag.
createTags ::
  -- | 'ctResourceARN'
  Text ->
  CreateTags
createTags pResourceARN_ =
  CreateTags' {_ctTags = Nothing, _ctResourceARN = pResourceARN_}

-- | The key-value pair for the resource tag.
ctTags :: Lens' CreateTags (HashMap Text (Text))
ctTags = lens _ctTags (\s a -> s {_ctTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the resource tag.
ctResourceARN :: Lens' CreateTags Text
ctResourceARN = lens _ctResourceARN (\s a -> s {_ctResourceARN = a})

instance AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = postJSON mq
  response = receiveNull CreateTagsResponse'

instance Hashable CreateTags

instance NFData CreateTags

instance ToHeaders CreateTags where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateTags where
  toJSON CreateTags' {..} =
    object (catMaybes [("tags" .=) <$> _ctTags])

instance ToPath CreateTags where
  toPath CreateTags' {..} = mconcat ["/v1/tags/", toBS _ctResourceARN]

instance ToQuery CreateTags where
  toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
createTagsResponse ::
  CreateTagsResponse
createTagsResponse = CreateTagsResponse'

instance NFData CreateTagsResponse
