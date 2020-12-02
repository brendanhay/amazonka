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
-- Module      : Network.AWS.MediaLive.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create tags for a resource
module Network.AWS.MediaLive.CreateTags
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
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for CreateTagsRequest
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
-- * 'ctTags' - Undocumented member.
--
-- * 'ctResourceARN' - Undocumented member.
createTags ::
  -- | 'ctResourceARN'
  Text ->
  CreateTags
createTags pResourceARN_ =
  CreateTags' {_ctTags = Nothing, _ctResourceARN = pResourceARN_}

-- | Undocumented member.
ctTags :: Lens' CreateTags (HashMap Text (Text))
ctTags = lens _ctTags (\s a -> s {_ctTags = a}) . _Default . _Map

-- | Undocumented member.
ctResourceARN :: Lens' CreateTags Text
ctResourceARN = lens _ctResourceARN (\s a -> s {_ctResourceARN = a})

instance AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = postJSON mediaLive
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
  toPath CreateTags' {..} =
    mconcat ["/prod/tags/", toBS _ctResourceARN]

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
