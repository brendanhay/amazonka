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
-- Module      : Network.AWS.Glue.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of tags associated with a resource.
module Network.AWS.Glue.GetTags
  ( -- * Creating a Request
    getTags,
    GetTags,

    -- * Request Lenses
    gtResourceARN,

    -- * Destructuring the Response
    getTagsResponse,
    GetTagsResponse,

    -- * Response Lenses
    gtrsTags,
    gtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTags' smart constructor.
newtype GetTags = GetTags' {_gtResourceARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtResourceARN' - The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
getTags ::
  -- | 'gtResourceARN'
  Text ->
  GetTags
getTags pResourceARN_ = GetTags' {_gtResourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
gtResourceARN :: Lens' GetTags Text
gtResourceARN = lens _gtResourceARN (\s a -> s {_gtResourceARN = a})

instance AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetTagsResponse'
            <$> (x .?> "Tags" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetTags

instance NFData GetTags

instance ToHeaders GetTags where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetTags" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTags where
  toJSON GetTags' {..} =
    object (catMaybes [Just ("ResourceArn" .= _gtResourceARN)])

instance ToPath GetTags where
  toPath = const "/"

instance ToQuery GetTags where
  toQuery = const mempty

-- | /See:/ 'getTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { _gtrsTags ::
      !(Maybe (Map Text (Text))),
    _gtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTags' - The requested tags.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTagsResponse ::
  -- | 'gtrsResponseStatus'
  Int ->
  GetTagsResponse
getTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { _gtrsTags = Nothing,
      _gtrsResponseStatus = pResponseStatus_
    }

-- | The requested tags.
gtrsTags :: Lens' GetTagsResponse (HashMap Text (Text))
gtrsTags = lens _gtrsTags (\s a -> s {_gtrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTagsResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\s a -> s {_gtrsResponseStatus = a})

instance NFData GetTagsResponse
