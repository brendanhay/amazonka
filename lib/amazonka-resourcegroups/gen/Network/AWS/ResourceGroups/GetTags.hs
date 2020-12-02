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
-- Module      : Network.AWS.ResourceGroups.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are associated with a resource group, specified by an ARN.
module Network.AWS.ResourceGroups.GetTags
  ( -- * Creating a Request
    getTags,
    GetTags,

    -- * Request Lenses
    gtARN,

    -- * Destructuring the Response
    getTagsResponse,
    GetTagsResponse,

    -- * Response Lenses
    gtrsARN,
    gtrsTags,
    gtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'getTags' smart constructor.
newtype GetTags = GetTags' {_gtARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtARN' - The ARN of the resource group whose tags you want to retrieve.
getTags ::
  -- | 'gtARN'
  Text ->
  GetTags
getTags pARN_ = GetTags' {_gtARN = pARN_}

-- | The ARN of the resource group whose tags you want to retrieve.
gtARN :: Lens' GetTags Text
gtARN = lens _gtARN (\s a -> s {_gtARN = a})

instance AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = get resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          GetTagsResponse'
            <$> (x .?> "Arn")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetTags

instance NFData GetTags

instance ToHeaders GetTags where
  toHeaders = const mempty

instance ToPath GetTags where
  toPath GetTags' {..} = mconcat ["/resources/", toBS _gtARN, "/tags"]

instance ToQuery GetTags where
  toQuery = const mempty

-- | /See:/ 'getTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { _gtrsARN :: !(Maybe Text),
    _gtrsTags :: !(Maybe (Map Text (Text))),
    _gtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsARN' - The ARN of the tagged resource group.
--
-- * 'gtrsTags' - The tags associated with the specified resource group.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTagsResponse ::
  -- | 'gtrsResponseStatus'
  Int ->
  GetTagsResponse
getTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { _gtrsARN = Nothing,
      _gtrsTags = Nothing,
      _gtrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the tagged resource group.
gtrsARN :: Lens' GetTagsResponse (Maybe Text)
gtrsARN = lens _gtrsARN (\s a -> s {_gtrsARN = a})

-- | The tags associated with the specified resource group.
gtrsTags :: Lens' GetTagsResponse (HashMap Text (Text))
gtrsTags = lens _gtrsTags (\s a -> s {_gtrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTagsResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\s a -> s {_gtrsResponseStatus = a})

instance NFData GetTagsResponse
