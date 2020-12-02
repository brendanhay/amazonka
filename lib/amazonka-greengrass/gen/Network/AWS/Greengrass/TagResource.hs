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
-- Module      : Network.AWS.Greengrass.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a Greengrass resource. Valid resources are 'Group', 'ConnectorDefinition', 'CoreDefinition', 'DeviceDefinition', 'FunctionDefinition', 'LoggerDefinition', 'SubscriptionDefinition', 'ResourceDefinition', and 'BulkDeployment'.
module Network.AWS.Greengrass.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trTags,
    trResourceARN,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trTags ::
      !(Maybe (Map Text (Text))),
    _trResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trTags' - Undocumented member.
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the resource.
tagResource ::
  -- | 'trResourceARN'
  Text ->
  TagResource
tagResource pResourceARN_ =
  TagResource' {_trTags = Nothing, _trResourceARN = pResourceARN_}

-- | Undocumented member.
trTags :: Lens' TagResource (HashMap Text (Text))
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the resource.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON greengrass
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
    object (catMaybes [("tags" .=) <$> _trTags])

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
