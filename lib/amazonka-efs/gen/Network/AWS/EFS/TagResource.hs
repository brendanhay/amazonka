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
-- Module      : Network.AWS.EFS.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a tag for an EFS resource. You can create tags for EFS file systems and access points using this API operation.
--
--
-- This operation requires permissions for the @elasticfilesystem:TagResource@ action.
module Network.AWS.EFS.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResourceId,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceId :: !Text,
    _trTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceId' - The ID specifying the EFS resource that you want to create a tag for.
--
-- * 'trTags' -
tagResource ::
  -- | 'trResourceId'
  Text ->
  TagResource
tagResource pResourceId_ =
  TagResource' {_trResourceId = pResourceId_, _trTags = mempty}

-- | The ID specifying the EFS resource that you want to create a tag for.
trResourceId :: Lens' TagResource Text
trResourceId = lens _trResourceId (\s a -> s {_trResourceId = a})

-- |
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON efs
  response = receiveNull TagResourceResponse'

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders = const mempty

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object (catMaybes [Just ("Tags" .= _trTags)])

instance ToPath TagResource where
  toPath TagResource' {..} =
    mconcat ["/2015-02-01/resource-tags/", toBS _trResourceId]

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
