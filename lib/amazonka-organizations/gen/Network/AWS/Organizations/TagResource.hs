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
-- Module      : Network.AWS.Organizations.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource.
--
--
-- Currently, you can attach tags to the following resources in AWS Organizations.
--
--     * AWS account
--
--     * Organization root
--
--     * Organizational unit (OU)
--
--     * Policy (any type)
--
--
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.TagResource
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

import Network.AWS.Lens
import Network.AWS.Organizations.Types
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
-- * 'trResourceId' - The ID of the resource to add a tag to.
--
-- * 'trTags' - A list of tags to add to the specified resource. You can specify any of the following taggable resources.     * AWS account – specify the account ID number.     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @      * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @      * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @  For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ .
tagResource ::
  -- | 'trResourceId'
  Text ->
  TagResource
tagResource pResourceId_ =
  TagResource' {_trResourceId = pResourceId_, _trTags = mempty}

-- | The ID of the resource to add a tag to.
trResourceId :: Lens' TagResource Text
trResourceId = lens _trResourceId (\s a -> s {_trResourceId = a})

-- | A list of tags to add to the specified resource. You can specify any of the following taggable resources.     * AWS account – specify the account ID number.     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @      * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @      * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @  For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ .
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON organizations
  response = receiveNull TagResourceResponse'

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.TagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object
      ( catMaybes
          [Just ("ResourceId" .= _trResourceId), Just ("Tags" .= _trTags)]
      )

instance ToPath TagResource where
  toPath = const "/"

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
