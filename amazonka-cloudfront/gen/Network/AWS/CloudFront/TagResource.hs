{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add tags to a CloudFront resource.
module Network.AWS.CloudFront.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resource,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to add tags to a CloudFront resource.
--
-- /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Prelude.Text,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'tagResource_resource' - An ARN of a CloudFront resource.
--
-- 'tags', 'tagResource_tags' - A complex type that contains zero or more @Tag@ elements.
newTagResource ::
  -- | 'resource'
  Prelude.Text ->
  -- | 'tags'
  Tags ->
  TagResource
newTagResource pResource_ pTags_ =
  TagResource' {resource = pResource_, tags = pTags_}

-- | An ARN of a CloudFront resource.
tagResource_resource :: Lens.Lens' TagResource Prelude.Text
tagResource_resource = Lens.lens (\TagResource' {resource} -> resource) (\s@TagResource' {} a -> s {resource = a} :: TagResource)

-- | A complex type that contains zero or more @Tag@ elements.
tagResource_tags :: Lens.Lens' TagResource Tags
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource)

instance Prelude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Request.postXML defaultService
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Prelude.ToElement TagResource where
  toElement TagResource' {..} =
    Prelude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}Tags"
      tags

instance Prelude.ToHeaders TagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath TagResource where
  toPath = Prelude.const "/2020-05-31/tagging"

instance Prelude.ToQuery TagResource where
  toQuery TagResource' {..} =
    Prelude.mconcat
      ["Resource" Prelude.=: resource, "Operation=Tag"]

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse
