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
-- Module      : Network.AWS.Lambda.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to
-- a function.
module Network.AWS.Lambda.TagResource
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

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The function\'s Amazon Resource Name (ARN).
    resource :: Prelude.Text,
    -- | A list of tags to apply to the function.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'tagResource_resource' - The function\'s Amazon Resource Name (ARN).
--
-- 'tags', 'tagResource_tags' - A list of tags to apply to the function.
newTagResource ::
  -- | 'resource'
  Prelude.Text ->
  TagResource
newTagResource pResource_ =
  TagResource'
    { resource = pResource_,
      tags = Prelude.mempty
    }

-- | The function\'s Amazon Resource Name (ARN).
tagResource_resource :: Lens.Lens' TagResource Prelude.Text
tagResource_resource = Lens.lens (\TagResource' {resource} -> resource) (\s@TagResource' {} a -> s {resource = a} :: TagResource)

-- | A list of tags to apply to the function.
tagResource_tags :: Lens.Lens' TagResource (Prelude.HashMap Prelude.Text Prelude.Text)
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens._Coerce

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Core.ToHeaders TagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON TagResource where
  toJSON TagResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Tags" Core..= tags)]
      )

instance Core.ToPath TagResource where
  toPath TagResource' {..} =
    Prelude.mconcat
      ["/2017-03-31/tags/", Core.toBS resource]

instance Core.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse
