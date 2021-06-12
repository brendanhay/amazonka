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
-- Module      : Network.AWS.MQ.CreateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a tag to a resource.
module Network.AWS.MQ.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_tags,
    createTags_resourceArn,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The key-value pair for the resource tag.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon Resource Name (ARN) of the resource tag.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTags_tags' - The key-value pair for the resource tag.
--
-- 'resourceArn', 'createTags_resourceArn' - The Amazon Resource Name (ARN) of the resource tag.
newCreateTags ::
  -- | 'resourceArn'
  Core.Text ->
  CreateTags
newCreateTags pResourceArn_ =
  CreateTags'
    { tags = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | The key-value pair for the resource tag.
createTags_tags :: Lens.Lens' CreateTags (Core.Maybe (Core.HashMap Core.Text Core.Text))
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the resource tag.
createTags_resourceArn :: Lens.Lens' CreateTags Core.Text
createTags_resourceArn = Lens.lens (\CreateTags' {resourceArn} -> resourceArn) (\s@CreateTags' {} a -> s {resourceArn = a} :: CreateTags)

instance Core.AWSRequest CreateTags where
  type AWSResponse CreateTags = CreateTagsResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull CreateTagsResponse'

instance Core.Hashable CreateTags

instance Core.NFData CreateTags

instance Core.ToHeaders CreateTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Core.object
      (Core.catMaybes [("tags" Core..=) Core.<$> tags])

instance Core.ToPath CreateTags where
  toPath CreateTags' {..} =
    Core.mconcat ["/v1/tags/", Core.toBS resourceArn]

instance Core.ToQuery CreateTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTagsResponse ::
  CreateTagsResponse
newCreateTagsResponse = CreateTagsResponse'

instance Core.NFData CreateTagsResponse
