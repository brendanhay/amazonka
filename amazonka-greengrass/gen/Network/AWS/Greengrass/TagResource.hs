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
-- Module      : Network.AWS.Greengrass.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a Greengrass resource. Valid resources are \'Group\',
-- \'ConnectorDefinition\', \'CoreDefinition\', \'DeviceDefinition\',
-- \'FunctionDefinition\', \'LoggerDefinition\',
-- \'SubscriptionDefinition\', \'ResourceDefinition\', and
-- \'BulkDeployment\'.
module Network.AWS.Greengrass.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_tags,
    tagResource_resourceArn,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'tagResource_tags' - Undocumented member.
--
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Name (ARN) of the resource.
newTagResource ::
  -- | 'resourceArn'
  Core.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { tags = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | Undocumented member.
tagResource_tags :: Lens.Lens' TagResource (Core.Maybe (Core.HashMap Core.Text Core.Text))
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the resource.
tagResource_resourceArn :: Lens.Lens' TagResource Core.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Core.Hashable TagResource

instance Core.NFData TagResource

instance Core.ToHeaders TagResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TagResource where
  toJSON TagResource' {..} =
    Core.object
      (Core.catMaybes [("tags" Core..=) Core.<$> tags])

instance Core.ToPath TagResource where
  toPath TagResource' {..} =
    Core.mconcat ["/tags/", Core.toBS resourceArn]

instance Core.ToQuery TagResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Core.NFData TagResourceResponse
