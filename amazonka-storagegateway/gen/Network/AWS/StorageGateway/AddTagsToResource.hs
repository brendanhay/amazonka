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
-- Module      : Network.AWS.StorageGateway.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource. You use tags to add
-- metadata to resources, which you can use to categorize these resources.
-- For example, you can categorize resources by purpose, owner,
-- environment, or team. Each tag consists of a key and a value, which you
-- define. You can add tags to the following AWS Storage Gateway resources:
--
-- -   Storage gateways of all types
--
-- -   Storage volumes
--
-- -   Virtual tapes
--
-- -   NFS and SMB file shares
--
-- You can create a maximum of 50 tags for each resource. Virtual tapes and
-- storage volumes that are recovered to a new gateway maintain their tags.
module Network.AWS.StorageGateway.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceARN,
    addTagsToResource_tags,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,

    -- * Response Lenses
    addTagsToResourceResponse_resourceARN,
    addTagsToResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | AddTagsToResourceInput
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
    resourceARN :: Core.Text,
    -- | The key-value pair that represents the tag you want to add to the
    -- resource. The value can be an empty string.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'addTagsToResource_resourceARN' - The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- 'tags', 'addTagsToResource_tags' - The key-value pair that represents the tag you want to add to the
-- resource. The value can be an empty string.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
newAddTagsToResource ::
  -- | 'resourceARN'
  Core.Text ->
  AddTagsToResource
newAddTagsToResource pResourceARN_ =
  AddTagsToResource'
    { resourceARN = pResourceARN_,
      tags = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
addTagsToResource_resourceARN :: Lens.Lens' AddTagsToResource Core.Text
addTagsToResource_resourceARN = Lens.lens (\AddTagsToResource' {resourceARN} -> resourceARN) (\s@AddTagsToResource' {} a -> s {resourceARN = a} :: AddTagsToResource)

-- | The key-value pair that represents the tag you want to add to the
-- resource. The value can be an empty string.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Core.<$> (x Core..?> "ResourceARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddTagsToResource

instance Core.NFData AddTagsToResource

instance Core.ToHeaders AddTagsToResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.AddTagsToResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath AddTagsToResource where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToResource where
  toQuery = Core.const Core.mempty

-- | AddTagsToResourceOutput
--
-- /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
    resourceARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'addTagsToResourceResponse_resourceARN' - The Amazon Resource Name (ARN) of the resource you want to add tags to.
--
-- 'httpStatus', 'addTagsToResourceResponse_httpStatus' - The response's http status code.
newAddTagsToResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddTagsToResourceResponse
newAddTagsToResourceResponse pHttpStatus_ =
  AddTagsToResourceResponse'
    { resourceARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
addTagsToResourceResponse_resourceARN :: Lens.Lens' AddTagsToResourceResponse (Core.Maybe Core.Text)
addTagsToResourceResponse_resourceARN = Lens.lens (\AddTagsToResourceResponse' {resourceARN} -> resourceARN) (\s@AddTagsToResourceResponse' {} a -> s {resourceARN = a} :: AddTagsToResourceResponse)

-- | The response's http status code.
addTagsToResourceResponse_httpStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
addTagsToResourceResponse_httpStatus = Lens.lens (\AddTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddTagsToResourceResponse)

instance Core.NFData AddTagsToResourceResponse
