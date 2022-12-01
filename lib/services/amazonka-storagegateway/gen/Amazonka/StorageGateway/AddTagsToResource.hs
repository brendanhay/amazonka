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
-- Module      : Amazonka.StorageGateway.AddTagsToResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource. You use tags to add
-- metadata to resources, which you can use to categorize these resources.
-- For example, you can categorize resources by purpose, owner,
-- environment, or team. Each tag consists of a key and a value, which you
-- define. You can add tags to the following Storage Gateway resources:
--
-- -   Storage gateways of all types
--
-- -   Storage volumes
--
-- -   Virtual tapes
--
-- -   NFS and SMB file shares
--
-- -   File System associations
--
-- You can create a maximum of 50 tags for each resource. Virtual tapes and
-- storage volumes that are recovered to a new gateway maintain their tags.
module Amazonka.StorageGateway.AddTagsToResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | AddTagsToResourceInput
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
    resourceARN :: Prelude.Text,
    -- | The key-value pair that represents the tag you want to add to the
    -- resource. The value can be an empty string.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AddTagsToResource
newAddTagsToResource pResourceARN_ =
  AddTagsToResource'
    { resourceARN = pResourceARN_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
addTagsToResource_resourceARN :: Lens.Lens' AddTagsToResource Prelude.Text
addTagsToResource_resourceARN = Lens.lens (\AddTagsToResource' {resourceARN} -> resourceARN) (\s@AddTagsToResource' {} a -> s {resourceARN = a} :: AddTagsToResource)

-- | The key-value pair that represents the tag you want to add to the
-- resource. The value can be an empty string.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Prelude.<$> (x Core..?> "ResourceARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTagsToResource where
  hashWithSalt _salt AddTagsToResource' {..} =
    _salt `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTagsToResource where
  rnf AddTagsToResource' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders AddTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.AddTagsToResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Core..= resourceARN),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Core.ToQuery AddTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | AddTagsToResourceOutput
--
-- /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AddTagsToResourceResponse
newAddTagsToResourceResponse pHttpStatus_ =
  AddTagsToResourceResponse'
    { resourceARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource you want to add tags to.
addTagsToResourceResponse_resourceARN :: Lens.Lens' AddTagsToResourceResponse (Prelude.Maybe Prelude.Text)
addTagsToResourceResponse_resourceARN = Lens.lens (\AddTagsToResourceResponse' {resourceARN} -> resourceARN) (\s@AddTagsToResourceResponse' {} a -> s {resourceARN = a} :: AddTagsToResourceResponse)

-- | The response's http status code.
addTagsToResourceResponse_httpStatus :: Lens.Lens' AddTagsToResourceResponse Prelude.Int
addTagsToResourceResponse_httpStatus = Lens.lens (\AddTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddTagsToResourceResponse)

instance Prelude.NFData AddTagsToResourceResponse where
  rnf AddTagsToResourceResponse' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf httpStatus
