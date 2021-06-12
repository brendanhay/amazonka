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
-- Module      : Network.AWS.SSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tag keys from the specified resource.
module Network.AWS.SSM.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceType,
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,

    -- * Response Lenses
    removeTagsFromResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The type of resource from which you want to remove a tag.
    --
    -- The ManagedInstance type for this API action is only for on-premises
    -- managed instances. Specify the name of the managed instance in the
    -- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
    resourceType :: ResourceTypeForTagging,
    -- | The ID of the resource from which you want to remove tags. For example:
    --
    -- ManagedInstance: mi-012345abcde
    --
    -- MaintenanceWindow: mw-012345abcde
    --
    -- PatchBaseline: pb-012345abcde
    --
    -- For the Document and Parameter values, use the name of the resource.
    --
    -- The ManagedInstance type for this API action is only for on-premises
    -- managed instances. Specify the name of the managed instance in the
    -- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
    resourceId :: Core.Text,
    -- | Tag keys that you want to remove from the specified resource.
    tagKeys :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'removeTagsFromResource_resourceType' - The type of resource from which you want to remove a tag.
--
-- The ManagedInstance type for this API action is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
--
-- 'resourceId', 'removeTagsFromResource_resourceId' - The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
--
-- MaintenanceWindow: mw-012345abcde
--
-- PatchBaseline: pb-012345abcde
--
-- For the Document and Parameter values, use the name of the resource.
--
-- The ManagedInstance type for this API action is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - Tag keys that you want to remove from the specified resource.
newRemoveTagsFromResource ::
  -- | 'resourceType'
  ResourceTypeForTagging ->
  -- | 'resourceId'
  Core.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceType_ pResourceId_ =
  RemoveTagsFromResource'
    { resourceType =
        pResourceType_,
      resourceId = pResourceId_,
      tagKeys = Core.mempty
    }

-- | The type of resource from which you want to remove a tag.
--
-- The ManagedInstance type for this API action is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
removeTagsFromResource_resourceType :: Lens.Lens' RemoveTagsFromResource ResourceTypeForTagging
removeTagsFromResource_resourceType = Lens.lens (\RemoveTagsFromResource' {resourceType} -> resourceType) (\s@RemoveTagsFromResource' {} a -> s {resourceType = a} :: RemoveTagsFromResource)

-- | The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
--
-- MaintenanceWindow: mw-012345abcde
--
-- PatchBaseline: pb-012345abcde
--
-- For the Document and Parameter values, use the name of the resource.
--
-- The ManagedInstance type for this API action is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
removeTagsFromResource_resourceId :: Lens.Lens' RemoveTagsFromResource Core.Text
removeTagsFromResource_resourceId = Lens.lens (\RemoveTagsFromResource' {resourceId} -> resourceId) (\s@RemoveTagsFromResource' {} a -> s {resourceId = a} :: RemoveTagsFromResource)

-- | Tag keys that you want to remove from the specified resource.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Core.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Core.. Lens._Coerce

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveTagsFromResource

instance Core.NFData RemoveTagsFromResource

instance Core.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.RemoveTagsFromResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath RemoveTagsFromResource where
  toPath = Core.const "/"

instance Core.ToQuery RemoveTagsFromResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsFromResourceResponse_httpStatus' - The response's http status code.
newRemoveTagsFromResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse pHttpStatus_ =
  RemoveTagsFromResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

instance Core.NFData RemoveTagsFromResourceResponse
