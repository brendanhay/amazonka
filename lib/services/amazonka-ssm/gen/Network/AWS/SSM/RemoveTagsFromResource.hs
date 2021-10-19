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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The type of resource from which you want to remove a tag.
    --
    -- The @ManagedInstance@ type for this API operation is only for
    -- on-premises managed instances. Specify the name of the managed instance
    -- in the following format: @mi-ID_number @. For example,
    -- @mi-1a2b3c4d5e6f@.
    resourceType :: ResourceTypeForTagging,
    -- | The ID of the resource from which you want to remove tags. For example:
    --
    -- ManagedInstance: mi-012345abcde
    --
    -- MaintenanceWindow: mw-012345abcde
    --
    -- PatchBaseline: pb-012345abcde
    --
    -- OpsMetadata object: @ResourceID@ for tagging is created from the Amazon
    -- Resource Name (ARN) for the object. Specifically, @ResourceID@ is
    -- created from the strings that come after the word @opsmetadata@ in the
    -- ARN. For example, an OpsMetadata object with an ARN of
    -- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
    -- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
    -- @\/aws\/ssm\/MyGroup\/appmanager@.
    --
    -- For the Document and Parameter values, use the name of the resource.
    --
    -- The ManagedInstance type for this API operation is only for on-premises
    -- managed instances. Specify the name of the managed instance in the
    -- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
    resourceId :: Prelude.Text,
    -- | Tag keys that you want to remove from the specified resource.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed instances. Specify the name of the managed instance
-- in the following format: @mi-ID_number @. For example,
-- @mi-1a2b3c4d5e6f@.
--
-- 'resourceId', 'removeTagsFromResource_resourceId' - The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
--
-- MaintenanceWindow: mw-012345abcde
--
-- PatchBaseline: pb-012345abcde
--
-- OpsMetadata object: @ResourceID@ for tagging is created from the Amazon
-- Resource Name (ARN) for the object. Specifically, @ResourceID@ is
-- created from the strings that come after the word @opsmetadata@ in the
-- ARN. For example, an OpsMetadata object with an ARN of
-- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
-- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
-- @\/aws\/ssm\/MyGroup\/appmanager@.
--
-- For the Document and Parameter values, use the name of the resource.
--
-- The ManagedInstance type for this API operation is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - Tag keys that you want to remove from the specified resource.
newRemoveTagsFromResource ::
  -- | 'resourceType'
  ResourceTypeForTagging ->
  -- | 'resourceId'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceType_ pResourceId_ =
  RemoveTagsFromResource'
    { resourceType =
        pResourceType_,
      resourceId = pResourceId_,
      tagKeys = Prelude.mempty
    }

-- | The type of resource from which you want to remove a tag.
--
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed instances. Specify the name of the managed instance
-- in the following format: @mi-ID_number @. For example,
-- @mi-1a2b3c4d5e6f@.
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
-- OpsMetadata object: @ResourceID@ for tagging is created from the Amazon
-- Resource Name (ARN) for the object. Specifically, @ResourceID@ is
-- created from the strings that come after the word @opsmetadata@ in the
-- ARN. For example, an OpsMetadata object with an ARN of
-- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
-- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
-- @\/aws\/ssm\/MyGroup\/appmanager@.
--
-- For the Document and Parameter values, use the name of the resource.
--
-- The ManagedInstance type for this API operation is only for on-premises
-- managed instances. Specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
removeTagsFromResource_resourceId :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceId = Lens.lens (\RemoveTagsFromResource' {resourceId} -> resourceId) (\s@RemoveTagsFromResource' {} a -> s {resourceId = a} :: RemoveTagsFromResource)

-- | Tag keys that you want to remove from the specified resource.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTagsFromResource

instance Prelude.NFData RemoveTagsFromResource

instance Core.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.RemoveTagsFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Core..= resourceType),
            Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse pHttpStatus_ =
  RemoveTagsFromResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
