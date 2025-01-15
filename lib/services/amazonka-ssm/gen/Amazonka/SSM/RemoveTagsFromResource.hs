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
-- Module      : Amazonka.SSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tag keys from the specified resource.
module Amazonka.SSM.RemoveTagsFromResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The type of resource from which you want to remove a tag.
    --
    -- The @ManagedInstance@ type for this API operation is only for
    -- on-premises managed nodes. Specify the name of the managed node in the
    -- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
    resourceType :: ResourceTypeForTagging,
    -- | The ID of the resource from which you want to remove tags. For example:
    --
    -- ManagedInstance: mi-012345abcde
    --
    -- MaintenanceWindow: mw-012345abcde
    --
    -- @Automation@: @example-c160-4567-8519-012345abcde@
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
    -- The @ManagedInstance@ type for this API operation is only for
    -- on-premises managed nodes. Specify the name of the managed node in the
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
-- on-premises managed nodes. Specify the name of the managed node in the
-- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
--
-- 'resourceId', 'removeTagsFromResource_resourceId' - The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
--
-- MaintenanceWindow: mw-012345abcde
--
-- @Automation@: @example-c160-4567-8519-012345abcde@
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
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed nodes. Specify the name of the managed node in the
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
-- on-premises managed nodes. Specify the name of the managed node in the
-- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
removeTagsFromResource_resourceType :: Lens.Lens' RemoveTagsFromResource ResourceTypeForTagging
removeTagsFromResource_resourceType = Lens.lens (\RemoveTagsFromResource' {resourceType} -> resourceType) (\s@RemoveTagsFromResource' {} a -> s {resourceType = a} :: RemoveTagsFromResource)

-- | The ID of the resource from which you want to remove tags. For example:
--
-- ManagedInstance: mi-012345abcde
--
-- MaintenanceWindow: mw-012345abcde
--
-- @Automation@: @example-c160-4567-8519-012345abcde@
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
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed nodes. Specify the name of the managed node in the
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTagsFromResource where
  hashWithSalt _salt RemoveTagsFromResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromResource where
  rnf RemoveTagsFromResource' {..} =
    Prelude.rnf resourceType `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.RemoveTagsFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromResource where
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
  where
  rnf RemoveTagsFromResourceResponse' {..} =
    Prelude.rnf httpStatus
