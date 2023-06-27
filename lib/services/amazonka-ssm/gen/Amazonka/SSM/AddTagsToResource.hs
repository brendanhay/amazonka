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
-- Module      : Amazonka.SSM.AddTagsToResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified resource. /Tags/
-- are metadata that you can assign to your automations, documents, managed
-- nodes, maintenance windows, Parameter Store parameters, and patch
-- baselines. Tags enable you to categorize your resources in different
-- ways, for example, by purpose, owner, or environment. Each tag consists
-- of a key and an optional value, both of which you define. For example,
-- you could define a set of tags for your account\'s managed nodes that
-- helps you track each node\'s owner and stack level. For example:
--
-- -   @Key=Owner,Value=DbAdmin@
--
-- -   @Key=Owner,Value=SysAdmin@
--
-- -   @Key=Owner,Value=Dev@
--
-- -   @Key=Stack,Value=Production@
--
-- -   @Key=Stack,Value=Pre-Production@
--
-- -   @Key=Stack,Value=Test@
--
-- Most resources can have a maximum of 50 tags. Automations can have a
-- maximum of 5 tags.
--
-- We recommend that you devise a set of tag keys that meets your needs for
-- each resource type. Using a consistent set of tag keys makes it easier
-- for you to manage your resources. You can search and filter the
-- resources based on the tags you add. Tags don\'t have any semantic
-- meaning to and are interpreted strictly as a string of characters.
--
-- For more information about using tags with Amazon Elastic Compute Cloud
-- (Amazon EC2) instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon EC2 User Guide/.
module Amazonka.SSM.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceType,
    addTagsToResource_resourceId,
    addTagsToResource_tags,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,

    -- * Response Lenses
    addTagsToResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | Specifies the type of resource you are tagging.
    --
    -- The @ManagedInstance@ type for this API operation is for on-premises
    -- managed nodes. You must specify the name of the managed node in the
    -- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
    resourceType :: ResourceTypeForTagging,
    -- | The resource ID you want to tag.
    --
    -- Use the ID of the resource. Here are some examples:
    --
    -- @MaintenanceWindow@: @mw-012345abcde@
    --
    -- @PatchBaseline@: @pb-012345abcde@
    --
    -- @Automation@: @example-c160-4567-8519-012345abcde@
    --
    -- @OpsMetadata@ object: @ResourceID@ for tagging is created from the
    -- Amazon Resource Name (ARN) for the object. Specifically, @ResourceID@ is
    -- created from the strings that come after the word @opsmetadata@ in the
    -- ARN. For example, an OpsMetadata object with an ARN of
    -- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
    -- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
    -- @\/aws\/ssm\/MyGroup\/appmanager@.
    --
    -- For the @Document@ and @Parameter@ values, use the name of the resource.
    --
    -- @ManagedInstance@: @mi-012345abcde@
    --
    -- The @ManagedInstance@ type for this API operation is only for
    -- on-premises managed nodes. You must specify the name of the managed node
    -- in the following format: @mi-@/@ID_number@/@ @. For example,
    -- @mi-1a2b3c4d5e6f@.
    resourceId :: Prelude.Text,
    -- | One or more tags. The value parameter is required.
    --
    -- Don\'t enter personally identifiable information in this field.
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
-- 'resourceType', 'addTagsToResource_resourceType' - Specifies the type of resource you are tagging.
--
-- The @ManagedInstance@ type for this API operation is for on-premises
-- managed nodes. You must specify the name of the managed node in the
-- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
--
-- 'resourceId', 'addTagsToResource_resourceId' - The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
--
-- @MaintenanceWindow@: @mw-012345abcde@
--
-- @PatchBaseline@: @pb-012345abcde@
--
-- @Automation@: @example-c160-4567-8519-012345abcde@
--
-- @OpsMetadata@ object: @ResourceID@ for tagging is created from the
-- Amazon Resource Name (ARN) for the object. Specifically, @ResourceID@ is
-- created from the strings that come after the word @opsmetadata@ in the
-- ARN. For example, an OpsMetadata object with an ARN of
-- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
-- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
-- @\/aws\/ssm\/MyGroup\/appmanager@.
--
-- For the @Document@ and @Parameter@ values, use the name of the resource.
--
-- @ManagedInstance@: @mi-012345abcde@
--
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed nodes. You must specify the name of the managed node
-- in the following format: @mi-@/@ID_number@/@ @. For example,
-- @mi-1a2b3c4d5e6f@.
--
-- 'tags', 'addTagsToResource_tags' - One or more tags. The value parameter is required.
--
-- Don\'t enter personally identifiable information in this field.
newAddTagsToResource ::
  -- | 'resourceType'
  ResourceTypeForTagging ->
  -- | 'resourceId'
  Prelude.Text ->
  AddTagsToResource
newAddTagsToResource pResourceType_ pResourceId_ =
  AddTagsToResource'
    { resourceType = pResourceType_,
      resourceId = pResourceId_,
      tags = Prelude.mempty
    }

-- | Specifies the type of resource you are tagging.
--
-- The @ManagedInstance@ type for this API operation is for on-premises
-- managed nodes. You must specify the name of the managed node in the
-- following format: @mi-@/@ID_number@/@ @. For example, @mi-1a2b3c4d5e6f@.
addTagsToResource_resourceType :: Lens.Lens' AddTagsToResource ResourceTypeForTagging
addTagsToResource_resourceType = Lens.lens (\AddTagsToResource' {resourceType} -> resourceType) (\s@AddTagsToResource' {} a -> s {resourceType = a} :: AddTagsToResource)

-- | The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
--
-- @MaintenanceWindow@: @mw-012345abcde@
--
-- @PatchBaseline@: @pb-012345abcde@
--
-- @Automation@: @example-c160-4567-8519-012345abcde@
--
-- @OpsMetadata@ object: @ResourceID@ for tagging is created from the
-- Amazon Resource Name (ARN) for the object. Specifically, @ResourceID@ is
-- created from the strings that come after the word @opsmetadata@ in the
-- ARN. For example, an OpsMetadata object with an ARN of
-- @arn:aws:ssm:us-east-2:1234567890:opsmetadata\/aws\/ssm\/MyGroup\/appmanager@
-- has a @ResourceID@ of either @aws\/ssm\/MyGroup\/appmanager@ or
-- @\/aws\/ssm\/MyGroup\/appmanager@.
--
-- For the @Document@ and @Parameter@ values, use the name of the resource.
--
-- @ManagedInstance@: @mi-012345abcde@
--
-- The @ManagedInstance@ type for this API operation is only for
-- on-premises managed nodes. You must specify the name of the managed node
-- in the following format: @mi-@/@ID_number@/@ @. For example,
-- @mi-1a2b3c4d5e6f@.
addTagsToResource_resourceId :: Lens.Lens' AddTagsToResource Prelude.Text
addTagsToResource_resourceId = Lens.lens (\AddTagsToResource' {resourceId} -> resourceId) (\s@AddTagsToResource' {} a -> s {resourceId = a} :: AddTagsToResource)

-- | One or more tags. The value parameter is required.
--
-- Don\'t enter personally identifiable information in this field.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTagsToResource where
  hashWithSalt _salt AddTagsToResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTagsToResource where
  rnf AddTagsToResource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders AddTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.AddTagsToResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'addTagsToResourceResponse_httpStatus' - The response's http status code.
newAddTagsToResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsToResourceResponse
newAddTagsToResourceResponse pHttpStatus_ =
  AddTagsToResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addTagsToResourceResponse_httpStatus :: Lens.Lens' AddTagsToResourceResponse Prelude.Int
addTagsToResourceResponse_httpStatus = Lens.lens (\AddTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddTagsToResourceResponse)

instance Prelude.NFData AddTagsToResourceResponse where
  rnf AddTagsToResourceResponse' {..} =
    Prelude.rnf httpStatus
