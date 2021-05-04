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
-- Module      : Network.AWS.SSM.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified resource. Tags are
-- metadata that you can assign to your documents, managed instances,
-- maintenance windows, Parameter Store parameters, and patch baselines.
-- Tags enable you to categorize your resources in different ways, for
-- example, by purpose, owner, or environment. Each tag consists of a key
-- and an optional value, both of which you define. For example, you could
-- define a set of tags for your account\'s managed instances that helps
-- you track each instance\'s owner and stack level. For example: Key=Owner
-- and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production,
-- Pre-Production, or Test.
--
-- Each resource can have a maximum of 50 tags.
--
-- We recommend that you devise a set of tag keys that meets your needs for
-- each resource type. Using a consistent set of tag keys makes it easier
-- for you to manage your resources. You can search and filter the
-- resources based on the tags you add. Tags don\'t have any semantic
-- meaning to and are interpreted strictly as a string of characters.
--
-- For more information about using tags with EC2 instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.SSM.AddTagsToResource
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | Specifies the type of resource you are tagging.
    --
    -- The ManagedInstance type for this API action is for on-premises managed
    -- instances. You must specify the name of the managed instance in the
    -- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
    resourceType :: ResourceTypeForTagging,
    -- | The resource ID you want to tag.
    --
    -- Use the ID of the resource. Here are some examples:
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
    -- managed instances. You must specify the name of the managed instance in
    -- the following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
    resourceId :: Prelude.Text,
    -- | One or more tags. The value parameter is required, but if you don\'t
    -- want the tag to have a value, specify the parameter with no value, and
    -- we set the value to an empty string.
    --
    -- Do not enter personally identifiable information in this field.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- The ManagedInstance type for this API action is for on-premises managed
-- instances. You must specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
--
-- 'resourceId', 'addTagsToResource_resourceId' - The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
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
-- managed instances. You must specify the name of the managed instance in
-- the following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
--
-- 'tags', 'addTagsToResource_tags' - One or more tags. The value parameter is required, but if you don\'t
-- want the tag to have a value, specify the parameter with no value, and
-- we set the value to an empty string.
--
-- Do not enter personally identifiable information in this field.
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
-- The ManagedInstance type for this API action is for on-premises managed
-- instances. You must specify the name of the managed instance in the
-- following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
addTagsToResource_resourceType :: Lens.Lens' AddTagsToResource ResourceTypeForTagging
addTagsToResource_resourceType = Lens.lens (\AddTagsToResource' {resourceType} -> resourceType) (\s@AddTagsToResource' {} a -> s {resourceType = a} :: AddTagsToResource)

-- | The resource ID you want to tag.
--
-- Use the ID of the resource. Here are some examples:
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
-- managed instances. You must specify the name of the managed instance in
-- the following format: mi-ID_number. For example, mi-1a2b3c4d5e6f.
addTagsToResource_resourceId :: Lens.Lens' AddTagsToResource Prelude.Text
addTagsToResource_resourceId = Lens.lens (\AddTagsToResource' {resourceId} -> resourceId) (\s@AddTagsToResource' {} a -> s {resourceId = a} :: AddTagsToResource)

-- | One or more tags. The value parameter is required, but if you don\'t
-- want the tag to have a value, specify the parameter with no value, and
-- we set the value to an empty string.
--
-- Do not enter personally identifiable information in this field.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTagsToResource

instance Prelude.NFData AddTagsToResource

instance Prelude.ToHeaders AddTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.AddTagsToResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceType" Prelude..= resourceType),
            Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AddTagsToResourceResponse
