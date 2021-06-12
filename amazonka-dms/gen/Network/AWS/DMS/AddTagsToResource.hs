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
-- Module      : Network.AWS.DMS.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an AWS DMS resource, including replication
-- instance, endpoint, security group, and migration task. These tags can
-- also be used with cost allocation reporting to track cost associated
-- with DMS resources, or used in a Condition statement in an IAM policy
-- for DMS. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html Tag>
-- data type description.
module Network.AWS.DMS.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceArn,
    addTagsToResource_tags,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,

    -- * Response Lenses
    addTagsToResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Associates a set of tags with an AWS DMS resource.
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | Identifies the AWS DMS resource to which tags should be added. The value
    -- for this parameter is an Amazon Resource Name (ARN).
    --
    -- For AWS DMS, you can tag a replication instance, an endpoint, or a
    -- replication task.
    resourceArn :: Core.Text,
    -- | One or more tags to be assigned to the resource.
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
-- 'resourceArn', 'addTagsToResource_resourceArn' - Identifies the AWS DMS resource to which tags should be added. The value
-- for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a
-- replication task.
--
-- 'tags', 'addTagsToResource_tags' - One or more tags to be assigned to the resource.
newAddTagsToResource ::
  -- | 'resourceArn'
  Core.Text ->
  AddTagsToResource
newAddTagsToResource pResourceArn_ =
  AddTagsToResource'
    { resourceArn = pResourceArn_,
      tags = Core.mempty
    }

-- | Identifies the AWS DMS resource to which tags should be added. The value
-- for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a
-- replication task.
addTagsToResource_resourceArn :: Lens.Lens' AddTagsToResource Core.Text
addTagsToResource_resourceArn = Lens.lens (\AddTagsToResource' {resourceArn} -> resourceArn) (\s@AddTagsToResource' {} a -> s {resourceArn = a} :: AddTagsToResource)

-- | One or more tags to be assigned to the resource.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsToResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddTagsToResource

instance Core.NFData AddTagsToResource

instance Core.ToHeaders AddTagsToResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.AddTagsToResource" ::
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
          [ Core.Just ("ResourceArn" Core..= resourceArn),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath AddTagsToResource where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToResource where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'addTagsToResourceResponse_httpStatus' - The response's http status code.
newAddTagsToResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddTagsToResourceResponse
newAddTagsToResourceResponse pHttpStatus_ =
  AddTagsToResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addTagsToResourceResponse_httpStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
addTagsToResourceResponse_httpStatus = Lens.lens (\AddTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddTagsToResourceResponse)

instance Core.NFData AddTagsToResourceResponse
