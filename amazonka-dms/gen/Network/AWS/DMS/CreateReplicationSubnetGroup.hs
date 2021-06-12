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
-- Module      : Network.AWS.DMS.CreateReplicationSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication subnet group given a list of the subnet IDs in a
-- VPC.
module Network.AWS.DMS.CreateReplicationSubnetGroup
  ( -- * Creating a Request
    CreateReplicationSubnetGroup (..),
    newCreateReplicationSubnetGroup,

    -- * Request Lenses
    createReplicationSubnetGroup_tags,
    createReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    createReplicationSubnetGroup_replicationSubnetGroupDescription,
    createReplicationSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateReplicationSubnetGroupResponse (..),
    newCreateReplicationSubnetGroupResponse,

    -- * Response Lenses
    createReplicationSubnetGroupResponse_replicationSubnetGroup,
    createReplicationSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { -- | One or more tags to be assigned to the subnet group.
    tags :: Core.Maybe [Tag],
    -- | The name for the replication subnet group. This value is stored as a
    -- lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters,
    -- periods, spaces, underscores, or hyphens. Must not be \"default\".
    --
    -- Example: @mySubnetgroup@
    replicationSubnetGroupIdentifier :: Core.Text,
    -- | The description for the subnet group.
    replicationSubnetGroupDescription :: Core.Text,
    -- | One or more subnet IDs to be assigned to the subnet group.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createReplicationSubnetGroup_tags' - One or more tags to be assigned to the subnet group.
--
-- 'replicationSubnetGroupIdentifier', 'createReplicationSubnetGroup_replicationSubnetGroupIdentifier' - The name for the replication subnet group. This value is stored as a
-- lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters,
-- periods, spaces, underscores, or hyphens. Must not be \"default\".
--
-- Example: @mySubnetgroup@
--
-- 'replicationSubnetGroupDescription', 'createReplicationSubnetGroup_replicationSubnetGroupDescription' - The description for the subnet group.
--
-- 'subnetIds', 'createReplicationSubnetGroup_subnetIds' - One or more subnet IDs to be assigned to the subnet group.
newCreateReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Core.Text ->
  -- | 'replicationSubnetGroupDescription'
  Core.Text ->
  CreateReplicationSubnetGroup
newCreateReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_
  pReplicationSubnetGroupDescription_ =
    CreateReplicationSubnetGroup'
      { tags = Core.Nothing,
        replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_,
        replicationSubnetGroupDescription =
          pReplicationSubnetGroupDescription_,
        subnetIds = Core.mempty
      }

-- | One or more tags to be assigned to the subnet group.
createReplicationSubnetGroup_tags :: Lens.Lens' CreateReplicationSubnetGroup (Core.Maybe [Tag])
createReplicationSubnetGroup_tags = Lens.lens (\CreateReplicationSubnetGroup' {tags} -> tags) (\s@CreateReplicationSubnetGroup' {} a -> s {tags = a} :: CreateReplicationSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The name for the replication subnet group. This value is stored as a
-- lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters,
-- periods, spaces, underscores, or hyphens. Must not be \"default\".
--
-- Example: @mySubnetgroup@
createReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationSubnetGroup Core.Text
createReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\CreateReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@CreateReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationSubnetGroup)

-- | The description for the subnet group.
createReplicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' CreateReplicationSubnetGroup Core.Text
createReplicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\CreateReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@CreateReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: CreateReplicationSubnetGroup)

-- | One or more subnet IDs to be assigned to the subnet group.
createReplicationSubnetGroup_subnetIds :: Lens.Lens' CreateReplicationSubnetGroup [Core.Text]
createReplicationSubnetGroup_subnetIds = Lens.lens (\CreateReplicationSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateReplicationSubnetGroup' {} a -> s {subnetIds = a} :: CreateReplicationSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest CreateReplicationSubnetGroup where
  type
    AWSResponse CreateReplicationSubnetGroup =
      CreateReplicationSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationSubnetGroupResponse'
            Core.<$> (x Core..?> "ReplicationSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateReplicationSubnetGroup

instance Core.NFData CreateReplicationSubnetGroup

instance Core.ToHeaders CreateReplicationSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.CreateReplicationSubnetGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateReplicationSubnetGroup where
  toJSON CreateReplicationSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Core..= replicationSubnetGroupIdentifier
              ),
            Core.Just
              ( "ReplicationSubnetGroupDescription"
                  Core..= replicationSubnetGroupDescription
              ),
            Core.Just ("SubnetIds" Core..= subnetIds)
          ]
      )

instance Core.ToPath CreateReplicationSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateReplicationSubnetGroup where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newCreateReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { -- | The replication subnet group that was created.
    replicationSubnetGroup :: Core.Maybe ReplicationSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroup', 'createReplicationSubnetGroupResponse_replicationSubnetGroup' - The replication subnet group that was created.
--
-- 'httpStatus', 'createReplicationSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateReplicationSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateReplicationSubnetGroupResponse
newCreateReplicationSubnetGroupResponse pHttpStatus_ =
  CreateReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication subnet group that was created.
createReplicationSubnetGroupResponse_replicationSubnetGroup :: Lens.Lens' CreateReplicationSubnetGroupResponse (Core.Maybe ReplicationSubnetGroup)
createReplicationSubnetGroupResponse_replicationSubnetGroup = Lens.lens (\CreateReplicationSubnetGroupResponse' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@CreateReplicationSubnetGroupResponse' {} a -> s {replicationSubnetGroup = a} :: CreateReplicationSubnetGroupResponse)

-- | The response's http status code.
createReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' CreateReplicationSubnetGroupResponse Core.Int
createReplicationSubnetGroupResponse_httpStatus = Lens.lens (\CreateReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateReplicationSubnetGroupResponse)

instance
  Core.NFData
    CreateReplicationSubnetGroupResponse
