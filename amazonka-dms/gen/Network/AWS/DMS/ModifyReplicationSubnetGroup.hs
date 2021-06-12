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
-- Module      : Network.AWS.DMS.ModifyReplicationSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for the specified replication subnet group.
module Network.AWS.DMS.ModifyReplicationSubnetGroup
  ( -- * Creating a Request
    ModifyReplicationSubnetGroup (..),
    newModifyReplicationSubnetGroup,

    -- * Request Lenses
    modifyReplicationSubnetGroup_replicationSubnetGroupDescription,
    modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    modifyReplicationSubnetGroup_subnetIds,

    -- * Destructuring the Response
    ModifyReplicationSubnetGroupResponse (..),
    newModifyReplicationSubnetGroupResponse,

    -- * Response Lenses
    modifyReplicationSubnetGroupResponse_replicationSubnetGroup,
    modifyReplicationSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { -- | A description for the replication instance subnet group.
    replicationSubnetGroupDescription :: Core.Maybe Core.Text,
    -- | The name of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Core.Text,
    -- | A list of subnet IDs.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupDescription', 'modifyReplicationSubnetGroup_replicationSubnetGroupDescription' - A description for the replication instance subnet group.
--
-- 'replicationSubnetGroupIdentifier', 'modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier' - The name of the replication instance subnet group.
--
-- 'subnetIds', 'modifyReplicationSubnetGroup_subnetIds' - A list of subnet IDs.
newModifyReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Core.Text ->
  ModifyReplicationSubnetGroup
newModifyReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_ =
    ModifyReplicationSubnetGroup'
      { replicationSubnetGroupDescription =
          Core.Nothing,
        replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_,
        subnetIds = Core.mempty
      }

-- | A description for the replication instance subnet group.
modifyReplicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' ModifyReplicationSubnetGroup (Core.Maybe Core.Text)
modifyReplicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\ModifyReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@ModifyReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: ModifyReplicationSubnetGroup)

-- | The name of the replication instance subnet group.
modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' ModifyReplicationSubnetGroup Core.Text
modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\ModifyReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@ModifyReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: ModifyReplicationSubnetGroup)

-- | A list of subnet IDs.
modifyReplicationSubnetGroup_subnetIds :: Lens.Lens' ModifyReplicationSubnetGroup [Core.Text]
modifyReplicationSubnetGroup_subnetIds = Lens.lens (\ModifyReplicationSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyReplicationSubnetGroup' {} a -> s {subnetIds = a} :: ModifyReplicationSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest ModifyReplicationSubnetGroup where
  type
    AWSResponse ModifyReplicationSubnetGroup =
      ModifyReplicationSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationSubnetGroupResponse'
            Core.<$> (x Core..?> "ReplicationSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyReplicationSubnetGroup

instance Core.NFData ModifyReplicationSubnetGroup

instance Core.ToHeaders ModifyReplicationSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ModifyReplicationSubnetGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyReplicationSubnetGroup where
  toJSON ModifyReplicationSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReplicationSubnetGroupDescription" Core..=)
              Core.<$> replicationSubnetGroupDescription,
            Core.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Core..= replicationSubnetGroupIdentifier
              ),
            Core.Just ("SubnetIds" Core..= subnetIds)
          ]
      )

instance Core.ToPath ModifyReplicationSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery ModifyReplicationSubnetGroup where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newModifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { -- | The modified replication subnet group.
    replicationSubnetGroup :: Core.Maybe ReplicationSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReplicationSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroup', 'modifyReplicationSubnetGroupResponse_replicationSubnetGroup' - The modified replication subnet group.
--
-- 'httpStatus', 'modifyReplicationSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyReplicationSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyReplicationSubnetGroupResponse
newModifyReplicationSubnetGroupResponse pHttpStatus_ =
  ModifyReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified replication subnet group.
modifyReplicationSubnetGroupResponse_replicationSubnetGroup :: Lens.Lens' ModifyReplicationSubnetGroupResponse (Core.Maybe ReplicationSubnetGroup)
modifyReplicationSubnetGroupResponse_replicationSubnetGroup = Lens.lens (\ModifyReplicationSubnetGroupResponse' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@ModifyReplicationSubnetGroupResponse' {} a -> s {replicationSubnetGroup = a} :: ModifyReplicationSubnetGroupResponse)

-- | The response's http status code.
modifyReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyReplicationSubnetGroupResponse Core.Int
modifyReplicationSubnetGroupResponse_httpStatus = Lens.lens (\ModifyReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyReplicationSubnetGroupResponse)

instance
  Core.NFData
    ModifyReplicationSubnetGroupResponse
