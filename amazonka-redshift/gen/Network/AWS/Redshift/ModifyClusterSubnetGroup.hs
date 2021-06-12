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
-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the
-- new list of subnets.
module Network.AWS.Redshift.ModifyClusterSubnetGroup
  ( -- * Creating a Request
    ModifyClusterSubnetGroup (..),
    newModifyClusterSubnetGroup,

    -- * Request Lenses
    modifyClusterSubnetGroup_description,
    modifyClusterSubnetGroup_clusterSubnetGroupName,
    modifyClusterSubnetGroup_subnetIds,

    -- * Destructuring the Response
    ModifyClusterSubnetGroupResponse (..),
    newModifyClusterSubnetGroupResponse,

    -- * Response Lenses
    modifyClusterSubnetGroupResponse_clusterSubnetGroup,
    modifyClusterSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
  { -- | A text description of the subnet group to be modified.
    description :: Core.Maybe Core.Text,
    -- | The name of the subnet group to be modified.
    clusterSubnetGroupName :: Core.Text,
    -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
    -- single request.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyClusterSubnetGroup_description' - A text description of the subnet group to be modified.
--
-- 'clusterSubnetGroupName', 'modifyClusterSubnetGroup_clusterSubnetGroupName' - The name of the subnet group to be modified.
--
-- 'subnetIds', 'modifyClusterSubnetGroup_subnetIds' - An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
newModifyClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Core.Text ->
  ModifyClusterSubnetGroup
newModifyClusterSubnetGroup pClusterSubnetGroupName_ =
  ModifyClusterSubnetGroup'
    { description =
        Core.Nothing,
      clusterSubnetGroupName = pClusterSubnetGroupName_,
      subnetIds = Core.mempty
    }

-- | A text description of the subnet group to be modified.
modifyClusterSubnetGroup_description :: Lens.Lens' ModifyClusterSubnetGroup (Core.Maybe Core.Text)
modifyClusterSubnetGroup_description = Lens.lens (\ModifyClusterSubnetGroup' {description} -> description) (\s@ModifyClusterSubnetGroup' {} a -> s {description = a} :: ModifyClusterSubnetGroup)

-- | The name of the subnet group to be modified.
modifyClusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' ModifyClusterSubnetGroup Core.Text
modifyClusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\ModifyClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@ModifyClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: ModifyClusterSubnetGroup)

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
modifyClusterSubnetGroup_subnetIds :: Lens.Lens' ModifyClusterSubnetGroup [Core.Text]
modifyClusterSubnetGroup_subnetIds = Lens.lens (\ModifyClusterSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyClusterSubnetGroup' {} a -> s {subnetIds = a} :: ModifyClusterSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest ModifyClusterSubnetGroup where
  type
    AWSResponse ModifyClusterSubnetGroup =
      ModifyClusterSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSubnetGroupResult"
      ( \s h x ->
          ModifyClusterSubnetGroupResponse'
            Core.<$> (x Core..@? "ClusterSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClusterSubnetGroup

instance Core.NFData ModifyClusterSubnetGroup

instance Core.ToHeaders ModifyClusterSubnetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClusterSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClusterSubnetGroup where
  toQuery ModifyClusterSubnetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClusterSubnetGroup" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Description" Core.=: description,
        "ClusterSubnetGroupName"
          Core.=: clusterSubnetGroupName,
        "SubnetIds"
          Core.=: Core.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newModifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe ClusterSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroup', 'modifyClusterSubnetGroupResponse_clusterSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyClusterSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClusterSubnetGroupResponse
newModifyClusterSubnetGroupResponse pHttpStatus_ =
  ModifyClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterSubnetGroupResponse_clusterSubnetGroup :: Lens.Lens' ModifyClusterSubnetGroupResponse (Core.Maybe ClusterSubnetGroup)
modifyClusterSubnetGroupResponse_clusterSubnetGroup = Lens.lens (\ModifyClusterSubnetGroupResponse' {clusterSubnetGroup} -> clusterSubnetGroup) (\s@ModifyClusterSubnetGroupResponse' {} a -> s {clusterSubnetGroup = a} :: ModifyClusterSubnetGroupResponse)

-- | The response's http status code.
modifyClusterSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyClusterSubnetGroupResponse Core.Int
modifyClusterSubnetGroupResponse_httpStatus = Lens.lens (\ModifyClusterSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyClusterSubnetGroupResponse)

instance Core.NFData ModifyClusterSubnetGroupResponse
