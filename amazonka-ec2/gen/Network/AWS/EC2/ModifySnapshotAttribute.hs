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
-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes permission settings for the specified snapshot. You may
-- add or remove specified AWS account IDs from a snapshot\'s list of
-- create volume permissions, but you cannot do both in a single operation.
-- If you need to both add and remove account IDs for a snapshot, you must
-- use multiple operations. You can make up to 500 modifications to a
-- snapshot in a single operation.
--
-- Encrypted snapshots and snapshots with AWS Marketplace product codes
-- cannot be made public. Snapshots encrypted with your default CMK cannot
-- be shared with other accounts.
--
-- For more information about modifying snapshot permissions, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.ModifySnapshotAttribute
  ( -- * Creating a Request
    ModifySnapshotAttribute (..),
    newModifySnapshotAttribute,

    -- * Request Lenses
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_userIds,
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_operationType,
    modifySnapshotAttribute_snapshotId,

    -- * Destructuring the Response
    ModifySnapshotAttributeResponse (..),
    newModifySnapshotAttributeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifySnapshotAttribute' smart constructor.
data ModifySnapshotAttribute = ModifySnapshotAttribute'
  { -- | A JSON representation of the snapshot attribute modification.
    createVolumePermission :: Core.Maybe CreateVolumePermissionModifications,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The group to modify for the snapshot.
    groupNames :: Core.Maybe [Core.Text],
    -- | The account ID to modify for the snapshot.
    userIds :: Core.Maybe [Core.Text],
    -- | The snapshot attribute to modify. Only volume creation permissions can
    -- be modified.
    attribute :: Core.Maybe SnapshotAttributeName,
    -- | The type of operation to perform to the attribute.
    operationType :: Core.Maybe OperationType,
    -- | The ID of the snapshot.
    snapshotId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createVolumePermission', 'modifySnapshotAttribute_createVolumePermission' - A JSON representation of the snapshot attribute modification.
--
-- 'dryRun', 'modifySnapshotAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupNames', 'modifySnapshotAttribute_groupNames' - The group to modify for the snapshot.
--
-- 'userIds', 'modifySnapshotAttribute_userIds' - The account ID to modify for the snapshot.
--
-- 'attribute', 'modifySnapshotAttribute_attribute' - The snapshot attribute to modify. Only volume creation permissions can
-- be modified.
--
-- 'operationType', 'modifySnapshotAttribute_operationType' - The type of operation to perform to the attribute.
--
-- 'snapshotId', 'modifySnapshotAttribute_snapshotId' - The ID of the snapshot.
newModifySnapshotAttribute ::
  -- | 'snapshotId'
  Core.Text ->
  ModifySnapshotAttribute
newModifySnapshotAttribute pSnapshotId_ =
  ModifySnapshotAttribute'
    { createVolumePermission =
        Core.Nothing,
      dryRun = Core.Nothing,
      groupNames = Core.Nothing,
      userIds = Core.Nothing,
      attribute = Core.Nothing,
      operationType = Core.Nothing,
      snapshotId = pSnapshotId_
    }

-- | A JSON representation of the snapshot attribute modification.
modifySnapshotAttribute_createVolumePermission :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe CreateVolumePermissionModifications)
modifySnapshotAttribute_createVolumePermission = Lens.lens (\ModifySnapshotAttribute' {createVolumePermission} -> createVolumePermission) (\s@ModifySnapshotAttribute' {} a -> s {createVolumePermission = a} :: ModifySnapshotAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifySnapshotAttribute_dryRun :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe Core.Bool)
modifySnapshotAttribute_dryRun = Lens.lens (\ModifySnapshotAttribute' {dryRun} -> dryRun) (\s@ModifySnapshotAttribute' {} a -> s {dryRun = a} :: ModifySnapshotAttribute)

-- | The group to modify for the snapshot.
modifySnapshotAttribute_groupNames :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe [Core.Text])
modifySnapshotAttribute_groupNames = Lens.lens (\ModifySnapshotAttribute' {groupNames} -> groupNames) (\s@ModifySnapshotAttribute' {} a -> s {groupNames = a} :: ModifySnapshotAttribute) Core.. Lens.mapping Lens._Coerce

-- | The account ID to modify for the snapshot.
modifySnapshotAttribute_userIds :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe [Core.Text])
modifySnapshotAttribute_userIds = Lens.lens (\ModifySnapshotAttribute' {userIds} -> userIds) (\s@ModifySnapshotAttribute' {} a -> s {userIds = a} :: ModifySnapshotAttribute) Core.. Lens.mapping Lens._Coerce

-- | The snapshot attribute to modify. Only volume creation permissions can
-- be modified.
modifySnapshotAttribute_attribute :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe SnapshotAttributeName)
modifySnapshotAttribute_attribute = Lens.lens (\ModifySnapshotAttribute' {attribute} -> attribute) (\s@ModifySnapshotAttribute' {} a -> s {attribute = a} :: ModifySnapshotAttribute)

-- | The type of operation to perform to the attribute.
modifySnapshotAttribute_operationType :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe OperationType)
modifySnapshotAttribute_operationType = Lens.lens (\ModifySnapshotAttribute' {operationType} -> operationType) (\s@ModifySnapshotAttribute' {} a -> s {operationType = a} :: ModifySnapshotAttribute)

-- | The ID of the snapshot.
modifySnapshotAttribute_snapshotId :: Lens.Lens' ModifySnapshotAttribute Core.Text
modifySnapshotAttribute_snapshotId = Lens.lens (\ModifySnapshotAttribute' {snapshotId} -> snapshotId) (\s@ModifySnapshotAttribute' {} a -> s {snapshotId = a} :: ModifySnapshotAttribute)

instance Core.AWSRequest ModifySnapshotAttribute where
  type
    AWSResponse ModifySnapshotAttribute =
      ModifySnapshotAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifySnapshotAttributeResponse'

instance Core.Hashable ModifySnapshotAttribute

instance Core.NFData ModifySnapshotAttribute

instance Core.ToHeaders ModifySnapshotAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifySnapshotAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ModifySnapshotAttribute where
  toQuery ModifySnapshotAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifySnapshotAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "CreateVolumePermission"
          Core.=: createVolumePermission,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "UserGroup" Core.<$> groupNames),
        Core.toQuery
          (Core.toQueryList "UserId" Core.<$> userIds),
        "Attribute" Core.=: attribute,
        "OperationType" Core.=: operationType,
        "SnapshotId" Core.=: snapshotId
      ]

-- | /See:/ 'newModifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifySnapshotAttributeResponse ::
  ModifySnapshotAttributeResponse
newModifySnapshotAttributeResponse =
  ModifySnapshotAttributeResponse'

instance Core.NFData ModifySnapshotAttributeResponse
