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
-- Module      : Amazonka.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes permission settings for the specified snapshot. You may
-- add or remove specified Amazon Web Services account IDs from a
-- snapshot\'s list of create volume permissions, but you cannot do both in
-- a single operation. If you need to both add and remove account IDs for a
-- snapshot, you must use multiple operations. You can make up to 500
-- modifications to a snapshot in a single operation.
--
-- Encrypted snapshots and snapshots with Amazon Web Services Marketplace
-- product codes cannot be made public. Snapshots encrypted with your
-- default KMS key cannot be shared with other accounts.
--
-- For more information about modifying snapshot permissions, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Share a snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ModifySnapshotAttribute
  ( -- * Creating a Request
    ModifySnapshotAttribute (..),
    newModifySnapshotAttribute,

    -- * Request Lenses
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_operationType,
    modifySnapshotAttribute_userIds,
    modifySnapshotAttribute_snapshotId,

    -- * Destructuring the Response
    ModifySnapshotAttributeResponse (..),
    newModifySnapshotAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySnapshotAttribute' smart constructor.
data ModifySnapshotAttribute = ModifySnapshotAttribute'
  { -- | The snapshot attribute to modify. Only volume creation permissions can
    -- be modified.
    attribute :: Prelude.Maybe SnapshotAttributeName,
    -- | A JSON representation of the snapshot attribute modification.
    createVolumePermission :: Prelude.Maybe CreateVolumePermissionModifications,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The group to modify for the snapshot.
    groupNames :: Prelude.Maybe [Prelude.Text],
    -- | The type of operation to perform to the attribute.
    operationType :: Prelude.Maybe OperationType,
    -- | The account ID to modify for the snapshot.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'modifySnapshotAttribute_attribute' - The snapshot attribute to modify. Only volume creation permissions can
-- be modified.
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
-- 'operationType', 'modifySnapshotAttribute_operationType' - The type of operation to perform to the attribute.
--
-- 'userIds', 'modifySnapshotAttribute_userIds' - The account ID to modify for the snapshot.
--
-- 'snapshotId', 'modifySnapshotAttribute_snapshotId' - The ID of the snapshot.
newModifySnapshotAttribute ::
  -- | 'snapshotId'
  Prelude.Text ->
  ModifySnapshotAttribute
newModifySnapshotAttribute pSnapshotId_ =
  ModifySnapshotAttribute'
    { attribute =
        Prelude.Nothing,
      createVolumePermission = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      groupNames = Prelude.Nothing,
      operationType = Prelude.Nothing,
      userIds = Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | The snapshot attribute to modify. Only volume creation permissions can
-- be modified.
modifySnapshotAttribute_attribute :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe SnapshotAttributeName)
modifySnapshotAttribute_attribute = Lens.lens (\ModifySnapshotAttribute' {attribute} -> attribute) (\s@ModifySnapshotAttribute' {} a -> s {attribute = a} :: ModifySnapshotAttribute)

-- | A JSON representation of the snapshot attribute modification.
modifySnapshotAttribute_createVolumePermission :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe CreateVolumePermissionModifications)
modifySnapshotAttribute_createVolumePermission = Lens.lens (\ModifySnapshotAttribute' {createVolumePermission} -> createVolumePermission) (\s@ModifySnapshotAttribute' {} a -> s {createVolumePermission = a} :: ModifySnapshotAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifySnapshotAttribute_dryRun :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe Prelude.Bool)
modifySnapshotAttribute_dryRun = Lens.lens (\ModifySnapshotAttribute' {dryRun} -> dryRun) (\s@ModifySnapshotAttribute' {} a -> s {dryRun = a} :: ModifySnapshotAttribute)

-- | The group to modify for the snapshot.
modifySnapshotAttribute_groupNames :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifySnapshotAttribute_groupNames = Lens.lens (\ModifySnapshotAttribute' {groupNames} -> groupNames) (\s@ModifySnapshotAttribute' {} a -> s {groupNames = a} :: ModifySnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The type of operation to perform to the attribute.
modifySnapshotAttribute_operationType :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe OperationType)
modifySnapshotAttribute_operationType = Lens.lens (\ModifySnapshotAttribute' {operationType} -> operationType) (\s@ModifySnapshotAttribute' {} a -> s {operationType = a} :: ModifySnapshotAttribute)

-- | The account ID to modify for the snapshot.
modifySnapshotAttribute_userIds :: Lens.Lens' ModifySnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifySnapshotAttribute_userIds = Lens.lens (\ModifySnapshotAttribute' {userIds} -> userIds) (\s@ModifySnapshotAttribute' {} a -> s {userIds = a} :: ModifySnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the snapshot.
modifySnapshotAttribute_snapshotId :: Lens.Lens' ModifySnapshotAttribute Prelude.Text
modifySnapshotAttribute_snapshotId = Lens.lens (\ModifySnapshotAttribute' {snapshotId} -> snapshotId) (\s@ModifySnapshotAttribute' {} a -> s {snapshotId = a} :: ModifySnapshotAttribute)

instance Core.AWSRequest ModifySnapshotAttribute where
  type
    AWSResponse ModifySnapshotAttribute =
      ModifySnapshotAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ModifySnapshotAttributeResponse'

instance Prelude.Hashable ModifySnapshotAttribute where
  hashWithSalt _salt ModifySnapshotAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` createVolumePermission
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupNames
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ModifySnapshotAttribute where
  rnf ModifySnapshotAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf createVolumePermission
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupNames
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders ModifySnapshotAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifySnapshotAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifySnapshotAttribute where
  toQuery ModifySnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifySnapshotAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attribute" Data.=: attribute,
        "CreateVolumePermission"
          Data.=: createVolumePermission,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "UserGroup"
              Prelude.<$> groupNames
          ),
        "OperationType" Data.=: operationType,
        Data.toQuery
          (Data.toQueryList "UserId" Prelude.<$> userIds),
        "SnapshotId" Data.=: snapshotId
      ]

-- | /See:/ 'newModifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifySnapshotAttributeResponse ::
  ModifySnapshotAttributeResponse
newModifySnapshotAttributeResponse =
  ModifySnapshotAttributeResponse'

instance
  Prelude.NFData
    ModifySnapshotAttributeResponse
  where
  rnf _ = ()
