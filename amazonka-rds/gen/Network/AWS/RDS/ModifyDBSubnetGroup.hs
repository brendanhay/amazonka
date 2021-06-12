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
-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the AWS Region.
module Network.AWS.RDS.ModifyDBSubnetGroup
  ( -- * Creating a Request
    ModifyDBSubnetGroup (..),
    newModifyDBSubnetGroup,

    -- * Request Lenses
    modifyDBSubnetGroup_dbSubnetGroupDescription,
    modifyDBSubnetGroup_dbSubnetGroupName,
    modifyDBSubnetGroup_subnetIds,

    -- * Destructuring the Response
    ModifyDBSubnetGroupResponse (..),
    newModifyDBSubnetGroupResponse,

    -- * Response Lenses
    modifyDBSubnetGroupResponse_dbSubnetGroup,
    modifyDBSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyDBSubnetGroup' smart constructor.
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
  { -- | The description for the DB subnet group.
    dbSubnetGroupDescription :: Core.Maybe Core.Text,
    -- | The name for the DB subnet group. This value is stored as a lowercase
    -- string. You can\'t modify the default subnet group.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Core.Text,
    -- | The EC2 subnet IDs for the DB subnet group.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupDescription', 'modifyDBSubnetGroup_dbSubnetGroupDescription' - The description for the DB subnet group.
--
-- 'dbSubnetGroupName', 'modifyDBSubnetGroup_dbSubnetGroupName' - The name for the DB subnet group. This value is stored as a lowercase
-- string. You can\'t modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
--
-- 'subnetIds', 'modifyDBSubnetGroup_subnetIds' - The EC2 subnet IDs for the DB subnet group.
newModifyDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Core.Text ->
  ModifyDBSubnetGroup
newModifyDBSubnetGroup pDBSubnetGroupName_ =
  ModifyDBSubnetGroup'
    { dbSubnetGroupDescription =
        Core.Nothing,
      dbSubnetGroupName = pDBSubnetGroupName_,
      subnetIds = Core.mempty
    }

-- | The description for the DB subnet group.
modifyDBSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' ModifyDBSubnetGroup (Core.Maybe Core.Text)
modifyDBSubnetGroup_dbSubnetGroupDescription = Lens.lens (\ModifyDBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@ModifyDBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: ModifyDBSubnetGroup)

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. You can\'t modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
modifyDBSubnetGroup_dbSubnetGroupName :: Lens.Lens' ModifyDBSubnetGroup Core.Text
modifyDBSubnetGroup_dbSubnetGroupName = Lens.lens (\ModifyDBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@ModifyDBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: ModifyDBSubnetGroup)

-- | The EC2 subnet IDs for the DB subnet group.
modifyDBSubnetGroup_subnetIds :: Lens.Lens' ModifyDBSubnetGroup [Core.Text]
modifyDBSubnetGroup_subnetIds = Lens.lens (\ModifyDBSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyDBSubnetGroup' {} a -> s {subnetIds = a} :: ModifyDBSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest ModifyDBSubnetGroup where
  type
    AWSResponse ModifyDBSubnetGroup =
      ModifyDBSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBSubnetGroupResult"
      ( \s h x ->
          ModifyDBSubnetGroupResponse'
            Core.<$> (x Core..@? "DBSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyDBSubnetGroup

instance Core.NFData ModifyDBSubnetGroup

instance Core.ToHeaders ModifyDBSubnetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyDBSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDBSubnetGroup where
  toQuery ModifyDBSubnetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyDBSubnetGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSubnetGroupDescription"
          Core.=: dbSubnetGroupDescription,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "SubnetIds"
          Core.=: Core.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newModifyDBSubnetGroupResponse' smart constructor.
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
  { dbSubnetGroup :: Core.Maybe DBSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroup', 'modifyDBSubnetGroupResponse_dbSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyDBSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyDBSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyDBSubnetGroupResponse
newModifyDBSubnetGroupResponse pHttpStatus_ =
  ModifyDBSubnetGroupResponse'
    { dbSubnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBSubnetGroupResponse_dbSubnetGroup :: Lens.Lens' ModifyDBSubnetGroupResponse (Core.Maybe DBSubnetGroup)
modifyDBSubnetGroupResponse_dbSubnetGroup = Lens.lens (\ModifyDBSubnetGroupResponse' {dbSubnetGroup} -> dbSubnetGroup) (\s@ModifyDBSubnetGroupResponse' {} a -> s {dbSubnetGroup = a} :: ModifyDBSubnetGroupResponse)

-- | The response's http status code.
modifyDBSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyDBSubnetGroupResponse Core.Int
modifyDBSubnetGroupResponse_httpStatus = Lens.lens (\ModifyDBSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyDBSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyDBSubnetGroupResponse)

instance Core.NFData ModifyDBSubnetGroupResponse
