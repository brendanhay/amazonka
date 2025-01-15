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
-- Module      : Amazonka.Neptune.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the Amazon Region.
module Amazonka.Neptune.ModifyDBSubnetGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBSubnetGroup' smart constructor.
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
  { -- | The description for the DB subnet group.
    dbSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name for the DB subnet group. This value is stored as a lowercase
    -- string. You can\'t modify the default subnet group.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Text,
    -- | The EC2 subnet IDs for the DB subnet group.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ModifyDBSubnetGroup
newModifyDBSubnetGroup pDBSubnetGroupName_ =
  ModifyDBSubnetGroup'
    { dbSubnetGroupDescription =
        Prelude.Nothing,
      dbSubnetGroupName = pDBSubnetGroupName_,
      subnetIds = Prelude.mempty
    }

-- | The description for the DB subnet group.
modifyDBSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' ModifyDBSubnetGroup (Prelude.Maybe Prelude.Text)
modifyDBSubnetGroup_dbSubnetGroupDescription = Lens.lens (\ModifyDBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@ModifyDBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: ModifyDBSubnetGroup)

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. You can\'t modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
modifyDBSubnetGroup_dbSubnetGroupName :: Lens.Lens' ModifyDBSubnetGroup Prelude.Text
modifyDBSubnetGroup_dbSubnetGroupName = Lens.lens (\ModifyDBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@ModifyDBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: ModifyDBSubnetGroup)

-- | The EC2 subnet IDs for the DB subnet group.
modifyDBSubnetGroup_subnetIds :: Lens.Lens' ModifyDBSubnetGroup [Prelude.Text]
modifyDBSubnetGroup_subnetIds = Lens.lens (\ModifyDBSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyDBSubnetGroup' {} a -> s {subnetIds = a} :: ModifyDBSubnetGroup) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyDBSubnetGroup where
  type
    AWSResponse ModifyDBSubnetGroup =
      ModifyDBSubnetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBSubnetGroupResult"
      ( \s h x ->
          ModifyDBSubnetGroupResponse'
            Prelude.<$> (x Data..@? "DBSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBSubnetGroup where
  hashWithSalt _salt ModifyDBSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbSubnetGroupDescription
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData ModifyDBSubnetGroup where
  rnf ModifyDBSubnetGroup' {..} =
    Prelude.rnf dbSubnetGroupDescription `Prelude.seq`
      Prelude.rnf dbSubnetGroupName `Prelude.seq`
        Prelude.rnf subnetIds

instance Data.ToHeaders ModifyDBSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBSubnetGroup where
  toQuery ModifyDBSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBSubnetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSubnetGroupDescription"
          Data.=: dbSubnetGroupDescription,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "SubnetIds"
          Data.=: Data.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newModifyDBSubnetGroupResponse' smart constructor.
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
  { dbSubnetGroup :: Prelude.Maybe DBSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyDBSubnetGroupResponse
newModifyDBSubnetGroupResponse pHttpStatus_ =
  ModifyDBSubnetGroupResponse'
    { dbSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBSubnetGroupResponse_dbSubnetGroup :: Lens.Lens' ModifyDBSubnetGroupResponse (Prelude.Maybe DBSubnetGroup)
modifyDBSubnetGroupResponse_dbSubnetGroup = Lens.lens (\ModifyDBSubnetGroupResponse' {dbSubnetGroup} -> dbSubnetGroup) (\s@ModifyDBSubnetGroupResponse' {} a -> s {dbSubnetGroup = a} :: ModifyDBSubnetGroupResponse)

-- | The response's http status code.
modifyDBSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyDBSubnetGroupResponse Prelude.Int
modifyDBSubnetGroupResponse_httpStatus = Lens.lens (\ModifyDBSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyDBSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyDBSubnetGroupResponse)

instance Prelude.NFData ModifyDBSubnetGroupResponse where
  rnf ModifyDBSubnetGroupResponse' {..} =
    Prelude.rnf dbSubnetGroup `Prelude.seq`
      Prelude.rnf httpStatus
