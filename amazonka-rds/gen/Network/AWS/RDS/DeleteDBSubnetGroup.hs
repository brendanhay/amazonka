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
-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB subnet group.
--
-- The specified database subnet group must not be associated with any DB
-- instances.
module Network.AWS.RDS.DeleteDBSubnetGroup
  ( -- * Creating a Request
    DeleteDBSubnetGroup (..),
    newDeleteDBSubnetGroup,

    -- * Request Lenses
    deleteDBSubnetGroup_dbSubnetGroupName,

    -- * Destructuring the Response
    DeleteDBSubnetGroupResponse (..),
    newDeleteDBSubnetGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteDBSubnetGroup' smart constructor.
data DeleteDBSubnetGroup = DeleteDBSubnetGroup'
  { -- | The name of the database subnet group to delete.
    --
    -- You can\'t delete the default subnet group.
    --
    -- Constraints:
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupName', 'deleteDBSubnetGroup_dbSubnetGroupName' - The name of the database subnet group to delete.
--
-- You can\'t delete the default subnet group.
--
-- Constraints:
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
newDeleteDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Core.Text ->
  DeleteDBSubnetGroup
newDeleteDBSubnetGroup pDBSubnetGroupName_ =
  DeleteDBSubnetGroup'
    { dbSubnetGroupName =
        pDBSubnetGroupName_
    }

-- | The name of the database subnet group to delete.
--
-- You can\'t delete the default subnet group.
--
-- Constraints:
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
deleteDBSubnetGroup_dbSubnetGroupName :: Lens.Lens' DeleteDBSubnetGroup Core.Text
deleteDBSubnetGroup_dbSubnetGroupName = Lens.lens (\DeleteDBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DeleteDBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: DeleteDBSubnetGroup)

instance Core.AWSRequest DeleteDBSubnetGroup where
  type
    AWSResponse DeleteDBSubnetGroup =
      DeleteDBSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteDBSubnetGroupResponse'

instance Core.Hashable DeleteDBSubnetGroup

instance Core.NFData DeleteDBSubnetGroup

instance Core.ToHeaders DeleteDBSubnetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDBSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDBSubnetGroup where
  toQuery DeleteDBSubnetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteDBSubnetGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSubnetGroupName" Core.=: dbSubnetGroupName
      ]

-- | /See:/ 'newDeleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDBSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBSubnetGroupResponse ::
  DeleteDBSubnetGroupResponse
newDeleteDBSubnetGroupResponse =
  DeleteDBSubnetGroupResponse'

instance Core.NFData DeleteDBSubnetGroupResponse
