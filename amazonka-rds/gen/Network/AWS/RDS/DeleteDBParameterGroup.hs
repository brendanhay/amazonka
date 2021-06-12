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
-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB parameter group. The DB parameter group to be
-- deleted can\'t be associated with any DB instances.
module Network.AWS.RDS.DeleteDBParameterGroup
  ( -- * Creating a Request
    DeleteDBParameterGroup (..),
    newDeleteDBParameterGroup,

    -- * Request Lenses
    deleteDBParameterGroup_dbParameterGroupName,

    -- * Destructuring the Response
    DeleteDBParameterGroupResponse (..),
    newDeleteDBParameterGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteDBParameterGroup' smart constructor.
data DeleteDBParameterGroup = DeleteDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    -- -   Must be the name of an existing DB parameter group
    --
    -- -   You can\'t delete a default DB parameter group
    --
    -- -   Can\'t be associated with any DB instances
    dbParameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'deleteDBParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
--
-- -   You can\'t delete a default DB parameter group
--
-- -   Can\'t be associated with any DB instances
newDeleteDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Core.Text ->
  DeleteDBParameterGroup
newDeleteDBParameterGroup pDBParameterGroupName_ =
  DeleteDBParameterGroup'
    { dbParameterGroupName =
        pDBParameterGroupName_
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
--
-- -   You can\'t delete a default DB parameter group
--
-- -   Can\'t be associated with any DB instances
deleteDBParameterGroup_dbParameterGroupName :: Lens.Lens' DeleteDBParameterGroup Core.Text
deleteDBParameterGroup_dbParameterGroupName = Lens.lens (\DeleteDBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@DeleteDBParameterGroup' {} a -> s {dbParameterGroupName = a} :: DeleteDBParameterGroup)

instance Core.AWSRequest DeleteDBParameterGroup where
  type
    AWSResponse DeleteDBParameterGroup =
      DeleteDBParameterGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteDBParameterGroupResponse'

instance Core.Hashable DeleteDBParameterGroup

instance Core.NFData DeleteDBParameterGroup

instance Core.ToHeaders DeleteDBParameterGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDBParameterGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDBParameterGroup where
  toQuery DeleteDBParameterGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteDBParameterGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBParameterGroupName" Core.=: dbParameterGroupName
      ]

-- | /See:/ 'newDeleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDBParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBParameterGroupResponse ::
  DeleteDBParameterGroupResponse
newDeleteDBParameterGroupResponse =
  DeleteDBParameterGroupResponse'

instance Core.NFData DeleteDBParameterGroupResponse
