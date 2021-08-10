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
-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
--
-- The specified DB security group must not be associated with any DB
-- instances.
module Network.AWS.RDS.DeleteDBSecurityGroup
  ( -- * Creating a Request
    DeleteDBSecurityGroup (..),
    newDeleteDBSecurityGroup,

    -- * Request Lenses
    deleteDBSecurityGroup_dbSecurityGroupName,

    -- * Destructuring the Response
    DeleteDBSecurityGroupResponse (..),
    newDeleteDBSecurityGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteDBSecurityGroup' smart constructor.
data DeleteDBSecurityGroup = DeleteDBSecurityGroup'
  { -- | The name of the DB security group to delete.
    --
    -- You can\'t delete the default DB security group.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- -   Must not be \"Default\"
    dbSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupName', 'deleteDBSecurityGroup_dbSecurityGroupName' - The name of the DB security group to delete.
--
-- You can\'t delete the default DB security group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- -   Must not be \"Default\"
newDeleteDBSecurityGroup ::
  -- | 'dbSecurityGroupName'
  Prelude.Text ->
  DeleteDBSecurityGroup
newDeleteDBSecurityGroup pDBSecurityGroupName_ =
  DeleteDBSecurityGroup'
    { dbSecurityGroupName =
        pDBSecurityGroupName_
    }

-- | The name of the DB security group to delete.
--
-- You can\'t delete the default DB security group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- -   Must not be \"Default\"
deleteDBSecurityGroup_dbSecurityGroupName :: Lens.Lens' DeleteDBSecurityGroup Prelude.Text
deleteDBSecurityGroup_dbSecurityGroupName = Lens.lens (\DeleteDBSecurityGroup' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DeleteDBSecurityGroup' {} a -> s {dbSecurityGroupName = a} :: DeleteDBSecurityGroup)

instance Core.AWSRequest DeleteDBSecurityGroup where
  type
    AWSResponse DeleteDBSecurityGroup =
      DeleteDBSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteDBSecurityGroupResponse'

instance Prelude.Hashable DeleteDBSecurityGroup

instance Prelude.NFData DeleteDBSecurityGroup

instance Core.ToHeaders DeleteDBSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteDBSecurityGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDBSecurityGroup where
  toQuery DeleteDBSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteDBSecurityGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSecurityGroupName" Core.=: dbSecurityGroupName
      ]

-- | /See:/ 'newDeleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBSecurityGroupResponse ::
  DeleteDBSecurityGroupResponse
newDeleteDBSecurityGroupResponse =
  DeleteDBSecurityGroupResponse'

instance Prelude.NFData DeleteDBSecurityGroupResponse
