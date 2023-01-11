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
-- Module      : Amazonka.Neptune.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted
-- can\'t be associated with any DB instances.
module Amazonka.Neptune.DeleteDBParameterGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBParameterGroup' smart constructor.
data DeleteDBParameterGroup = DeleteDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    -- -   Must be the name of an existing DB parameter group
    --
    -- -   You can\'t delete a default DB parameter group
    --
    -- -   Cannot be associated with any DB instances
    dbParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   Cannot be associated with any DB instances
newDeleteDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Prelude.Text ->
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
-- -   Cannot be associated with any DB instances
deleteDBParameterGroup_dbParameterGroupName :: Lens.Lens' DeleteDBParameterGroup Prelude.Text
deleteDBParameterGroup_dbParameterGroupName = Lens.lens (\DeleteDBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@DeleteDBParameterGroup' {} a -> s {dbParameterGroupName = a} :: DeleteDBParameterGroup)

instance Core.AWSRequest DeleteDBParameterGroup where
  type
    AWSResponse DeleteDBParameterGroup =
      DeleteDBParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDBParameterGroupResponse'

instance Prelude.Hashable DeleteDBParameterGroup where
  hashWithSalt _salt DeleteDBParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName

instance Prelude.NFData DeleteDBParameterGroup where
  rnf DeleteDBParameterGroup' {..} =
    Prelude.rnf dbParameterGroupName

instance Data.ToHeaders DeleteDBParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBParameterGroup where
  toQuery DeleteDBParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBParameterGroupName" Data.=: dbParameterGroupName
      ]

-- | /See:/ 'newDeleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBParameterGroupResponse ::
  DeleteDBParameterGroupResponse
newDeleteDBParameterGroupResponse =
  DeleteDBParameterGroupResponse'

instance
  Prelude.NFData
    DeleteDBParameterGroupResponse
  where
  rnf _ = ()
