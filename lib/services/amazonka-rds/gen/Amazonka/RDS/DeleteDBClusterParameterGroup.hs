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
-- Module      : Amazonka.RDS.DeleteDBClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB cluster parameter group. The DB cluster parameter
-- group to be deleted can\'t be associated with any DB clusters.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ DB cluster deployments>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.DeleteDBClusterParameterGroup
  ( -- * Creating a Request
    DeleteDBClusterParameterGroup (..),
    newDeleteDBClusterParameterGroup,

    -- * Request Lenses
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- * Destructuring the Response
    DeleteDBClusterParameterGroupResponse (..),
    newDeleteDBClusterParameterGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteDBClusterParameterGroup' smart constructor.
data DeleteDBClusterParameterGroup = DeleteDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    -- -   Must be the name of an existing DB cluster parameter group.
    --
    -- -   You can\'t delete a default DB cluster parameter group.
    --
    -- -   Can\'t be associated with any DB clusters.
    dbClusterParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroupName', 'deleteDBClusterParameterGroup_dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB cluster parameter group.
--
-- -   You can\'t delete a default DB cluster parameter group.
--
-- -   Can\'t be associated with any DB clusters.
newDeleteDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Prelude.Text ->
  DeleteDBClusterParameterGroup
newDeleteDBClusterParameterGroup
  pDBClusterParameterGroupName_ =
    DeleteDBClusterParameterGroup'
      { dbClusterParameterGroupName =
          pDBClusterParameterGroupName_
      }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB cluster parameter group.
--
-- -   You can\'t delete a default DB cluster parameter group.
--
-- -   Can\'t be associated with any DB clusters.
deleteDBClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' DeleteDBClusterParameterGroup Prelude.Text
deleteDBClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\DeleteDBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DeleteDBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: DeleteDBClusterParameterGroup)

instance
  Core.AWSRequest
    DeleteDBClusterParameterGroup
  where
  type
    AWSResponse DeleteDBClusterParameterGroup =
      DeleteDBClusterParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDBClusterParameterGroupResponse'

instance
  Prelude.Hashable
    DeleteDBClusterParameterGroup
  where
  hashWithSalt _salt DeleteDBClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupName

instance Prelude.NFData DeleteDBClusterParameterGroup where
  rnf DeleteDBClusterParameterGroup' {..} =
    Prelude.rnf dbClusterParameterGroupName

instance Data.ToHeaders DeleteDBClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBClusterParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBClusterParameterGroup where
  toQuery DeleteDBClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteDBClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName
      ]

-- | /See:/ 'newDeleteDBClusterParameterGroupResponse' smart constructor.
data DeleteDBClusterParameterGroupResponse = DeleteDBClusterParameterGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBClusterParameterGroupResponse ::
  DeleteDBClusterParameterGroupResponse
newDeleteDBClusterParameterGroupResponse =
  DeleteDBClusterParameterGroupResponse'

instance
  Prelude.NFData
    DeleteDBClusterParameterGroupResponse
  where
  rnf _ = ()
