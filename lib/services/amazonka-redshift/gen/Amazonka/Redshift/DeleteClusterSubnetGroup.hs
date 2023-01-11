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
-- Module      : Amazonka.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster subnet group.
module Amazonka.Redshift.DeleteClusterSubnetGroup
  ( -- * Creating a Request
    DeleteClusterSubnetGroup (..),
    newDeleteClusterSubnetGroup,

    -- * Request Lenses
    deleteClusterSubnetGroup_clusterSubnetGroupName,

    -- * Destructuring the Response
    DeleteClusterSubnetGroupResponse (..),
    newDeleteClusterSubnetGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteClusterSubnetGroup' smart constructor.
data DeleteClusterSubnetGroup = DeleteClusterSubnetGroup'
  { -- | The name of the cluster subnet group name to be deleted.
    clusterSubnetGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroupName', 'deleteClusterSubnetGroup_clusterSubnetGroupName' - The name of the cluster subnet group name to be deleted.
newDeleteClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Prelude.Text ->
  DeleteClusterSubnetGroup
newDeleteClusterSubnetGroup pClusterSubnetGroupName_ =
  DeleteClusterSubnetGroup'
    { clusterSubnetGroupName =
        pClusterSubnetGroupName_
    }

-- | The name of the cluster subnet group name to be deleted.
deleteClusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' DeleteClusterSubnetGroup Prelude.Text
deleteClusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\DeleteClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@DeleteClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: DeleteClusterSubnetGroup)

instance Core.AWSRequest DeleteClusterSubnetGroup where
  type
    AWSResponse DeleteClusterSubnetGroup =
      DeleteClusterSubnetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteClusterSubnetGroupResponse'

instance Prelude.Hashable DeleteClusterSubnetGroup where
  hashWithSalt _salt DeleteClusterSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` clusterSubnetGroupName

instance Prelude.NFData DeleteClusterSubnetGroup where
  rnf DeleteClusterSubnetGroup' {..} =
    Prelude.rnf clusterSubnetGroupName

instance Data.ToHeaders DeleteClusterSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteClusterSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteClusterSubnetGroup where
  toQuery DeleteClusterSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteClusterSubnetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterSubnetGroupName"
          Data.=: clusterSubnetGroupName
      ]

-- | /See:/ 'newDeleteClusterSubnetGroupResponse' smart constructor.
data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteClusterSubnetGroupResponse ::
  DeleteClusterSubnetGroupResponse
newDeleteClusterSubnetGroupResponse =
  DeleteClusterSubnetGroupResponse'

instance
  Prelude.NFData
    DeleteClusterSubnetGroupResponse
  where
  rnf _ = ()
