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
-- Module      : Amazonka.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified Amazon Redshift parameter group.
--
-- You cannot delete a parameter group if it is associated with a cluster.
module Amazonka.Redshift.DeleteClusterParameterGroup
  ( -- * Creating a Request
    DeleteClusterParameterGroup (..),
    newDeleteClusterParameterGroup,

    -- * Request Lenses
    deleteClusterParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    DeleteClusterParameterGroupResponse (..),
    newDeleteClusterParameterGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteClusterParameterGroup' smart constructor.
data DeleteClusterParameterGroup = DeleteClusterParameterGroup'
  { -- | The name of the parameter group to be deleted.
    --
    -- Constraints:
    --
    -- -   Must be the name of an existing cluster parameter group.
    --
    -- -   Cannot delete a default cluster parameter group.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'deleteClusterParameterGroup_parameterGroupName' - The name of the parameter group to be deleted.
--
-- Constraints:
--
-- -   Must be the name of an existing cluster parameter group.
--
-- -   Cannot delete a default cluster parameter group.
newDeleteClusterParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  DeleteClusterParameterGroup
newDeleteClusterParameterGroup pParameterGroupName_ =
  DeleteClusterParameterGroup'
    { parameterGroupName =
        pParameterGroupName_
    }

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
-- -   Must be the name of an existing cluster parameter group.
--
-- -   Cannot delete a default cluster parameter group.
deleteClusterParameterGroup_parameterGroupName :: Lens.Lens' DeleteClusterParameterGroup Prelude.Text
deleteClusterParameterGroup_parameterGroupName = Lens.lens (\DeleteClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@DeleteClusterParameterGroup' {} a -> s {parameterGroupName = a} :: DeleteClusterParameterGroup)

instance Core.AWSRequest DeleteClusterParameterGroup where
  type
    AWSResponse DeleteClusterParameterGroup =
      DeleteClusterParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteClusterParameterGroupResponse'

instance Prelude.Hashable DeleteClusterParameterGroup where
  hashWithSalt _salt DeleteClusterParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData DeleteClusterParameterGroup where
  rnf DeleteClusterParameterGroup' {..} =
    Prelude.rnf parameterGroupName

instance Core.ToHeaders DeleteClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteClusterParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteClusterParameterGroup where
  toQuery DeleteClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ParameterGroupName" Core.=: parameterGroupName
      ]

-- | /See:/ 'newDeleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteClusterParameterGroupResponse ::
  DeleteClusterParameterGroupResponse
newDeleteClusterParameterGroupResponse =
  DeleteClusterParameterGroupResponse'

instance
  Prelude.NFData
    DeleteClusterParameterGroupResponse
  where
  rnf _ = ()
