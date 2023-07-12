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
-- Module      : Amazonka.DAX.DeleteParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified parameter group. You cannot delete a parameter
-- group if it is associated with any DAX clusters.
module Amazonka.DAX.DeleteParameterGroup
  ( -- * Creating a Request
    DeleteParameterGroup (..),
    newDeleteParameterGroup,

    -- * Request Lenses
    deleteParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    DeleteParameterGroupResponse (..),
    newDeleteParameterGroupResponse,

    -- * Response Lenses
    deleteParameterGroupResponse_deletionMessage,
    deleteParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteParameterGroup' smart constructor.
data DeleteParameterGroup = DeleteParameterGroup'
  { -- | The name of the parameter group to delete.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'deleteParameterGroup_parameterGroupName' - The name of the parameter group to delete.
newDeleteParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  DeleteParameterGroup
newDeleteParameterGroup pParameterGroupName_ =
  DeleteParameterGroup'
    { parameterGroupName =
        pParameterGroupName_
    }

-- | The name of the parameter group to delete.
deleteParameterGroup_parameterGroupName :: Lens.Lens' DeleteParameterGroup Prelude.Text
deleteParameterGroup_parameterGroupName = Lens.lens (\DeleteParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@DeleteParameterGroup' {} a -> s {parameterGroupName = a} :: DeleteParameterGroup)

instance Core.AWSRequest DeleteParameterGroup where
  type
    AWSResponse DeleteParameterGroup =
      DeleteParameterGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParameterGroupResponse'
            Prelude.<$> (x Data..?> "DeletionMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParameterGroup where
  hashWithSalt _salt DeleteParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData DeleteParameterGroup where
  rnf DeleteParameterGroup' {..} =
    Prelude.rnf parameterGroupName

instance Data.ToHeaders DeleteParameterGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDAXV3.DeleteParameterGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteParameterGroup where
  toJSON DeleteParameterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParameterGroupName" Data..= parameterGroupName)
          ]
      )

instance Data.ToPath DeleteParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteParameterGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParameterGroupResponse' smart constructor.
data DeleteParameterGroupResponse = DeleteParameterGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting
    -- the parameter group).
    deletionMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionMessage', 'deleteParameterGroupResponse_deletionMessage' - A user-specified message for this action (i.e., a reason for deleting
-- the parameter group).
--
-- 'httpStatus', 'deleteParameterGroupResponse_httpStatus' - The response's http status code.
newDeleteParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParameterGroupResponse
newDeleteParameterGroupResponse pHttpStatus_ =
  DeleteParameterGroupResponse'
    { deletionMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting
-- the parameter group).
deleteParameterGroupResponse_deletionMessage :: Lens.Lens' DeleteParameterGroupResponse (Prelude.Maybe Prelude.Text)
deleteParameterGroupResponse_deletionMessage = Lens.lens (\DeleteParameterGroupResponse' {deletionMessage} -> deletionMessage) (\s@DeleteParameterGroupResponse' {} a -> s {deletionMessage = a} :: DeleteParameterGroupResponse)

-- | The response's http status code.
deleteParameterGroupResponse_httpStatus :: Lens.Lens' DeleteParameterGroupResponse Prelude.Int
deleteParameterGroupResponse_httpStatus = Lens.lens (\DeleteParameterGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteParameterGroupResponse' {} a -> s {httpStatus = a} :: DeleteParameterGroupResponse)

instance Prelude.NFData DeleteParameterGroupResponse where
  rnf DeleteParameterGroupResponse' {..} =
    Prelude.rnf deletionMessage
      `Prelude.seq` Prelude.rnf httpStatus
