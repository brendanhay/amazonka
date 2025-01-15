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
-- Module      : Amazonka.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup
-- cannot be deleted.
module Amazonka.Athena.DeleteWorkGroup
  ( -- * Creating a Request
    DeleteWorkGroup (..),
    newDeleteWorkGroup,

    -- * Request Lenses
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,

    -- * Destructuring the Response
    DeleteWorkGroupResponse (..),
    newDeleteWorkGroupResponse,

    -- * Response Lenses
    deleteWorkGroupResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { -- | The option to delete the workgroup and its contents even if the
    -- workgroup contains any named queries or query executions.
    recursiveDeleteOption :: Prelude.Maybe Prelude.Bool,
    -- | The unique name of the workgroup to delete.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recursiveDeleteOption', 'deleteWorkGroup_recursiveDeleteOption' - The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
--
-- 'workGroup', 'deleteWorkGroup_workGroup' - The unique name of the workgroup to delete.
newDeleteWorkGroup ::
  -- | 'workGroup'
  Prelude.Text ->
  DeleteWorkGroup
newDeleteWorkGroup pWorkGroup_ =
  DeleteWorkGroup'
    { recursiveDeleteOption =
        Prelude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
deleteWorkGroup_recursiveDeleteOption :: Lens.Lens' DeleteWorkGroup (Prelude.Maybe Prelude.Bool)
deleteWorkGroup_recursiveDeleteOption = Lens.lens (\DeleteWorkGroup' {recursiveDeleteOption} -> recursiveDeleteOption) (\s@DeleteWorkGroup' {} a -> s {recursiveDeleteOption = a} :: DeleteWorkGroup)

-- | The unique name of the workgroup to delete.
deleteWorkGroup_workGroup :: Lens.Lens' DeleteWorkGroup Prelude.Text
deleteWorkGroup_workGroup = Lens.lens (\DeleteWorkGroup' {workGroup} -> workGroup) (\s@DeleteWorkGroup' {} a -> s {workGroup = a} :: DeleteWorkGroup)

instance Core.AWSRequest DeleteWorkGroup where
  type
    AWSResponse DeleteWorkGroup =
      DeleteWorkGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkGroup where
  hashWithSalt _salt DeleteWorkGroup' {..} =
    _salt
      `Prelude.hashWithSalt` recursiveDeleteOption
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData DeleteWorkGroup where
  rnf DeleteWorkGroup' {..} =
    Prelude.rnf recursiveDeleteOption `Prelude.seq`
      Prelude.rnf workGroup

instance Data.ToHeaders DeleteWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.DeleteWorkGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorkGroup where
  toJSON DeleteWorkGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecursiveDeleteOption" Data..=)
              Prelude.<$> recursiveDeleteOption,
            Prelude.Just ("WorkGroup" Data..= workGroup)
          ]
      )

instance Data.ToPath DeleteWorkGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkGroupResponse' smart constructor.
data DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkGroupResponse_httpStatus' - The response's http status code.
newDeleteWorkGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkGroupResponse
newDeleteWorkGroupResponse pHttpStatus_ =
  DeleteWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkGroupResponse_httpStatus :: Lens.Lens' DeleteWorkGroupResponse Prelude.Int
deleteWorkGroupResponse_httpStatus = Lens.lens (\DeleteWorkGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkGroupResponse' {} a -> s {httpStatus = a} :: DeleteWorkGroupResponse)

instance Prelude.NFData DeleteWorkGroupResponse where
  rnf DeleteWorkGroupResponse' {..} =
    Prelude.rnf httpStatus
