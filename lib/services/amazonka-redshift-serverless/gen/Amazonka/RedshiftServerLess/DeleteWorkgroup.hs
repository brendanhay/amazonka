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
-- Module      : Amazonka.RedshiftServerLess.DeleteWorkgroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workgroup.
module Amazonka.RedshiftServerLess.DeleteWorkgroup
  ( -- * Creating a Request
    DeleteWorkgroup (..),
    newDeleteWorkgroup,

    -- * Request Lenses
    deleteWorkgroup_workgroupName,

    -- * Destructuring the Response
    DeleteWorkgroupResponse (..),
    newDeleteWorkgroupResponse,

    -- * Response Lenses
    deleteWorkgroupResponse_httpStatus,
    deleteWorkgroupResponse_workgroup,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkgroup' smart constructor.
data DeleteWorkgroup = DeleteWorkgroup'
  { -- | The name of the workgroup to be deleted.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkgroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workgroupName', 'deleteWorkgroup_workgroupName' - The name of the workgroup to be deleted.
newDeleteWorkgroup ::
  -- | 'workgroupName'
  Prelude.Text ->
  DeleteWorkgroup
newDeleteWorkgroup pWorkgroupName_ =
  DeleteWorkgroup' {workgroupName = pWorkgroupName_}

-- | The name of the workgroup to be deleted.
deleteWorkgroup_workgroupName :: Lens.Lens' DeleteWorkgroup Prelude.Text
deleteWorkgroup_workgroupName = Lens.lens (\DeleteWorkgroup' {workgroupName} -> workgroupName) (\s@DeleteWorkgroup' {} a -> s {workgroupName = a} :: DeleteWorkgroup)

instance Core.AWSRequest DeleteWorkgroup where
  type
    AWSResponse DeleteWorkgroup =
      DeleteWorkgroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkgroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workgroup")
      )

instance Prelude.Hashable DeleteWorkgroup where
  hashWithSalt _salt DeleteWorkgroup' {..} =
    _salt `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData DeleteWorkgroup where
  rnf DeleteWorkgroup' {..} = Prelude.rnf workgroupName

instance Data.ToHeaders DeleteWorkgroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.DeleteWorkgroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorkgroup where
  toJSON DeleteWorkgroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath DeleteWorkgroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWorkgroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkgroupResponse' smart constructor.
data DeleteWorkgroupResponse = DeleteWorkgroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The deleted workgroup object.
    workgroup :: Workgroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkgroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkgroupResponse_httpStatus' - The response's http status code.
--
-- 'workgroup', 'deleteWorkgroupResponse_workgroup' - The deleted workgroup object.
newDeleteWorkgroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workgroup'
  Workgroup ->
  DeleteWorkgroupResponse
newDeleteWorkgroupResponse pHttpStatus_ pWorkgroup_ =
  DeleteWorkgroupResponse'
    { httpStatus = pHttpStatus_,
      workgroup = pWorkgroup_
    }

-- | The response's http status code.
deleteWorkgroupResponse_httpStatus :: Lens.Lens' DeleteWorkgroupResponse Prelude.Int
deleteWorkgroupResponse_httpStatus = Lens.lens (\DeleteWorkgroupResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkgroupResponse' {} a -> s {httpStatus = a} :: DeleteWorkgroupResponse)

-- | The deleted workgroup object.
deleteWorkgroupResponse_workgroup :: Lens.Lens' DeleteWorkgroupResponse Workgroup
deleteWorkgroupResponse_workgroup = Lens.lens (\DeleteWorkgroupResponse' {workgroup} -> workgroup) (\s@DeleteWorkgroupResponse' {} a -> s {workgroup = a} :: DeleteWorkgroupResponse)

instance Prelude.NFData DeleteWorkgroupResponse where
  rnf DeleteWorkgroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workgroup
