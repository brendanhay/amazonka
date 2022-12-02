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
-- Module      : Amazonka.SSMSAP.DeleteResourcePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes permissions associated with the target database.
module Amazonka.SSMSAP.DeleteResourcePermission
  ( -- * Creating a Request
    DeleteResourcePermission (..),
    newDeleteResourcePermission,

    -- * Request Lenses
    deleteResourcePermission_actionType,
    deleteResourcePermission_sourceResourceArn,
    deleteResourcePermission_resourceArn,

    -- * Destructuring the Response
    DeleteResourcePermissionResponse (..),
    newDeleteResourcePermissionResponse,

    -- * Response Lenses
    deleteResourcePermissionResponse_policy,
    deleteResourcePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newDeleteResourcePermission' smart constructor.
data DeleteResourcePermission = DeleteResourcePermission'
  { actionType :: Prelude.Maybe PermissionActionType,
    sourceResourceArn :: Prelude.Maybe Prelude.Text,
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'deleteResourcePermission_actionType' -
--
-- 'sourceResourceArn', 'deleteResourcePermission_sourceResourceArn' -
--
-- 'resourceArn', 'deleteResourcePermission_resourceArn' -
newDeleteResourcePermission ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteResourcePermission
newDeleteResourcePermission pResourceArn_ =
  DeleteResourcePermission'
    { actionType =
        Prelude.Nothing,
      sourceResourceArn = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- |
deleteResourcePermission_actionType :: Lens.Lens' DeleteResourcePermission (Prelude.Maybe PermissionActionType)
deleteResourcePermission_actionType = Lens.lens (\DeleteResourcePermission' {actionType} -> actionType) (\s@DeleteResourcePermission' {} a -> s {actionType = a} :: DeleteResourcePermission)

-- |
deleteResourcePermission_sourceResourceArn :: Lens.Lens' DeleteResourcePermission (Prelude.Maybe Prelude.Text)
deleteResourcePermission_sourceResourceArn = Lens.lens (\DeleteResourcePermission' {sourceResourceArn} -> sourceResourceArn) (\s@DeleteResourcePermission' {} a -> s {sourceResourceArn = a} :: DeleteResourcePermission)

-- |
deleteResourcePermission_resourceArn :: Lens.Lens' DeleteResourcePermission Prelude.Text
deleteResourcePermission_resourceArn = Lens.lens (\DeleteResourcePermission' {resourceArn} -> resourceArn) (\s@DeleteResourcePermission' {} a -> s {resourceArn = a} :: DeleteResourcePermission)

instance Core.AWSRequest DeleteResourcePermission where
  type
    AWSResponse DeleteResourcePermission =
      DeleteResourcePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourcePermissionResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePermission where
  hashWithSalt _salt DeleteResourcePermission' {..} =
    _salt `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` sourceResourceArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteResourcePermission where
  rnf DeleteResourcePermission' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf sourceResourceArn
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders DeleteResourcePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourcePermission where
  toJSON DeleteResourcePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionType" Data..=) Prelude.<$> actionType,
            ("SourceResourceArn" Data..=)
              Prelude.<$> sourceResourceArn,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath DeleteResourcePermission where
  toPath = Prelude.const "/delete-resource-permission"

instance Data.ToQuery DeleteResourcePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcePermissionResponse' smart constructor.
data DeleteResourcePermissionResponse = DeleteResourcePermissionResponse'
  { policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'deleteResourcePermissionResponse_policy' -
--
-- 'httpStatus', 'deleteResourcePermissionResponse_httpStatus' - The response's http status code.
newDeleteResourcePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePermissionResponse
newDeleteResourcePermissionResponse pHttpStatus_ =
  DeleteResourcePermissionResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
deleteResourcePermissionResponse_policy :: Lens.Lens' DeleteResourcePermissionResponse (Prelude.Maybe Prelude.Text)
deleteResourcePermissionResponse_policy = Lens.lens (\DeleteResourcePermissionResponse' {policy} -> policy) (\s@DeleteResourcePermissionResponse' {} a -> s {policy = a} :: DeleteResourcePermissionResponse)

-- | The response's http status code.
deleteResourcePermissionResponse_httpStatus :: Lens.Lens' DeleteResourcePermissionResponse Prelude.Int
deleteResourcePermissionResponse_httpStatus = Lens.lens (\DeleteResourcePermissionResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePermissionResponse' {} a -> s {httpStatus = a} :: DeleteResourcePermissionResponse)

instance
  Prelude.NFData
    DeleteResourcePermissionResponse
  where
  rnf DeleteResourcePermissionResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
