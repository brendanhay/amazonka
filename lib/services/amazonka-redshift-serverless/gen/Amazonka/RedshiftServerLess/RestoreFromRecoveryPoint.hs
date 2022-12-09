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
-- Module      : Amazonka.RedshiftServerLess.RestoreFromRecoveryPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restore the data from a recovery point.
module Amazonka.RedshiftServerLess.RestoreFromRecoveryPoint
  ( -- * Creating a Request
    RestoreFromRecoveryPoint (..),
    newRestoreFromRecoveryPoint,

    -- * Request Lenses
    restoreFromRecoveryPoint_namespaceName,
    restoreFromRecoveryPoint_recoveryPointId,
    restoreFromRecoveryPoint_workgroupName,

    -- * Destructuring the Response
    RestoreFromRecoveryPointResponse (..),
    newRestoreFromRecoveryPointResponse,

    -- * Response Lenses
    restoreFromRecoveryPointResponse_namespace,
    restoreFromRecoveryPointResponse_recoveryPointId,
    restoreFromRecoveryPointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreFromRecoveryPoint' smart constructor.
data RestoreFromRecoveryPoint = RestoreFromRecoveryPoint'
  { -- | The name of the namespace to restore data into.
    namespaceName :: Prelude.Text,
    -- | The unique identifier of the recovery point to restore from.
    recoveryPointId :: Prelude.Text,
    -- | The name of the workgroup used to restore data.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceName', 'restoreFromRecoveryPoint_namespaceName' - The name of the namespace to restore data into.
--
-- 'recoveryPointId', 'restoreFromRecoveryPoint_recoveryPointId' - The unique identifier of the recovery point to restore from.
--
-- 'workgroupName', 'restoreFromRecoveryPoint_workgroupName' - The name of the workgroup used to restore data.
newRestoreFromRecoveryPoint ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'recoveryPointId'
  Prelude.Text ->
  -- | 'workgroupName'
  Prelude.Text ->
  RestoreFromRecoveryPoint
newRestoreFromRecoveryPoint
  pNamespaceName_
  pRecoveryPointId_
  pWorkgroupName_ =
    RestoreFromRecoveryPoint'
      { namespaceName =
          pNamespaceName_,
        recoveryPointId = pRecoveryPointId_,
        workgroupName = pWorkgroupName_
      }

-- | The name of the namespace to restore data into.
restoreFromRecoveryPoint_namespaceName :: Lens.Lens' RestoreFromRecoveryPoint Prelude.Text
restoreFromRecoveryPoint_namespaceName = Lens.lens (\RestoreFromRecoveryPoint' {namespaceName} -> namespaceName) (\s@RestoreFromRecoveryPoint' {} a -> s {namespaceName = a} :: RestoreFromRecoveryPoint)

-- | The unique identifier of the recovery point to restore from.
restoreFromRecoveryPoint_recoveryPointId :: Lens.Lens' RestoreFromRecoveryPoint Prelude.Text
restoreFromRecoveryPoint_recoveryPointId = Lens.lens (\RestoreFromRecoveryPoint' {recoveryPointId} -> recoveryPointId) (\s@RestoreFromRecoveryPoint' {} a -> s {recoveryPointId = a} :: RestoreFromRecoveryPoint)

-- | The name of the workgroup used to restore data.
restoreFromRecoveryPoint_workgroupName :: Lens.Lens' RestoreFromRecoveryPoint Prelude.Text
restoreFromRecoveryPoint_workgroupName = Lens.lens (\RestoreFromRecoveryPoint' {workgroupName} -> workgroupName) (\s@RestoreFromRecoveryPoint' {} a -> s {workgroupName = a} :: RestoreFromRecoveryPoint)

instance Core.AWSRequest RestoreFromRecoveryPoint where
  type
    AWSResponse RestoreFromRecoveryPoint =
      RestoreFromRecoveryPointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreFromRecoveryPointResponse'
            Prelude.<$> (x Data..?> "namespace")
            Prelude.<*> (x Data..?> "recoveryPointId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreFromRecoveryPoint where
  hashWithSalt _salt RestoreFromRecoveryPoint' {..} =
    _salt `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` recoveryPointId
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData RestoreFromRecoveryPoint where
  rnf RestoreFromRecoveryPoint' {..} =
    Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf recoveryPointId
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders RestoreFromRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.RestoreFromRecoveryPoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreFromRecoveryPoint where
  toJSON RestoreFromRecoveryPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("namespaceName" Data..= namespaceName),
            Prelude.Just
              ("recoveryPointId" Data..= recoveryPointId),
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath RestoreFromRecoveryPoint where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreFromRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreFromRecoveryPointResponse' smart constructor.
data RestoreFromRecoveryPointResponse = RestoreFromRecoveryPointResponse'
  { -- | The namespace that data was restored into.
    namespace :: Prelude.Maybe Namespace,
    -- | The unique identifier of the recovery point used for the restore.
    recoveryPointId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'restoreFromRecoveryPointResponse_namespace' - The namespace that data was restored into.
--
-- 'recoveryPointId', 'restoreFromRecoveryPointResponse_recoveryPointId' - The unique identifier of the recovery point used for the restore.
--
-- 'httpStatus', 'restoreFromRecoveryPointResponse_httpStatus' - The response's http status code.
newRestoreFromRecoveryPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreFromRecoveryPointResponse
newRestoreFromRecoveryPointResponse pHttpStatus_ =
  RestoreFromRecoveryPointResponse'
    { namespace =
        Prelude.Nothing,
      recoveryPointId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The namespace that data was restored into.
restoreFromRecoveryPointResponse_namespace :: Lens.Lens' RestoreFromRecoveryPointResponse (Prelude.Maybe Namespace)
restoreFromRecoveryPointResponse_namespace = Lens.lens (\RestoreFromRecoveryPointResponse' {namespace} -> namespace) (\s@RestoreFromRecoveryPointResponse' {} a -> s {namespace = a} :: RestoreFromRecoveryPointResponse)

-- | The unique identifier of the recovery point used for the restore.
restoreFromRecoveryPointResponse_recoveryPointId :: Lens.Lens' RestoreFromRecoveryPointResponse (Prelude.Maybe Prelude.Text)
restoreFromRecoveryPointResponse_recoveryPointId = Lens.lens (\RestoreFromRecoveryPointResponse' {recoveryPointId} -> recoveryPointId) (\s@RestoreFromRecoveryPointResponse' {} a -> s {recoveryPointId = a} :: RestoreFromRecoveryPointResponse)

-- | The response's http status code.
restoreFromRecoveryPointResponse_httpStatus :: Lens.Lens' RestoreFromRecoveryPointResponse Prelude.Int
restoreFromRecoveryPointResponse_httpStatus = Lens.lens (\RestoreFromRecoveryPointResponse' {httpStatus} -> httpStatus) (\s@RestoreFromRecoveryPointResponse' {} a -> s {httpStatus = a} :: RestoreFromRecoveryPointResponse)

instance
  Prelude.NFData
    RestoreFromRecoveryPointResponse
  where
  rnf RestoreFromRecoveryPointResponse' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf recoveryPointId
      `Prelude.seq` Prelude.rnf httpStatus
