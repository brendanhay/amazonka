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
-- Module      : Amazonka.WellArchitected.UpdateWorkloadShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a workload share.
module Amazonka.WellArchitected.UpdateWorkloadShare
  ( -- * Creating a Request
    UpdateWorkloadShare (..),
    newUpdateWorkloadShare,

    -- * Request Lenses
    updateWorkloadShare_shareId,
    updateWorkloadShare_workloadId,
    updateWorkloadShare_permissionType,

    -- * Destructuring the Response
    UpdateWorkloadShareResponse (..),
    newUpdateWorkloadShareResponse,

    -- * Response Lenses
    updateWorkloadShareResponse_workloadId,
    updateWorkloadShareResponse_workloadShare,
    updateWorkloadShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for Update Workload Share
--
-- /See:/ 'newUpdateWorkloadShare' smart constructor.
data UpdateWorkloadShare = UpdateWorkloadShare'
  { shareId :: Prelude.Text,
    workloadId :: Prelude.Text,
    permissionType :: PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkloadShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'updateWorkloadShare_shareId' - Undocumented member.
--
-- 'workloadId', 'updateWorkloadShare_workloadId' - Undocumented member.
--
-- 'permissionType', 'updateWorkloadShare_permissionType' - Undocumented member.
newUpdateWorkloadShare ::
  -- | 'shareId'
  Prelude.Text ->
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'permissionType'
  PermissionType ->
  UpdateWorkloadShare
newUpdateWorkloadShare
  pShareId_
  pWorkloadId_
  pPermissionType_ =
    UpdateWorkloadShare'
      { shareId = pShareId_,
        workloadId = pWorkloadId_,
        permissionType = pPermissionType_
      }

-- | Undocumented member.
updateWorkloadShare_shareId :: Lens.Lens' UpdateWorkloadShare Prelude.Text
updateWorkloadShare_shareId = Lens.lens (\UpdateWorkloadShare' {shareId} -> shareId) (\s@UpdateWorkloadShare' {} a -> s {shareId = a} :: UpdateWorkloadShare)

-- | Undocumented member.
updateWorkloadShare_workloadId :: Lens.Lens' UpdateWorkloadShare Prelude.Text
updateWorkloadShare_workloadId = Lens.lens (\UpdateWorkloadShare' {workloadId} -> workloadId) (\s@UpdateWorkloadShare' {} a -> s {workloadId = a} :: UpdateWorkloadShare)

-- | Undocumented member.
updateWorkloadShare_permissionType :: Lens.Lens' UpdateWorkloadShare PermissionType
updateWorkloadShare_permissionType = Lens.lens (\UpdateWorkloadShare' {permissionType} -> permissionType) (\s@UpdateWorkloadShare' {} a -> s {permissionType = a} :: UpdateWorkloadShare)

instance Core.AWSRequest UpdateWorkloadShare where
  type
    AWSResponse UpdateWorkloadShare =
      UpdateWorkloadShareResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkloadShareResponse'
            Prelude.<$> (x Data..?> "WorkloadId")
            Prelude.<*> (x Data..?> "WorkloadShare")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkloadShare where
  hashWithSalt _salt UpdateWorkloadShare' {..} =
    _salt
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` permissionType

instance Prelude.NFData UpdateWorkloadShare where
  rnf UpdateWorkloadShare' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf permissionType

instance Data.ToHeaders UpdateWorkloadShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkloadShare where
  toJSON UpdateWorkloadShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PermissionType" Data..= permissionType)
          ]
      )

instance Data.ToPath UpdateWorkloadShare where
  toPath UpdateWorkloadShare' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/shares/",
        Data.toBS shareId
      ]

instance Data.ToQuery UpdateWorkloadShare where
  toQuery = Prelude.const Prelude.mempty

-- | Input for Update Workload Share
--
-- /See:/ 'newUpdateWorkloadShareResponse' smart constructor.
data UpdateWorkloadShareResponse = UpdateWorkloadShareResponse'
  { workloadId :: Prelude.Maybe Prelude.Text,
    workloadShare :: Prelude.Maybe WorkloadShare,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkloadShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'updateWorkloadShareResponse_workloadId' - Undocumented member.
--
-- 'workloadShare', 'updateWorkloadShareResponse_workloadShare' - Undocumented member.
--
-- 'httpStatus', 'updateWorkloadShareResponse_httpStatus' - The response's http status code.
newUpdateWorkloadShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkloadShareResponse
newUpdateWorkloadShareResponse pHttpStatus_ =
  UpdateWorkloadShareResponse'
    { workloadId =
        Prelude.Nothing,
      workloadShare = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateWorkloadShareResponse_workloadId :: Lens.Lens' UpdateWorkloadShareResponse (Prelude.Maybe Prelude.Text)
updateWorkloadShareResponse_workloadId = Lens.lens (\UpdateWorkloadShareResponse' {workloadId} -> workloadId) (\s@UpdateWorkloadShareResponse' {} a -> s {workloadId = a} :: UpdateWorkloadShareResponse)

-- | Undocumented member.
updateWorkloadShareResponse_workloadShare :: Lens.Lens' UpdateWorkloadShareResponse (Prelude.Maybe WorkloadShare)
updateWorkloadShareResponse_workloadShare = Lens.lens (\UpdateWorkloadShareResponse' {workloadShare} -> workloadShare) (\s@UpdateWorkloadShareResponse' {} a -> s {workloadShare = a} :: UpdateWorkloadShareResponse)

-- | The response's http status code.
updateWorkloadShareResponse_httpStatus :: Lens.Lens' UpdateWorkloadShareResponse Prelude.Int
updateWorkloadShareResponse_httpStatus = Lens.lens (\UpdateWorkloadShareResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkloadShareResponse' {} a -> s {httpStatus = a} :: UpdateWorkloadShareResponse)

instance Prelude.NFData UpdateWorkloadShareResponse where
  rnf UpdateWorkloadShareResponse' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadShare
      `Prelude.seq` Prelude.rnf httpStatus
