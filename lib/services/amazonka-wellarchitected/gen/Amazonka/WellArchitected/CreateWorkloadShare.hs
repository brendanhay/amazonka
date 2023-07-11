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
-- Module      : Amazonka.WellArchitected.CreateWorkloadShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a workload share.
--
-- The owner of a workload can share it with other Amazon Web Services
-- accounts and IAM users in the same Amazon Web Services Region. Shared
-- access to a workload is not removed until the workload invitation is
-- deleted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/workloads-sharing.html Sharing a Workload>
-- in the /Well-Architected Tool User Guide/.
module Amazonka.WellArchitected.CreateWorkloadShare
  ( -- * Creating a Request
    CreateWorkloadShare (..),
    newCreateWorkloadShare,

    -- * Request Lenses
    createWorkloadShare_workloadId,
    createWorkloadShare_sharedWith,
    createWorkloadShare_permissionType,
    createWorkloadShare_clientRequestToken,

    -- * Destructuring the Response
    CreateWorkloadShareResponse (..),
    newCreateWorkloadShareResponse,

    -- * Response Lenses
    createWorkloadShareResponse_shareId,
    createWorkloadShareResponse_workloadId,
    createWorkloadShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for Create Workload Share
--
-- /See:/ 'newCreateWorkloadShare' smart constructor.
data CreateWorkloadShare = CreateWorkloadShare'
  { workloadId :: Prelude.Text,
    sharedWith :: Prelude.Text,
    permissionType :: PermissionType,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkloadShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'createWorkloadShare_workloadId' - Undocumented member.
--
-- 'sharedWith', 'createWorkloadShare_sharedWith' - Undocumented member.
--
-- 'permissionType', 'createWorkloadShare_permissionType' - Undocumented member.
--
-- 'clientRequestToken', 'createWorkloadShare_clientRequestToken' - Undocumented member.
newCreateWorkloadShare ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'sharedWith'
  Prelude.Text ->
  -- | 'permissionType'
  PermissionType ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateWorkloadShare
newCreateWorkloadShare
  pWorkloadId_
  pSharedWith_
  pPermissionType_
  pClientRequestToken_ =
    CreateWorkloadShare'
      { workloadId = pWorkloadId_,
        sharedWith = pSharedWith_,
        permissionType = pPermissionType_,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
createWorkloadShare_workloadId :: Lens.Lens' CreateWorkloadShare Prelude.Text
createWorkloadShare_workloadId = Lens.lens (\CreateWorkloadShare' {workloadId} -> workloadId) (\s@CreateWorkloadShare' {} a -> s {workloadId = a} :: CreateWorkloadShare)

-- | Undocumented member.
createWorkloadShare_sharedWith :: Lens.Lens' CreateWorkloadShare Prelude.Text
createWorkloadShare_sharedWith = Lens.lens (\CreateWorkloadShare' {sharedWith} -> sharedWith) (\s@CreateWorkloadShare' {} a -> s {sharedWith = a} :: CreateWorkloadShare)

-- | Undocumented member.
createWorkloadShare_permissionType :: Lens.Lens' CreateWorkloadShare PermissionType
createWorkloadShare_permissionType = Lens.lens (\CreateWorkloadShare' {permissionType} -> permissionType) (\s@CreateWorkloadShare' {} a -> s {permissionType = a} :: CreateWorkloadShare)

-- | Undocumented member.
createWorkloadShare_clientRequestToken :: Lens.Lens' CreateWorkloadShare Prelude.Text
createWorkloadShare_clientRequestToken = Lens.lens (\CreateWorkloadShare' {clientRequestToken} -> clientRequestToken) (\s@CreateWorkloadShare' {} a -> s {clientRequestToken = a} :: CreateWorkloadShare)

instance Core.AWSRequest CreateWorkloadShare where
  type
    AWSResponse CreateWorkloadShare =
      CreateWorkloadShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkloadShareResponse'
            Prelude.<$> (x Data..?> "ShareId")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkloadShare where
  hashWithSalt _salt CreateWorkloadShare' {..} =
    _salt
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateWorkloadShare where
  rnf CreateWorkloadShare' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateWorkloadShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkloadShare where
  toJSON CreateWorkloadShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SharedWith" Data..= sharedWith),
            Prelude.Just
              ("PermissionType" Data..= permissionType),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateWorkloadShare where
  toPath CreateWorkloadShare' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId, "/shares"]

instance Data.ToQuery CreateWorkloadShare where
  toQuery = Prelude.const Prelude.mempty

-- | Input for Create Workload Share
--
-- /See:/ 'newCreateWorkloadShareResponse' smart constructor.
data CreateWorkloadShareResponse = CreateWorkloadShareResponse'
  { shareId :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkloadShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'createWorkloadShareResponse_shareId' - Undocumented member.
--
-- 'workloadId', 'createWorkloadShareResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'createWorkloadShareResponse_httpStatus' - The response's http status code.
newCreateWorkloadShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkloadShareResponse
newCreateWorkloadShareResponse pHttpStatus_ =
  CreateWorkloadShareResponse'
    { shareId =
        Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createWorkloadShareResponse_shareId :: Lens.Lens' CreateWorkloadShareResponse (Prelude.Maybe Prelude.Text)
createWorkloadShareResponse_shareId = Lens.lens (\CreateWorkloadShareResponse' {shareId} -> shareId) (\s@CreateWorkloadShareResponse' {} a -> s {shareId = a} :: CreateWorkloadShareResponse)

-- | Undocumented member.
createWorkloadShareResponse_workloadId :: Lens.Lens' CreateWorkloadShareResponse (Prelude.Maybe Prelude.Text)
createWorkloadShareResponse_workloadId = Lens.lens (\CreateWorkloadShareResponse' {workloadId} -> workloadId) (\s@CreateWorkloadShareResponse' {} a -> s {workloadId = a} :: CreateWorkloadShareResponse)

-- | The response's http status code.
createWorkloadShareResponse_httpStatus :: Lens.Lens' CreateWorkloadShareResponse Prelude.Int
createWorkloadShareResponse_httpStatus = Lens.lens (\CreateWorkloadShareResponse' {httpStatus} -> httpStatus) (\s@CreateWorkloadShareResponse' {} a -> s {httpStatus = a} :: CreateWorkloadShareResponse)

instance Prelude.NFData CreateWorkloadShareResponse where
  rnf CreateWorkloadShareResponse' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
