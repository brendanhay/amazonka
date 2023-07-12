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
-- Module      : Amazonka.WellArchitected.DeleteWorkloadShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a workload share.
module Amazonka.WellArchitected.DeleteWorkloadShare
  ( -- * Creating a Request
    DeleteWorkloadShare (..),
    newDeleteWorkloadShare,

    -- * Request Lenses
    deleteWorkloadShare_shareId,
    deleteWorkloadShare_workloadId,
    deleteWorkloadShare_clientRequestToken,

    -- * Destructuring the Response
    DeleteWorkloadShareResponse (..),
    newDeleteWorkloadShareResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for Delete Workload Share
--
-- /See:/ 'newDeleteWorkloadShare' smart constructor.
data DeleteWorkloadShare = DeleteWorkloadShare'
  { shareId :: Prelude.Text,
    workloadId :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkloadShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'deleteWorkloadShare_shareId' - Undocumented member.
--
-- 'workloadId', 'deleteWorkloadShare_workloadId' - Undocumented member.
--
-- 'clientRequestToken', 'deleteWorkloadShare_clientRequestToken' - Undocumented member.
newDeleteWorkloadShare ::
  -- | 'shareId'
  Prelude.Text ->
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeleteWorkloadShare
newDeleteWorkloadShare
  pShareId_
  pWorkloadId_
  pClientRequestToken_ =
    DeleteWorkloadShare'
      { shareId = pShareId_,
        workloadId = pWorkloadId_,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
deleteWorkloadShare_shareId :: Lens.Lens' DeleteWorkloadShare Prelude.Text
deleteWorkloadShare_shareId = Lens.lens (\DeleteWorkloadShare' {shareId} -> shareId) (\s@DeleteWorkloadShare' {} a -> s {shareId = a} :: DeleteWorkloadShare)

-- | Undocumented member.
deleteWorkloadShare_workloadId :: Lens.Lens' DeleteWorkloadShare Prelude.Text
deleteWorkloadShare_workloadId = Lens.lens (\DeleteWorkloadShare' {workloadId} -> workloadId) (\s@DeleteWorkloadShare' {} a -> s {workloadId = a} :: DeleteWorkloadShare)

-- | Undocumented member.
deleteWorkloadShare_clientRequestToken :: Lens.Lens' DeleteWorkloadShare Prelude.Text
deleteWorkloadShare_clientRequestToken = Lens.lens (\DeleteWorkloadShare' {clientRequestToken} -> clientRequestToken) (\s@DeleteWorkloadShare' {} a -> s {clientRequestToken = a} :: DeleteWorkloadShare)

instance Core.AWSRequest DeleteWorkloadShare where
  type
    AWSResponse DeleteWorkloadShare =
      DeleteWorkloadShareResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteWorkloadShareResponse'

instance Prelude.Hashable DeleteWorkloadShare where
  hashWithSalt _salt DeleteWorkloadShare' {..} =
    _salt
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData DeleteWorkloadShare where
  rnf DeleteWorkloadShare' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders DeleteWorkloadShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkloadShare where
  toPath DeleteWorkloadShare' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/shares/",
        Data.toBS shareId
      ]

instance Data.ToQuery DeleteWorkloadShare where
  toQuery DeleteWorkloadShare' {..} =
    Prelude.mconcat
      ["ClientRequestToken" Data.=: clientRequestToken]

-- | /See:/ 'newDeleteWorkloadShareResponse' smart constructor.
data DeleteWorkloadShareResponse = DeleteWorkloadShareResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkloadShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWorkloadShareResponse ::
  DeleteWorkloadShareResponse
newDeleteWorkloadShareResponse =
  DeleteWorkloadShareResponse'

instance Prelude.NFData DeleteWorkloadShareResponse where
  rnf _ = ()
