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
-- Module      : Amazonka.WellArchitected.DeleteWorkload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing workload.
module Amazonka.WellArchitected.DeleteWorkload
  ( -- * Creating a Request
    DeleteWorkload (..),
    newDeleteWorkload,

    -- * Request Lenses
    deleteWorkload_workloadId,
    deleteWorkload_clientRequestToken,

    -- * Destructuring the Response
    DeleteWorkloadResponse (..),
    newDeleteWorkloadResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for workload deletion.
--
-- /See:/ 'newDeleteWorkload' smart constructor.
data DeleteWorkload = DeleteWorkload'
  { workloadId :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'deleteWorkload_workloadId' - Undocumented member.
--
-- 'clientRequestToken', 'deleteWorkload_clientRequestToken' - Undocumented member.
newDeleteWorkload ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeleteWorkload
newDeleteWorkload pWorkloadId_ pClientRequestToken_ =
  DeleteWorkload'
    { workloadId = pWorkloadId_,
      clientRequestToken = pClientRequestToken_
    }

-- | Undocumented member.
deleteWorkload_workloadId :: Lens.Lens' DeleteWorkload Prelude.Text
deleteWorkload_workloadId = Lens.lens (\DeleteWorkload' {workloadId} -> workloadId) (\s@DeleteWorkload' {} a -> s {workloadId = a} :: DeleteWorkload)

-- | Undocumented member.
deleteWorkload_clientRequestToken :: Lens.Lens' DeleteWorkload Prelude.Text
deleteWorkload_clientRequestToken = Lens.lens (\DeleteWorkload' {clientRequestToken} -> clientRequestToken) (\s@DeleteWorkload' {} a -> s {clientRequestToken = a} :: DeleteWorkload)

instance Core.AWSRequest DeleteWorkload where
  type
    AWSResponse DeleteWorkload =
      DeleteWorkloadResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteWorkloadResponse'

instance Prelude.Hashable DeleteWorkload where
  hashWithSalt _salt DeleteWorkload' {..} =
    _salt `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData DeleteWorkload where
  rnf DeleteWorkload' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders DeleteWorkload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkload where
  toPath DeleteWorkload' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId]

instance Data.ToQuery DeleteWorkload where
  toQuery DeleteWorkload' {..} =
    Prelude.mconcat
      ["ClientRequestToken" Data.=: clientRequestToken]

-- | /See:/ 'newDeleteWorkloadResponse' smart constructor.
data DeleteWorkloadResponse = DeleteWorkloadResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkloadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWorkloadResponse ::
  DeleteWorkloadResponse
newDeleteWorkloadResponse = DeleteWorkloadResponse'

instance Prelude.NFData DeleteWorkloadResponse where
  rnf _ = ()
