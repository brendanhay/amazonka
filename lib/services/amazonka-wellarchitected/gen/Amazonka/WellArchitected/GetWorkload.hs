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
-- Module      : Amazonka.WellArchitected.GetWorkload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an existing workload.
module Amazonka.WellArchitected.GetWorkload
  ( -- * Creating a Request
    GetWorkload (..),
    newGetWorkload,

    -- * Request Lenses
    getWorkload_workloadId,

    -- * Destructuring the Response
    GetWorkloadResponse (..),
    newGetWorkloadResponse,

    -- * Response Lenses
    getWorkloadResponse_workload,
    getWorkloadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to get a workload.
--
-- /See:/ 'newGetWorkload' smart constructor.
data GetWorkload = GetWorkload'
  { workloadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'getWorkload_workloadId' - Undocumented member.
newGetWorkload ::
  -- | 'workloadId'
  Prelude.Text ->
  GetWorkload
newGetWorkload pWorkloadId_ =
  GetWorkload' {workloadId = pWorkloadId_}

-- | Undocumented member.
getWorkload_workloadId :: Lens.Lens' GetWorkload Prelude.Text
getWorkload_workloadId = Lens.lens (\GetWorkload' {workloadId} -> workloadId) (\s@GetWorkload' {} a -> s {workloadId = a} :: GetWorkload)

instance Core.AWSRequest GetWorkload where
  type AWSResponse GetWorkload = GetWorkloadResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkloadResponse'
            Prelude.<$> (x Data..?> "Workload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkload where
  hashWithSalt _salt GetWorkload' {..} =
    _salt `Prelude.hashWithSalt` workloadId

instance Prelude.NFData GetWorkload where
  rnf GetWorkload' {..} = Prelude.rnf workloadId

instance Data.ToHeaders GetWorkload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkload where
  toPath GetWorkload' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId]

instance Data.ToQuery GetWorkload where
  toQuery = Prelude.const Prelude.mempty

-- | Output of a get workload call.
--
-- /See:/ 'newGetWorkloadResponse' smart constructor.
data GetWorkloadResponse = GetWorkloadResponse'
  { workload :: Prelude.Maybe Workload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkloadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workload', 'getWorkloadResponse_workload' - Undocumented member.
--
-- 'httpStatus', 'getWorkloadResponse_httpStatus' - The response's http status code.
newGetWorkloadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkloadResponse
newGetWorkloadResponse pHttpStatus_ =
  GetWorkloadResponse'
    { workload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getWorkloadResponse_workload :: Lens.Lens' GetWorkloadResponse (Prelude.Maybe Workload)
getWorkloadResponse_workload = Lens.lens (\GetWorkloadResponse' {workload} -> workload) (\s@GetWorkloadResponse' {} a -> s {workload = a} :: GetWorkloadResponse)

-- | The response's http status code.
getWorkloadResponse_httpStatus :: Lens.Lens' GetWorkloadResponse Prelude.Int
getWorkloadResponse_httpStatus = Lens.lens (\GetWorkloadResponse' {httpStatus} -> httpStatus) (\s@GetWorkloadResponse' {} a -> s {httpStatus = a} :: GetWorkloadResponse)

instance Prelude.NFData GetWorkloadResponse where
  rnf GetWorkloadResponse' {..} =
    Prelude.rnf workload `Prelude.seq`
      Prelude.rnf httpStatus
