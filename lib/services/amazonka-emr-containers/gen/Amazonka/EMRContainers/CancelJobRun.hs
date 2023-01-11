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
-- Module      : Amazonka.EMRContainers.CancelJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job run. A job run is a unit of work, such as a Spark jar,
-- PySpark script, or SparkSQL query, that you submit to Amazon EMR on EKS.
module Amazonka.EMRContainers.CancelJobRun
  ( -- * Creating a Request
    CancelJobRun (..),
    newCancelJobRun,

    -- * Request Lenses
    cancelJobRun_id,
    cancelJobRun_virtualClusterId,

    -- * Destructuring the Response
    CancelJobRunResponse (..),
    newCancelJobRunResponse,

    -- * Response Lenses
    cancelJobRunResponse_id,
    cancelJobRunResponse_virtualClusterId,
    cancelJobRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelJobRun' smart constructor.
data CancelJobRun = CancelJobRun'
  { -- | The ID of the job run to cancel.
    id :: Prelude.Text,
    -- | The ID of the virtual cluster for which the job run will be canceled.
    virtualClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cancelJobRun_id' - The ID of the job run to cancel.
--
-- 'virtualClusterId', 'cancelJobRun_virtualClusterId' - The ID of the virtual cluster for which the job run will be canceled.
newCancelJobRun ::
  -- | 'id'
  Prelude.Text ->
  -- | 'virtualClusterId'
  Prelude.Text ->
  CancelJobRun
newCancelJobRun pId_ pVirtualClusterId_ =
  CancelJobRun'
    { id = pId_,
      virtualClusterId = pVirtualClusterId_
    }

-- | The ID of the job run to cancel.
cancelJobRun_id :: Lens.Lens' CancelJobRun Prelude.Text
cancelJobRun_id = Lens.lens (\CancelJobRun' {id} -> id) (\s@CancelJobRun' {} a -> s {id = a} :: CancelJobRun)

-- | The ID of the virtual cluster for which the job run will be canceled.
cancelJobRun_virtualClusterId :: Lens.Lens' CancelJobRun Prelude.Text
cancelJobRun_virtualClusterId = Lens.lens (\CancelJobRun' {virtualClusterId} -> virtualClusterId) (\s@CancelJobRun' {} a -> s {virtualClusterId = a} :: CancelJobRun)

instance Core.AWSRequest CancelJobRun where
  type AWSResponse CancelJobRun = CancelJobRunResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobRunResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "virtualClusterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelJobRun where
  hashWithSalt _salt CancelJobRun' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData CancelJobRun where
  rnf CancelJobRun' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf virtualClusterId

instance Data.ToHeaders CancelJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelJobRun where
  toPath CancelJobRun' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterId,
        "/jobruns/",
        Data.toBS id
      ]

instance Data.ToQuery CancelJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelJobRunResponse' smart constructor.
data CancelJobRunResponse = CancelJobRunResponse'
  { -- | The output contains the ID of the cancelled job run.
    id :: Prelude.Maybe Prelude.Text,
    -- | The output contains the virtual cluster ID for which the job run is
    -- cancelled.
    virtualClusterId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cancelJobRunResponse_id' - The output contains the ID of the cancelled job run.
--
-- 'virtualClusterId', 'cancelJobRunResponse_virtualClusterId' - The output contains the virtual cluster ID for which the job run is
-- cancelled.
--
-- 'httpStatus', 'cancelJobRunResponse_httpStatus' - The response's http status code.
newCancelJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelJobRunResponse
newCancelJobRunResponse pHttpStatus_ =
  CancelJobRunResponse'
    { id = Prelude.Nothing,
      virtualClusterId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The output contains the ID of the cancelled job run.
cancelJobRunResponse_id :: Lens.Lens' CancelJobRunResponse (Prelude.Maybe Prelude.Text)
cancelJobRunResponse_id = Lens.lens (\CancelJobRunResponse' {id} -> id) (\s@CancelJobRunResponse' {} a -> s {id = a} :: CancelJobRunResponse)

-- | The output contains the virtual cluster ID for which the job run is
-- cancelled.
cancelJobRunResponse_virtualClusterId :: Lens.Lens' CancelJobRunResponse (Prelude.Maybe Prelude.Text)
cancelJobRunResponse_virtualClusterId = Lens.lens (\CancelJobRunResponse' {virtualClusterId} -> virtualClusterId) (\s@CancelJobRunResponse' {} a -> s {virtualClusterId = a} :: CancelJobRunResponse)

-- | The response's http status code.
cancelJobRunResponse_httpStatus :: Lens.Lens' CancelJobRunResponse Prelude.Int
cancelJobRunResponse_httpStatus = Lens.lens (\CancelJobRunResponse' {httpStatus} -> httpStatus) (\s@CancelJobRunResponse' {} a -> s {httpStatus = a} :: CancelJobRunResponse)

instance Prelude.NFData CancelJobRunResponse where
  rnf CancelJobRunResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf virtualClusterId
      `Prelude.seq` Prelude.rnf httpStatus
