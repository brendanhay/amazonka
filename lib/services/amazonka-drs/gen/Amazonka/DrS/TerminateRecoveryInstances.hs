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
-- Module      : Amazonka.DrS.TerminateRecoveryInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a Job for terminating the EC2 resources associated with the
-- specified Recovery Instances, and then will delete the Recovery
-- Instances from the Elastic Disaster Recovery service.
module Amazonka.DrS.TerminateRecoveryInstances
  ( -- * Creating a Request
    TerminateRecoveryInstances (..),
    newTerminateRecoveryInstances,

    -- * Request Lenses
    terminateRecoveryInstances_recoveryInstanceIDs,

    -- * Destructuring the Response
    TerminateRecoveryInstancesResponse (..),
    newTerminateRecoveryInstancesResponse,

    -- * Response Lenses
    terminateRecoveryInstancesResponse_job,
    terminateRecoveryInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTerminateRecoveryInstances' smart constructor.
data TerminateRecoveryInstances = TerminateRecoveryInstances'
  { -- | The IDs of the Recovery Instances that should be terminated.
    recoveryInstanceIDs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateRecoveryInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceIDs', 'terminateRecoveryInstances_recoveryInstanceIDs' - The IDs of the Recovery Instances that should be terminated.
newTerminateRecoveryInstances ::
  -- | 'recoveryInstanceIDs'
  Prelude.NonEmpty Prelude.Text ->
  TerminateRecoveryInstances
newTerminateRecoveryInstances pRecoveryInstanceIDs_ =
  TerminateRecoveryInstances'
    { recoveryInstanceIDs =
        Lens.coerced Lens.# pRecoveryInstanceIDs_
    }

-- | The IDs of the Recovery Instances that should be terminated.
terminateRecoveryInstances_recoveryInstanceIDs :: Lens.Lens' TerminateRecoveryInstances (Prelude.NonEmpty Prelude.Text)
terminateRecoveryInstances_recoveryInstanceIDs = Lens.lens (\TerminateRecoveryInstances' {recoveryInstanceIDs} -> recoveryInstanceIDs) (\s@TerminateRecoveryInstances' {} a -> s {recoveryInstanceIDs = a} :: TerminateRecoveryInstances) Prelude.. Lens.coerced

instance Core.AWSRequest TerminateRecoveryInstances where
  type
    AWSResponse TerminateRecoveryInstances =
      TerminateRecoveryInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateRecoveryInstancesResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateRecoveryInstances where
  hashWithSalt _salt TerminateRecoveryInstances' {..} =
    _salt `Prelude.hashWithSalt` recoveryInstanceIDs

instance Prelude.NFData TerminateRecoveryInstances where
  rnf TerminateRecoveryInstances' {..} =
    Prelude.rnf recoveryInstanceIDs

instance Data.ToHeaders TerminateRecoveryInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateRecoveryInstances where
  toJSON TerminateRecoveryInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceIDs" Data..= recoveryInstanceIDs)
          ]
      )

instance Data.ToPath TerminateRecoveryInstances where
  toPath = Prelude.const "/TerminateRecoveryInstances"

instance Data.ToQuery TerminateRecoveryInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateRecoveryInstancesResponse' smart constructor.
data TerminateRecoveryInstancesResponse = TerminateRecoveryInstancesResponse'
  { -- | The Job for terminating the Recovery Instances.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateRecoveryInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'terminateRecoveryInstancesResponse_job' - The Job for terminating the Recovery Instances.
--
-- 'httpStatus', 'terminateRecoveryInstancesResponse_httpStatus' - The response's http status code.
newTerminateRecoveryInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateRecoveryInstancesResponse
newTerminateRecoveryInstancesResponse pHttpStatus_ =
  TerminateRecoveryInstancesResponse'
    { job =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Job for terminating the Recovery Instances.
terminateRecoveryInstancesResponse_job :: Lens.Lens' TerminateRecoveryInstancesResponse (Prelude.Maybe Job)
terminateRecoveryInstancesResponse_job = Lens.lens (\TerminateRecoveryInstancesResponse' {job} -> job) (\s@TerminateRecoveryInstancesResponse' {} a -> s {job = a} :: TerminateRecoveryInstancesResponse)

-- | The response's http status code.
terminateRecoveryInstancesResponse_httpStatus :: Lens.Lens' TerminateRecoveryInstancesResponse Prelude.Int
terminateRecoveryInstancesResponse_httpStatus = Lens.lens (\TerminateRecoveryInstancesResponse' {httpStatus} -> httpStatus) (\s@TerminateRecoveryInstancesResponse' {} a -> s {httpStatus = a} :: TerminateRecoveryInstancesResponse)

instance
  Prelude.NFData
    TerminateRecoveryInstancesResponse
  where
  rnf TerminateRecoveryInstancesResponse' {..} =
    Prelude.rnf job `Prelude.seq`
      Prelude.rnf httpStatus
