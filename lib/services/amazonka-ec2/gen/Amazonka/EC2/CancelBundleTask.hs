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
-- Module      : Amazonka.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows
-- instance.
module Amazonka.EC2.CancelBundleTask
  ( -- * Creating a Request
    CancelBundleTask (..),
    newCancelBundleTask,

    -- * Request Lenses
    cancelBundleTask_dryRun,
    cancelBundleTask_bundleId,

    -- * Destructuring the Response
    CancelBundleTaskResponse (..),
    newCancelBundleTaskResponse,

    -- * Response Lenses
    cancelBundleTaskResponse_bundleTask,
    cancelBundleTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CancelBundleTask.
--
-- /See:/ 'newCancelBundleTask' smart constructor.
data CancelBundleTask = CancelBundleTask'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the bundle task.
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBundleTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelBundleTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'bundleId', 'cancelBundleTask_bundleId' - The ID of the bundle task.
newCancelBundleTask ::
  -- | 'bundleId'
  Prelude.Text ->
  CancelBundleTask
newCancelBundleTask pBundleId_ =
  CancelBundleTask'
    { dryRun = Prelude.Nothing,
      bundleId = pBundleId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelBundleTask_dryRun :: Lens.Lens' CancelBundleTask (Prelude.Maybe Prelude.Bool)
cancelBundleTask_dryRun = Lens.lens (\CancelBundleTask' {dryRun} -> dryRun) (\s@CancelBundleTask' {} a -> s {dryRun = a} :: CancelBundleTask)

-- | The ID of the bundle task.
cancelBundleTask_bundleId :: Lens.Lens' CancelBundleTask Prelude.Text
cancelBundleTask_bundleId = Lens.lens (\CancelBundleTask' {bundleId} -> bundleId) (\s@CancelBundleTask' {} a -> s {bundleId = a} :: CancelBundleTask)

instance Core.AWSRequest CancelBundleTask where
  type
    AWSResponse CancelBundleTask =
      CancelBundleTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CancelBundleTaskResponse'
            Prelude.<$> (x Data..@? "bundleInstanceTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelBundleTask where
  hashWithSalt _salt CancelBundleTask' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` bundleId

instance Prelude.NFData CancelBundleTask where
  rnf CancelBundleTask' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf bundleId

instance Data.ToHeaders CancelBundleTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelBundleTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelBundleTask where
  toQuery CancelBundleTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelBundleTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "BundleId" Data.=: bundleId
      ]

-- | Contains the output of CancelBundleTask.
--
-- /See:/ 'newCancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { -- | Information about the bundle task.
    bundleTask :: Prelude.Maybe BundleTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBundleTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleTask', 'cancelBundleTaskResponse_bundleTask' - Information about the bundle task.
--
-- 'httpStatus', 'cancelBundleTaskResponse_httpStatus' - The response's http status code.
newCancelBundleTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelBundleTaskResponse
newCancelBundleTaskResponse pHttpStatus_ =
  CancelBundleTaskResponse'
    { bundleTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the bundle task.
cancelBundleTaskResponse_bundleTask :: Lens.Lens' CancelBundleTaskResponse (Prelude.Maybe BundleTask)
cancelBundleTaskResponse_bundleTask = Lens.lens (\CancelBundleTaskResponse' {bundleTask} -> bundleTask) (\s@CancelBundleTaskResponse' {} a -> s {bundleTask = a} :: CancelBundleTaskResponse)

-- | The response's http status code.
cancelBundleTaskResponse_httpStatus :: Lens.Lens' CancelBundleTaskResponse Prelude.Int
cancelBundleTaskResponse_httpStatus = Lens.lens (\CancelBundleTaskResponse' {httpStatus} -> httpStatus) (\s@CancelBundleTaskResponse' {} a -> s {httpStatus = a} :: CancelBundleTaskResponse)

instance Prelude.NFData CancelBundleTaskResponse where
  rnf CancelBundleTaskResponse' {..} =
    Prelude.rnf bundleTask `Prelude.seq`
      Prelude.rnf httpStatus
