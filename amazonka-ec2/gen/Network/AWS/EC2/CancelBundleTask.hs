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
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows
-- instance.
module Network.AWS.EC2.CancelBundleTask
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelBundleTaskResponse'
            Prelude.<$> (x Core..@? "bundleInstanceTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelBundleTask

instance Prelude.NFData CancelBundleTask

instance Core.ToHeaders CancelBundleTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelBundleTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelBundleTask where
  toQuery CancelBundleTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CancelBundleTask" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "BundleId" Core.=: bundleId
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

instance Prelude.NFData CancelBundleTaskResponse
