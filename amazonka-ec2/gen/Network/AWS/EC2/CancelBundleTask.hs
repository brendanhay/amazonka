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
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the bundle task.
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CancelBundleTask
newCancelBundleTask pBundleId_ =
  CancelBundleTask'
    { dryRun = Core.Nothing,
      bundleId = pBundleId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelBundleTask_dryRun :: Lens.Lens' CancelBundleTask (Core.Maybe Core.Bool)
cancelBundleTask_dryRun = Lens.lens (\CancelBundleTask' {dryRun} -> dryRun) (\s@CancelBundleTask' {} a -> s {dryRun = a} :: CancelBundleTask)

-- | The ID of the bundle task.
cancelBundleTask_bundleId :: Lens.Lens' CancelBundleTask Core.Text
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
            Core.<$> (x Core..@? "bundleInstanceTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelBundleTask

instance Core.NFData CancelBundleTask

instance Core.ToHeaders CancelBundleTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelBundleTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelBundleTask where
  toQuery CancelBundleTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelBundleTask" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "BundleId" Core.=: bundleId
      ]

-- | Contains the output of CancelBundleTask.
--
-- /See:/ 'newCancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { -- | Information about the bundle task.
    bundleTask :: Core.Maybe BundleTask,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CancelBundleTaskResponse
newCancelBundleTaskResponse pHttpStatus_ =
  CancelBundleTaskResponse'
    { bundleTask =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the bundle task.
cancelBundleTaskResponse_bundleTask :: Lens.Lens' CancelBundleTaskResponse (Core.Maybe BundleTask)
cancelBundleTaskResponse_bundleTask = Lens.lens (\CancelBundleTaskResponse' {bundleTask} -> bundleTask) (\s@CancelBundleTaskResponse' {} a -> s {bundleTask = a} :: CancelBundleTaskResponse)

-- | The response's http status code.
cancelBundleTaskResponse_httpStatus :: Lens.Lens' CancelBundleTaskResponse Core.Int
cancelBundleTaskResponse_httpStatus = Lens.lens (\CancelBundleTaskResponse' {httpStatus} -> httpStatus) (\s@CancelBundleTaskResponse' {} a -> s {httpStatus = a} :: CancelBundleTaskResponse)

instance Core.NFData CancelBundleTaskResponse
