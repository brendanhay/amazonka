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
-- Module      : Network.AWS.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active conversion task. The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI>.
module Network.AWS.EC2.CancelConversionTask
  ( -- * Creating a Request
    CancelConversionTask (..),
    newCancelConversionTask,

    -- * Request Lenses
    cancelConversionTask_dryRun,
    cancelConversionTask_reasonMessage,
    cancelConversionTask_conversionTaskId,

    -- * Destructuring the Response
    CancelConversionTaskResponse (..),
    newCancelConversionTaskResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The reason for canceling the conversion task.
    reasonMessage :: Core.Maybe Core.Text,
    -- | The ID of the conversion task.
    conversionTaskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelConversionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelConversionTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'reasonMessage', 'cancelConversionTask_reasonMessage' - The reason for canceling the conversion task.
--
-- 'conversionTaskId', 'cancelConversionTask_conversionTaskId' - The ID of the conversion task.
newCancelConversionTask ::
  -- | 'conversionTaskId'
  Core.Text ->
  CancelConversionTask
newCancelConversionTask pConversionTaskId_ =
  CancelConversionTask'
    { dryRun = Core.Nothing,
      reasonMessage = Core.Nothing,
      conversionTaskId = pConversionTaskId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelConversionTask_dryRun :: Lens.Lens' CancelConversionTask (Core.Maybe Core.Bool)
cancelConversionTask_dryRun = Lens.lens (\CancelConversionTask' {dryRun} -> dryRun) (\s@CancelConversionTask' {} a -> s {dryRun = a} :: CancelConversionTask)

-- | The reason for canceling the conversion task.
cancelConversionTask_reasonMessage :: Lens.Lens' CancelConversionTask (Core.Maybe Core.Text)
cancelConversionTask_reasonMessage = Lens.lens (\CancelConversionTask' {reasonMessage} -> reasonMessage) (\s@CancelConversionTask' {} a -> s {reasonMessage = a} :: CancelConversionTask)

-- | The ID of the conversion task.
cancelConversionTask_conversionTaskId :: Lens.Lens' CancelConversionTask Core.Text
cancelConversionTask_conversionTaskId = Lens.lens (\CancelConversionTask' {conversionTaskId} -> conversionTaskId) (\s@CancelConversionTask' {} a -> s {conversionTaskId = a} :: CancelConversionTask)

instance Core.AWSRequest CancelConversionTask where
  type
    AWSResponse CancelConversionTask =
      CancelConversionTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull CancelConversionTaskResponse'

instance Core.Hashable CancelConversionTask

instance Core.NFData CancelConversionTask

instance Core.ToHeaders CancelConversionTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelConversionTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelConversionTask where
  toQuery CancelConversionTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelConversionTask" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ReasonMessage" Core.=: reasonMessage,
        "ConversionTaskId" Core.=: conversionTaskId
      ]

-- | /See:/ 'newCancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse = CancelConversionTaskResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelConversionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelConversionTaskResponse ::
  CancelConversionTaskResponse
newCancelConversionTaskResponse =
  CancelConversionTaskResponse'

instance Core.NFData CancelConversionTaskResponse
