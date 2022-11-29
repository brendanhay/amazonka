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
-- Module      : Amazonka.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.CancelConversionTask
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The reason for canceling the conversion task.
    reasonMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the conversion task.
    conversionTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CancelConversionTask
newCancelConversionTask pConversionTaskId_ =
  CancelConversionTask'
    { dryRun = Prelude.Nothing,
      reasonMessage = Prelude.Nothing,
      conversionTaskId = pConversionTaskId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelConversionTask_dryRun :: Lens.Lens' CancelConversionTask (Prelude.Maybe Prelude.Bool)
cancelConversionTask_dryRun = Lens.lens (\CancelConversionTask' {dryRun} -> dryRun) (\s@CancelConversionTask' {} a -> s {dryRun = a} :: CancelConversionTask)

-- | The reason for canceling the conversion task.
cancelConversionTask_reasonMessage :: Lens.Lens' CancelConversionTask (Prelude.Maybe Prelude.Text)
cancelConversionTask_reasonMessage = Lens.lens (\CancelConversionTask' {reasonMessage} -> reasonMessage) (\s@CancelConversionTask' {} a -> s {reasonMessage = a} :: CancelConversionTask)

-- | The ID of the conversion task.
cancelConversionTask_conversionTaskId :: Lens.Lens' CancelConversionTask Prelude.Text
cancelConversionTask_conversionTaskId = Lens.lens (\CancelConversionTask' {conversionTaskId} -> conversionTaskId) (\s@CancelConversionTask' {} a -> s {conversionTaskId = a} :: CancelConversionTask)

instance Core.AWSRequest CancelConversionTask where
  type
    AWSResponse CancelConversionTask =
      CancelConversionTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CancelConversionTaskResponse'

instance Prelude.Hashable CancelConversionTask where
  hashWithSalt _salt CancelConversionTask' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` reasonMessage
      `Prelude.hashWithSalt` conversionTaskId

instance Prelude.NFData CancelConversionTask where
  rnf CancelConversionTask' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf reasonMessage
      `Prelude.seq` Prelude.rnf conversionTaskId

instance Core.ToHeaders CancelConversionTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelConversionTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelConversionTask where
  toQuery CancelConversionTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CancelConversionTask" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "ReasonMessage" Core.=: reasonMessage,
        "ConversionTaskId" Core.=: conversionTaskId
      ]

-- | /See:/ 'newCancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse = CancelConversionTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelConversionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelConversionTaskResponse ::
  CancelConversionTaskResponse
newCancelConversionTaskResponse =
  CancelConversionTaskResponse'

instance Prelude.NFData CancelConversionTaskResponse where
  rnf _ = ()
