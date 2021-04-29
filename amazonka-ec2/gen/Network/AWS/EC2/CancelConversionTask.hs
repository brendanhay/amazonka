{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CancelConversionTask where
  type
    Rs CancelConversionTask =
      CancelConversionTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull CancelConversionTaskResponse'

instance Prelude.Hashable CancelConversionTask

instance Prelude.NFData CancelConversionTask

instance Prelude.ToHeaders CancelConversionTask where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelConversionTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelConversionTask where
  toQuery CancelConversionTask' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelConversionTask" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "ReasonMessage" Prelude.=: reasonMessage,
        "ConversionTaskId" Prelude.=: conversionTaskId
      ]

-- | /See:/ 'newCancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse = CancelConversionTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelConversionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelConversionTaskResponse ::
  CancelConversionTaskResponse
newCancelConversionTaskResponse =
  CancelConversionTaskResponse'

instance Prelude.NFData CancelConversionTaskResponse
