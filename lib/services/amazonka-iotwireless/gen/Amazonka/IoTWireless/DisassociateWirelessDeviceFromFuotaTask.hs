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
-- Module      : Amazonka.IoTWireless.DisassociateWirelessDeviceFromFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a wireless device from a FUOTA task.
module Amazonka.IoTWireless.DisassociateWirelessDeviceFromFuotaTask
  ( -- * Creating a Request
    DisassociateWirelessDeviceFromFuotaTask (..),
    newDisassociateWirelessDeviceFromFuotaTask,

    -- * Request Lenses
    disassociateWirelessDeviceFromFuotaTask_id,
    disassociateWirelessDeviceFromFuotaTask_wirelessDeviceId,

    -- * Destructuring the Response
    DisassociateWirelessDeviceFromFuotaTaskResponse (..),
    newDisassociateWirelessDeviceFromFuotaTaskResponse,

    -- * Response Lenses
    disassociateWirelessDeviceFromFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateWirelessDeviceFromFuotaTask' smart constructor.
data DisassociateWirelessDeviceFromFuotaTask = DisassociateWirelessDeviceFromFuotaTask'
  { id :: Prelude.Text,
    wirelessDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateWirelessDeviceFromFuotaTask_id' - Undocumented member.
--
-- 'wirelessDeviceId', 'disassociateWirelessDeviceFromFuotaTask_wirelessDeviceId' - Undocumented member.
newDisassociateWirelessDeviceFromFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'wirelessDeviceId'
  Prelude.Text ->
  DisassociateWirelessDeviceFromFuotaTask
newDisassociateWirelessDeviceFromFuotaTask
  pId_
  pWirelessDeviceId_ =
    DisassociateWirelessDeviceFromFuotaTask'
      { id = pId_,
        wirelessDeviceId =
          pWirelessDeviceId_
      }

-- | Undocumented member.
disassociateWirelessDeviceFromFuotaTask_id :: Lens.Lens' DisassociateWirelessDeviceFromFuotaTask Prelude.Text
disassociateWirelessDeviceFromFuotaTask_id = Lens.lens (\DisassociateWirelessDeviceFromFuotaTask' {id} -> id) (\s@DisassociateWirelessDeviceFromFuotaTask' {} a -> s {id = a} :: DisassociateWirelessDeviceFromFuotaTask)

-- | Undocumented member.
disassociateWirelessDeviceFromFuotaTask_wirelessDeviceId :: Lens.Lens' DisassociateWirelessDeviceFromFuotaTask Prelude.Text
disassociateWirelessDeviceFromFuotaTask_wirelessDeviceId = Lens.lens (\DisassociateWirelessDeviceFromFuotaTask' {wirelessDeviceId} -> wirelessDeviceId) (\s@DisassociateWirelessDeviceFromFuotaTask' {} a -> s {wirelessDeviceId = a} :: DisassociateWirelessDeviceFromFuotaTask)

instance
  Core.AWSRequest
    DisassociateWirelessDeviceFromFuotaTask
  where
  type
    AWSResponse
      DisassociateWirelessDeviceFromFuotaTask =
      DisassociateWirelessDeviceFromFuotaTaskResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWirelessDeviceFromFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWirelessDeviceFromFuotaTask
  where
  hashWithSalt
    _salt
    DisassociateWirelessDeviceFromFuotaTask' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` wirelessDeviceId

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromFuotaTask
  where
  rnf DisassociateWirelessDeviceFromFuotaTask' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf wirelessDeviceId

instance
  Data.ToHeaders
    DisassociateWirelessDeviceFromFuotaTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateWirelessDeviceFromFuotaTask
  where
  toPath DisassociateWirelessDeviceFromFuotaTask' {..} =
    Prelude.mconcat
      [ "/fuota-tasks/",
        Data.toBS id,
        "/wireless-devices/",
        Data.toBS wirelessDeviceId
      ]

instance
  Data.ToQuery
    DisassociateWirelessDeviceFromFuotaTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWirelessDeviceFromFuotaTaskResponse' smart constructor.
data DisassociateWirelessDeviceFromFuotaTaskResponse = DisassociateWirelessDeviceFromFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWirelessDeviceFromFuotaTaskResponse_httpStatus' - The response's http status code.
newDisassociateWirelessDeviceFromFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWirelessDeviceFromFuotaTaskResponse
newDisassociateWirelessDeviceFromFuotaTaskResponse
  pHttpStatus_ =
    DisassociateWirelessDeviceFromFuotaTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWirelessDeviceFromFuotaTaskResponse_httpStatus :: Lens.Lens' DisassociateWirelessDeviceFromFuotaTaskResponse Prelude.Int
disassociateWirelessDeviceFromFuotaTaskResponse_httpStatus = Lens.lens (\DisassociateWirelessDeviceFromFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@DisassociateWirelessDeviceFromFuotaTaskResponse' {} a -> s {httpStatus = a} :: DisassociateWirelessDeviceFromFuotaTaskResponse)

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromFuotaTaskResponse
  where
  rnf
    DisassociateWirelessDeviceFromFuotaTaskResponse' {..} =
      Prelude.rnf httpStatus
