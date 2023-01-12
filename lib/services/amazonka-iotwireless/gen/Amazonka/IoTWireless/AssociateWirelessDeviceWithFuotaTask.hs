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
-- Module      : Amazonka.IoTWireless.AssociateWirelessDeviceWithFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a wireless device with a FUOTA task.
module Amazonka.IoTWireless.AssociateWirelessDeviceWithFuotaTask
  ( -- * Creating a Request
    AssociateWirelessDeviceWithFuotaTask (..),
    newAssociateWirelessDeviceWithFuotaTask,

    -- * Request Lenses
    associateWirelessDeviceWithFuotaTask_id,
    associateWirelessDeviceWithFuotaTask_wirelessDeviceId,

    -- * Destructuring the Response
    AssociateWirelessDeviceWithFuotaTaskResponse (..),
    newAssociateWirelessDeviceWithFuotaTaskResponse,

    -- * Response Lenses
    associateWirelessDeviceWithFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateWirelessDeviceWithFuotaTask' smart constructor.
data AssociateWirelessDeviceWithFuotaTask = AssociateWirelessDeviceWithFuotaTask'
  { id :: Prelude.Text,
    wirelessDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateWirelessDeviceWithFuotaTask_id' - Undocumented member.
--
-- 'wirelessDeviceId', 'associateWirelessDeviceWithFuotaTask_wirelessDeviceId' - Undocumented member.
newAssociateWirelessDeviceWithFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'wirelessDeviceId'
  Prelude.Text ->
  AssociateWirelessDeviceWithFuotaTask
newAssociateWirelessDeviceWithFuotaTask
  pId_
  pWirelessDeviceId_ =
    AssociateWirelessDeviceWithFuotaTask'
      { id = pId_,
        wirelessDeviceId = pWirelessDeviceId_
      }

-- | Undocumented member.
associateWirelessDeviceWithFuotaTask_id :: Lens.Lens' AssociateWirelessDeviceWithFuotaTask Prelude.Text
associateWirelessDeviceWithFuotaTask_id = Lens.lens (\AssociateWirelessDeviceWithFuotaTask' {id} -> id) (\s@AssociateWirelessDeviceWithFuotaTask' {} a -> s {id = a} :: AssociateWirelessDeviceWithFuotaTask)

-- | Undocumented member.
associateWirelessDeviceWithFuotaTask_wirelessDeviceId :: Lens.Lens' AssociateWirelessDeviceWithFuotaTask Prelude.Text
associateWirelessDeviceWithFuotaTask_wirelessDeviceId = Lens.lens (\AssociateWirelessDeviceWithFuotaTask' {wirelessDeviceId} -> wirelessDeviceId) (\s@AssociateWirelessDeviceWithFuotaTask' {} a -> s {wirelessDeviceId = a} :: AssociateWirelessDeviceWithFuotaTask)

instance
  Core.AWSRequest
    AssociateWirelessDeviceWithFuotaTask
  where
  type
    AWSResponse AssociateWirelessDeviceWithFuotaTask =
      AssociateWirelessDeviceWithFuotaTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWirelessDeviceWithFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWirelessDeviceWithFuotaTask
  where
  hashWithSalt
    _salt
    AssociateWirelessDeviceWithFuotaTask' {..} =
      _salt `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` wirelessDeviceId

instance
  Prelude.NFData
    AssociateWirelessDeviceWithFuotaTask
  where
  rnf AssociateWirelessDeviceWithFuotaTask' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf wirelessDeviceId

instance
  Data.ToHeaders
    AssociateWirelessDeviceWithFuotaTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociateWirelessDeviceWithFuotaTask
  where
  toJSON AssociateWirelessDeviceWithFuotaTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WirelessDeviceId" Data..= wirelessDeviceId)
          ]
      )

instance
  Data.ToPath
    AssociateWirelessDeviceWithFuotaTask
  where
  toPath AssociateWirelessDeviceWithFuotaTask' {..} =
    Prelude.mconcat
      ["/fuota-tasks/", Data.toBS id, "/wireless-device"]

instance
  Data.ToQuery
    AssociateWirelessDeviceWithFuotaTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWirelessDeviceWithFuotaTaskResponse' smart constructor.
data AssociateWirelessDeviceWithFuotaTaskResponse = AssociateWirelessDeviceWithFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateWirelessDeviceWithFuotaTaskResponse_httpStatus' - The response's http status code.
newAssociateWirelessDeviceWithFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWirelessDeviceWithFuotaTaskResponse
newAssociateWirelessDeviceWithFuotaTaskResponse
  pHttpStatus_ =
    AssociateWirelessDeviceWithFuotaTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateWirelessDeviceWithFuotaTaskResponse_httpStatus :: Lens.Lens' AssociateWirelessDeviceWithFuotaTaskResponse Prelude.Int
associateWirelessDeviceWithFuotaTaskResponse_httpStatus = Lens.lens (\AssociateWirelessDeviceWithFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@AssociateWirelessDeviceWithFuotaTaskResponse' {} a -> s {httpStatus = a} :: AssociateWirelessDeviceWithFuotaTaskResponse)

instance
  Prelude.NFData
    AssociateWirelessDeviceWithFuotaTaskResponse
  where
  rnf AssociateWirelessDeviceWithFuotaTaskResponse' {..} =
    Prelude.rnf httpStatus
