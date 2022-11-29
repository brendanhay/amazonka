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
-- Module      : Amazonka.IoTWireless.DisassociateWirelessDeviceFromThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a wireless device from its currently associated thing.
module Amazonka.IoTWireless.DisassociateWirelessDeviceFromThing
  ( -- * Creating a Request
    DisassociateWirelessDeviceFromThing (..),
    newDisassociateWirelessDeviceFromThing,

    -- * Request Lenses
    disassociateWirelessDeviceFromThing_id,

    -- * Destructuring the Response
    DisassociateWirelessDeviceFromThingResponse (..),
    newDisassociateWirelessDeviceFromThingResponse,

    -- * Response Lenses
    disassociateWirelessDeviceFromThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateWirelessDeviceFromThing' smart constructor.
data DisassociateWirelessDeviceFromThing = DisassociateWirelessDeviceFromThing'
  { -- | The ID of the resource to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateWirelessDeviceFromThing_id' - The ID of the resource to update.
newDisassociateWirelessDeviceFromThing ::
  -- | 'id'
  Prelude.Text ->
  DisassociateWirelessDeviceFromThing
newDisassociateWirelessDeviceFromThing pId_ =
  DisassociateWirelessDeviceFromThing' {id = pId_}

-- | The ID of the resource to update.
disassociateWirelessDeviceFromThing_id :: Lens.Lens' DisassociateWirelessDeviceFromThing Prelude.Text
disassociateWirelessDeviceFromThing_id = Lens.lens (\DisassociateWirelessDeviceFromThing' {id} -> id) (\s@DisassociateWirelessDeviceFromThing' {} a -> s {id = a} :: DisassociateWirelessDeviceFromThing)

instance
  Core.AWSRequest
    DisassociateWirelessDeviceFromThing
  where
  type
    AWSResponse DisassociateWirelessDeviceFromThing =
      DisassociateWirelessDeviceFromThingResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWirelessDeviceFromThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWirelessDeviceFromThing
  where
  hashWithSalt
    _salt
    DisassociateWirelessDeviceFromThing' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromThing
  where
  rnf DisassociateWirelessDeviceFromThing' {..} =
    Prelude.rnf id

instance
  Core.ToHeaders
    DisassociateWirelessDeviceFromThing
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DisassociateWirelessDeviceFromThing
  where
  toPath DisassociateWirelessDeviceFromThing' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Core.toBS id, "/thing"]

instance
  Core.ToQuery
    DisassociateWirelessDeviceFromThing
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWirelessDeviceFromThingResponse' smart constructor.
data DisassociateWirelessDeviceFromThingResponse = DisassociateWirelessDeviceFromThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWirelessDeviceFromThingResponse_httpStatus' - The response's http status code.
newDisassociateWirelessDeviceFromThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWirelessDeviceFromThingResponse
newDisassociateWirelessDeviceFromThingResponse
  pHttpStatus_ =
    DisassociateWirelessDeviceFromThingResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWirelessDeviceFromThingResponse_httpStatus :: Lens.Lens' DisassociateWirelessDeviceFromThingResponse Prelude.Int
disassociateWirelessDeviceFromThingResponse_httpStatus = Lens.lens (\DisassociateWirelessDeviceFromThingResponse' {httpStatus} -> httpStatus) (\s@DisassociateWirelessDeviceFromThingResponse' {} a -> s {httpStatus = a} :: DisassociateWirelessDeviceFromThingResponse)

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromThingResponse
  where
  rnf DisassociateWirelessDeviceFromThingResponse' {..} =
    Prelude.rnf httpStatus
