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
-- Module      : Amazonka.IoTWireless.AssociateWirelessDeviceWithThing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a wireless device with a thing.
module Amazonka.IoTWireless.AssociateWirelessDeviceWithThing
  ( -- * Creating a Request
    AssociateWirelessDeviceWithThing (..),
    newAssociateWirelessDeviceWithThing,

    -- * Request Lenses
    associateWirelessDeviceWithThing_id,
    associateWirelessDeviceWithThing_thingArn,

    -- * Destructuring the Response
    AssociateWirelessDeviceWithThingResponse (..),
    newAssociateWirelessDeviceWithThingResponse,

    -- * Response Lenses
    associateWirelessDeviceWithThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateWirelessDeviceWithThing' smart constructor.
data AssociateWirelessDeviceWithThing = AssociateWirelessDeviceWithThing'
  { -- | The ID of the resource to update.
    id :: Prelude.Text,
    -- | The ARN of the thing to associate with the wireless device.
    thingArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateWirelessDeviceWithThing_id' - The ID of the resource to update.
--
-- 'thingArn', 'associateWirelessDeviceWithThing_thingArn' - The ARN of the thing to associate with the wireless device.
newAssociateWirelessDeviceWithThing ::
  -- | 'id'
  Prelude.Text ->
  -- | 'thingArn'
  Prelude.Text ->
  AssociateWirelessDeviceWithThing
newAssociateWirelessDeviceWithThing pId_ pThingArn_ =
  AssociateWirelessDeviceWithThing'
    { id = pId_,
      thingArn = pThingArn_
    }

-- | The ID of the resource to update.
associateWirelessDeviceWithThing_id :: Lens.Lens' AssociateWirelessDeviceWithThing Prelude.Text
associateWirelessDeviceWithThing_id = Lens.lens (\AssociateWirelessDeviceWithThing' {id} -> id) (\s@AssociateWirelessDeviceWithThing' {} a -> s {id = a} :: AssociateWirelessDeviceWithThing)

-- | The ARN of the thing to associate with the wireless device.
associateWirelessDeviceWithThing_thingArn :: Lens.Lens' AssociateWirelessDeviceWithThing Prelude.Text
associateWirelessDeviceWithThing_thingArn = Lens.lens (\AssociateWirelessDeviceWithThing' {thingArn} -> thingArn) (\s@AssociateWirelessDeviceWithThing' {} a -> s {thingArn = a} :: AssociateWirelessDeviceWithThing)

instance
  Core.AWSRequest
    AssociateWirelessDeviceWithThing
  where
  type
    AWSResponse AssociateWirelessDeviceWithThing =
      AssociateWirelessDeviceWithThingResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWirelessDeviceWithThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWirelessDeviceWithThing
  where
  hashWithSalt
    _salt
    AssociateWirelessDeviceWithThing' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` thingArn

instance
  Prelude.NFData
    AssociateWirelessDeviceWithThing
  where
  rnf AssociateWirelessDeviceWithThing' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf thingArn

instance
  Data.ToHeaders
    AssociateWirelessDeviceWithThing
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AssociateWirelessDeviceWithThing where
  toJSON AssociateWirelessDeviceWithThing' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ThingArn" Data..= thingArn)]
      )

instance Data.ToPath AssociateWirelessDeviceWithThing where
  toPath AssociateWirelessDeviceWithThing' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id, "/thing"]

instance
  Data.ToQuery
    AssociateWirelessDeviceWithThing
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWirelessDeviceWithThingResponse' smart constructor.
data AssociateWirelessDeviceWithThingResponse = AssociateWirelessDeviceWithThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateWirelessDeviceWithThingResponse_httpStatus' - The response's http status code.
newAssociateWirelessDeviceWithThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWirelessDeviceWithThingResponse
newAssociateWirelessDeviceWithThingResponse
  pHttpStatus_ =
    AssociateWirelessDeviceWithThingResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateWirelessDeviceWithThingResponse_httpStatus :: Lens.Lens' AssociateWirelessDeviceWithThingResponse Prelude.Int
associateWirelessDeviceWithThingResponse_httpStatus = Lens.lens (\AssociateWirelessDeviceWithThingResponse' {httpStatus} -> httpStatus) (\s@AssociateWirelessDeviceWithThingResponse' {} a -> s {httpStatus = a} :: AssociateWirelessDeviceWithThingResponse)

instance
  Prelude.NFData
    AssociateWirelessDeviceWithThingResponse
  where
  rnf AssociateWirelessDeviceWithThingResponse' {..} =
    Prelude.rnf httpStatus
