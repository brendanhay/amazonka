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
-- Module      : Network.AWS.IoTWireless.AssociateWirelessGatewayWithThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a wireless gateway with a thing.
module Network.AWS.IoTWireless.AssociateWirelessGatewayWithThing
  ( -- * Creating a Request
    AssociateWirelessGatewayWithThing (..),
    newAssociateWirelessGatewayWithThing,

    -- * Request Lenses
    associateWirelessGatewayWithThing_id,
    associateWirelessGatewayWithThing_thingArn,

    -- * Destructuring the Response
    AssociateWirelessGatewayWithThingResponse (..),
    newAssociateWirelessGatewayWithThingResponse,

    -- * Response Lenses
    associateWirelessGatewayWithThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateWirelessGatewayWithThing' smart constructor.
data AssociateWirelessGatewayWithThing = AssociateWirelessGatewayWithThing'
  { -- | The ID of the resource to update.
    id :: Prelude.Text,
    -- | The ARN of the thing to associate with the wireless gateway.
    thingArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessGatewayWithThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateWirelessGatewayWithThing_id' - The ID of the resource to update.
--
-- 'thingArn', 'associateWirelessGatewayWithThing_thingArn' - The ARN of the thing to associate with the wireless gateway.
newAssociateWirelessGatewayWithThing ::
  -- | 'id'
  Prelude.Text ->
  -- | 'thingArn'
  Prelude.Text ->
  AssociateWirelessGatewayWithThing
newAssociateWirelessGatewayWithThing pId_ pThingArn_ =
  AssociateWirelessGatewayWithThing'
    { id = pId_,
      thingArn = pThingArn_
    }

-- | The ID of the resource to update.
associateWirelessGatewayWithThing_id :: Lens.Lens' AssociateWirelessGatewayWithThing Prelude.Text
associateWirelessGatewayWithThing_id = Lens.lens (\AssociateWirelessGatewayWithThing' {id} -> id) (\s@AssociateWirelessGatewayWithThing' {} a -> s {id = a} :: AssociateWirelessGatewayWithThing)

-- | The ARN of the thing to associate with the wireless gateway.
associateWirelessGatewayWithThing_thingArn :: Lens.Lens' AssociateWirelessGatewayWithThing Prelude.Text
associateWirelessGatewayWithThing_thingArn = Lens.lens (\AssociateWirelessGatewayWithThing' {thingArn} -> thingArn) (\s@AssociateWirelessGatewayWithThing' {} a -> s {thingArn = a} :: AssociateWirelessGatewayWithThing)

instance
  Core.AWSRequest
    AssociateWirelessGatewayWithThing
  where
  type
    AWSResponse AssociateWirelessGatewayWithThing =
      AssociateWirelessGatewayWithThingResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWirelessGatewayWithThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWirelessGatewayWithThing

instance
  Prelude.NFData
    AssociateWirelessGatewayWithThing

instance
  Core.ToHeaders
    AssociateWirelessGatewayWithThing
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    AssociateWirelessGatewayWithThing
  where
  toJSON AssociateWirelessGatewayWithThing' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ThingArn" Core..= thingArn)]
      )

instance
  Core.ToPath
    AssociateWirelessGatewayWithThing
  where
  toPath AssociateWirelessGatewayWithThing' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Core.toBS id, "/thing"]

instance
  Core.ToQuery
    AssociateWirelessGatewayWithThing
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWirelessGatewayWithThingResponse' smart constructor.
data AssociateWirelessGatewayWithThingResponse = AssociateWirelessGatewayWithThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessGatewayWithThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateWirelessGatewayWithThingResponse_httpStatus' - The response's http status code.
newAssociateWirelessGatewayWithThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWirelessGatewayWithThingResponse
newAssociateWirelessGatewayWithThingResponse
  pHttpStatus_ =
    AssociateWirelessGatewayWithThingResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateWirelessGatewayWithThingResponse_httpStatus :: Lens.Lens' AssociateWirelessGatewayWithThingResponse Prelude.Int
associateWirelessGatewayWithThingResponse_httpStatus = Lens.lens (\AssociateWirelessGatewayWithThingResponse' {httpStatus} -> httpStatus) (\s@AssociateWirelessGatewayWithThingResponse' {} a -> s {httpStatus = a} :: AssociateWirelessGatewayWithThingResponse)

instance
  Prelude.NFData
    AssociateWirelessGatewayWithThingResponse
