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
-- Module      : Network.AWS.IoTData.UpdateThingShadow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shadow for the specified thing.
--
-- For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html UpdateThingShadow>
-- in the AWS IoT Developer Guide.
module Network.AWS.IoTData.UpdateThingShadow
  ( -- * Creating a Request
    UpdateThingShadow (..),
    newUpdateThingShadow,

    -- * Request Lenses
    updateThingShadow_shadowName,
    updateThingShadow_thingName,
    updateThingShadow_payload,

    -- * Destructuring the Response
    UpdateThingShadowResponse (..),
    newUpdateThingShadowResponse,

    -- * Response Lenses
    updateThingShadowResponse_payload,
    updateThingShadowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateThingShadow operation.
--
-- /See:/ 'newUpdateThingShadow' smart constructor.
data UpdateThingShadow = UpdateThingShadow'
  { -- | The name of the shadow.
    shadowName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Text,
    -- | The state information, in JSON format.
    payload :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingShadow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shadowName', 'updateThingShadow_shadowName' - The name of the shadow.
--
-- 'thingName', 'updateThingShadow_thingName' - The name of the thing.
--
-- 'payload', 'updateThingShadow_payload' - The state information, in JSON format.
newUpdateThingShadow ::
  -- | 'thingName'
  Prelude.Text ->
  -- | 'payload'
  Prelude.ByteString ->
  UpdateThingShadow
newUpdateThingShadow pThingName_ pPayload_ =
  UpdateThingShadow'
    { shadowName = Prelude.Nothing,
      thingName = pThingName_,
      payload = pPayload_
    }

-- | The name of the shadow.
updateThingShadow_shadowName :: Lens.Lens' UpdateThingShadow (Prelude.Maybe Prelude.Text)
updateThingShadow_shadowName = Lens.lens (\UpdateThingShadow' {shadowName} -> shadowName) (\s@UpdateThingShadow' {} a -> s {shadowName = a} :: UpdateThingShadow)

-- | The name of the thing.
updateThingShadow_thingName :: Lens.Lens' UpdateThingShadow Prelude.Text
updateThingShadow_thingName = Lens.lens (\UpdateThingShadow' {thingName} -> thingName) (\s@UpdateThingShadow' {} a -> s {thingName = a} :: UpdateThingShadow)

-- | The state information, in JSON format.
updateThingShadow_payload :: Lens.Lens' UpdateThingShadow Prelude.ByteString
updateThingShadow_payload = Lens.lens (\UpdateThingShadow' {payload} -> payload) (\s@UpdateThingShadow' {} a -> s {payload = a} :: UpdateThingShadow)

instance Core.AWSRequest UpdateThingShadow where
  type
    AWSResponse UpdateThingShadow =
      UpdateThingShadowResponse
  request = Request.postBody defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          UpdateThingShadowResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just x))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingShadow

instance Prelude.NFData UpdateThingShadow

instance Core.ToBody UpdateThingShadow where
  toBody UpdateThingShadow' {..} = Core.toBody payload

instance Core.ToHeaders UpdateThingShadow where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateThingShadow where
  toPath UpdateThingShadow' {..} =
    Prelude.mconcat
      ["/things/", Core.toBS thingName, "/shadow"]

instance Core.ToQuery UpdateThingShadow where
  toQuery UpdateThingShadow' {..} =
    Prelude.mconcat ["name" Core.=: shadowName]

-- | The output from the UpdateThingShadow operation.
--
-- /See:/ 'newUpdateThingShadowResponse' smart constructor.
data UpdateThingShadowResponse = UpdateThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThingShadowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'updateThingShadowResponse_payload' - The state information, in JSON format.
--
-- 'httpStatus', 'updateThingShadowResponse_httpStatus' - The response's http status code.
newUpdateThingShadowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThingShadowResponse
newUpdateThingShadowResponse pHttpStatus_ =
  UpdateThingShadowResponse'
    { payload =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state information, in JSON format.
updateThingShadowResponse_payload :: Lens.Lens' UpdateThingShadowResponse (Prelude.Maybe Prelude.ByteString)
updateThingShadowResponse_payload = Lens.lens (\UpdateThingShadowResponse' {payload} -> payload) (\s@UpdateThingShadowResponse' {} a -> s {payload = a} :: UpdateThingShadowResponse)

-- | The response's http status code.
updateThingShadowResponse_httpStatus :: Lens.Lens' UpdateThingShadowResponse Prelude.Int
updateThingShadowResponse_httpStatus = Lens.lens (\UpdateThingShadowResponse' {httpStatus} -> httpStatus) (\s@UpdateThingShadowResponse' {} a -> s {httpStatus = a} :: UpdateThingShadowResponse)

instance Prelude.NFData UpdateThingShadowResponse
