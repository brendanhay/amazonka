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
-- Module      : Amazonka.IoTData.UpdateThingShadow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shadow for the specified thing.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateThingShadow>
-- action.
--
-- For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html UpdateThingShadow>
-- in the IoT Developer Guide.
module Amazonka.IoTData.UpdateThingShadow
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          UpdateThingShadowResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThingShadow where
  hashWithSalt _salt UpdateThingShadow' {..} =
    _salt
      `Prelude.hashWithSalt` shadowName
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` payload

instance Prelude.NFData UpdateThingShadow where
  rnf UpdateThingShadow' {..} =
    Prelude.rnf shadowName
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf payload

instance Data.ToBody UpdateThingShadow where
  toBody UpdateThingShadow' {..} = Data.toBody payload

instance Data.ToHeaders UpdateThingShadow where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateThingShadow where
  toPath UpdateThingShadow' {..} =
    Prelude.mconcat
      ["/things/", Data.toBS thingName, "/shadow"]

instance Data.ToQuery UpdateThingShadow where
  toQuery UpdateThingShadow' {..} =
    Prelude.mconcat ["name" Data.=: shadowName]

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

instance Prelude.NFData UpdateThingShadowResponse where
  rnf UpdateThingShadowResponse' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf httpStatus
