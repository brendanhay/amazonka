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
-- Module      : Network.AWS.IoTData.GetThingShadow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the shadow for the specified thing.
--
-- For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html GetThingShadow>
-- in the AWS IoT Developer Guide.
module Network.AWS.IoTData.GetThingShadow
  ( -- * Creating a Request
    GetThingShadow (..),
    newGetThingShadow,

    -- * Request Lenses
    getThingShadow_shadowName,
    getThingShadow_thingName,

    -- * Destructuring the Response
    GetThingShadowResponse (..),
    newGetThingShadowResponse,

    -- * Response Lenses
    getThingShadowResponse_payload,
    getThingShadowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetThingShadow operation.
--
-- /See:/ 'newGetThingShadow' smart constructor.
data GetThingShadow = GetThingShadow'
  { -- | The name of the shadow.
    shadowName :: Core.Maybe Core.Text,
    -- | The name of the thing.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetThingShadow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shadowName', 'getThingShadow_shadowName' - The name of the shadow.
--
-- 'thingName', 'getThingShadow_thingName' - The name of the thing.
newGetThingShadow ::
  -- | 'thingName'
  Core.Text ->
  GetThingShadow
newGetThingShadow pThingName_ =
  GetThingShadow'
    { shadowName = Core.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
getThingShadow_shadowName :: Lens.Lens' GetThingShadow (Core.Maybe Core.Text)
getThingShadow_shadowName = Lens.lens (\GetThingShadow' {shadowName} -> shadowName) (\s@GetThingShadow' {} a -> s {shadowName = a} :: GetThingShadow)

-- | The name of the thing.
getThingShadow_thingName :: Lens.Lens' GetThingShadow Core.Text
getThingShadow_thingName = Lens.lens (\GetThingShadow' {thingName} -> thingName) (\s@GetThingShadow' {} a -> s {thingName = a} :: GetThingShadow)

instance Core.AWSRequest GetThingShadow where
  type
    AWSResponse GetThingShadow =
      GetThingShadowResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetThingShadowResponse'
            Core.<$> (Core.pure (Core.Just x))
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetThingShadow

instance Core.NFData GetThingShadow

instance Core.ToHeaders GetThingShadow where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetThingShadow where
  toPath GetThingShadow' {..} =
    Core.mconcat
      ["/things/", Core.toBS thingName, "/shadow"]

instance Core.ToQuery GetThingShadow where
  toQuery GetThingShadow' {..} =
    Core.mconcat ["name" Core.=: shadowName]

-- | The output from the GetThingShadow operation.
--
-- /See:/ 'newGetThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Core.Maybe Core.ByteString,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetThingShadowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'getThingShadowResponse_payload' - The state information, in JSON format.
--
-- 'httpStatus', 'getThingShadowResponse_httpStatus' - The response's http status code.
newGetThingShadowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetThingShadowResponse
newGetThingShadowResponse pHttpStatus_ =
  GetThingShadowResponse'
    { payload = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state information, in JSON format.
getThingShadowResponse_payload :: Lens.Lens' GetThingShadowResponse (Core.Maybe Core.ByteString)
getThingShadowResponse_payload = Lens.lens (\GetThingShadowResponse' {payload} -> payload) (\s@GetThingShadowResponse' {} a -> s {payload = a} :: GetThingShadowResponse)

-- | The response's http status code.
getThingShadowResponse_httpStatus :: Lens.Lens' GetThingShadowResponse Core.Int
getThingShadowResponse_httpStatus = Lens.lens (\GetThingShadowResponse' {httpStatus} -> httpStatus) (\s@GetThingShadowResponse' {} a -> s {httpStatus = a} :: GetThingShadowResponse)

instance Core.NFData GetThingShadowResponse
