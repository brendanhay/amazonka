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

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetThingShadow operation.
--
-- /See:/ 'newGetThingShadow' smart constructor.
data GetThingShadow = GetThingShadow'
  { -- | The name of the shadow.
    shadowName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetThingShadow
newGetThingShadow pThingName_ =
  GetThingShadow'
    { shadowName = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
getThingShadow_shadowName :: Lens.Lens' GetThingShadow (Prelude.Maybe Prelude.Text)
getThingShadow_shadowName = Lens.lens (\GetThingShadow' {shadowName} -> shadowName) (\s@GetThingShadow' {} a -> s {shadowName = a} :: GetThingShadow)

-- | The name of the thing.
getThingShadow_thingName :: Lens.Lens' GetThingShadow Prelude.Text
getThingShadow_thingName = Lens.lens (\GetThingShadow' {thingName} -> thingName) (\s@GetThingShadow' {} a -> s {thingName = a} :: GetThingShadow)

instance Prelude.AWSRequest GetThingShadow where
  type Rs GetThingShadow = GetThingShadowResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetThingShadowResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just x))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetThingShadow

instance Prelude.NFData GetThingShadow

instance Prelude.ToHeaders GetThingShadow where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetThingShadow where
  toPath GetThingShadow' {..} =
    Prelude.mconcat
      ["/things/", Prelude.toBS thingName, "/shadow"]

instance Prelude.ToQuery GetThingShadow where
  toQuery GetThingShadow' {..} =
    Prelude.mconcat ["name" Prelude.=: shadowName]

-- | The output from the GetThingShadow operation.
--
-- /See:/ 'newGetThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetThingShadowResponse
newGetThingShadowResponse pHttpStatus_ =
  GetThingShadowResponse'
    { payload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state information, in JSON format.
getThingShadowResponse_payload :: Lens.Lens' GetThingShadowResponse (Prelude.Maybe Prelude.ByteString)
getThingShadowResponse_payload = Lens.lens (\GetThingShadowResponse' {payload} -> payload) (\s@GetThingShadowResponse' {} a -> s {payload = a} :: GetThingShadowResponse)

-- | The response's http status code.
getThingShadowResponse_httpStatus :: Lens.Lens' GetThingShadowResponse Prelude.Int
getThingShadowResponse_httpStatus = Lens.lens (\GetThingShadowResponse' {httpStatus} -> httpStatus) (\s@GetThingShadowResponse' {} a -> s {httpStatus = a} :: GetThingShadowResponse)

instance Prelude.NFData GetThingShadowResponse
