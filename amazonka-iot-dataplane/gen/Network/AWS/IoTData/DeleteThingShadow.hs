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
-- Module      : Network.AWS.IoTData.DeleteThingShadow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the shadow for the specified thing.
--
-- For more information, see
-- <http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html DeleteThingShadow>
-- in the AWS IoT Developer Guide.
module Network.AWS.IoTData.DeleteThingShadow
  ( -- * Creating a Request
    DeleteThingShadow (..),
    newDeleteThingShadow,

    -- * Request Lenses
    deleteThingShadow_shadowName,
    deleteThingShadow_thingName,

    -- * Destructuring the Response
    DeleteThingShadowResponse (..),
    newDeleteThingShadowResponse,

    -- * Response Lenses
    deleteThingShadowResponse_httpStatus,
    deleteThingShadowResponse_payload,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingShadow operation.
--
-- /See:/ 'newDeleteThingShadow' smart constructor.
data DeleteThingShadow = DeleteThingShadow'
  { -- | The name of the shadow.
    shadowName :: Core.Maybe Core.Text,
    -- | The name of the thing.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThingShadow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shadowName', 'deleteThingShadow_shadowName' - The name of the shadow.
--
-- 'thingName', 'deleteThingShadow_thingName' - The name of the thing.
newDeleteThingShadow ::
  -- | 'thingName'
  Core.Text ->
  DeleteThingShadow
newDeleteThingShadow pThingName_ =
  DeleteThingShadow'
    { shadowName = Core.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
deleteThingShadow_shadowName :: Lens.Lens' DeleteThingShadow (Core.Maybe Core.Text)
deleteThingShadow_shadowName = Lens.lens (\DeleteThingShadow' {shadowName} -> shadowName) (\s@DeleteThingShadow' {} a -> s {shadowName = a} :: DeleteThingShadow)

-- | The name of the thing.
deleteThingShadow_thingName :: Lens.Lens' DeleteThingShadow Core.Text
deleteThingShadow_thingName = Lens.lens (\DeleteThingShadow' {thingName} -> thingName) (\s@DeleteThingShadow' {} a -> s {thingName = a} :: DeleteThingShadow)

instance Core.AWSRequest DeleteThingShadow where
  type
    AWSResponse DeleteThingShadow =
      DeleteThingShadowResponse
  request = Request.delete defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          DeleteThingShadowResponse'
            Core.<$> (Core.pure (Core.fromEnum s)) Core.<*> (Core.pure x)
      )

instance Core.Hashable DeleteThingShadow

instance Core.NFData DeleteThingShadow

instance Core.ToHeaders DeleteThingShadow where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteThingShadow where
  toPath DeleteThingShadow' {..} =
    Core.mconcat
      ["/things/", Core.toBS thingName, "/shadow"]

instance Core.ToQuery DeleteThingShadow where
  toQuery DeleteThingShadow' {..} =
    Core.mconcat ["name" Core.=: shadowName]

-- | The output from the DeleteThingShadow operation.
--
-- /See:/ 'newDeleteThingShadowResponse' smart constructor.
data DeleteThingShadowResponse = DeleteThingShadowResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The state information, in JSON format.
    payload :: Core.ByteString
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThingShadowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteThingShadowResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'deleteThingShadowResponse_payload' - The state information, in JSON format.
newDeleteThingShadowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'payload'
  Core.ByteString ->
  DeleteThingShadowResponse
newDeleteThingShadowResponse pHttpStatus_ pPayload_ =
  DeleteThingShadowResponse'
    { httpStatus =
        pHttpStatus_,
      payload = pPayload_
    }

-- | The response's http status code.
deleteThingShadowResponse_httpStatus :: Lens.Lens' DeleteThingShadowResponse Core.Int
deleteThingShadowResponse_httpStatus = Lens.lens (\DeleteThingShadowResponse' {httpStatus} -> httpStatus) (\s@DeleteThingShadowResponse' {} a -> s {httpStatus = a} :: DeleteThingShadowResponse)

-- | The state information, in JSON format.
deleteThingShadowResponse_payload :: Lens.Lens' DeleteThingShadowResponse Core.ByteString
deleteThingShadowResponse_payload = Lens.lens (\DeleteThingShadowResponse' {payload} -> payload) (\s@DeleteThingShadowResponse' {} a -> s {payload = a} :: DeleteThingShadowResponse)

instance Core.NFData DeleteThingShadowResponse
