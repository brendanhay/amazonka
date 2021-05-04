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

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingShadow operation.
--
-- /See:/ 'newDeleteThingShadow' smart constructor.
data DeleteThingShadow = DeleteThingShadow'
  { -- | The name of the shadow.
    shadowName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteThingShadow
newDeleteThingShadow pThingName_ =
  DeleteThingShadow'
    { shadowName = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
deleteThingShadow_shadowName :: Lens.Lens' DeleteThingShadow (Prelude.Maybe Prelude.Text)
deleteThingShadow_shadowName = Lens.lens (\DeleteThingShadow' {shadowName} -> shadowName) (\s@DeleteThingShadow' {} a -> s {shadowName = a} :: DeleteThingShadow)

-- | The name of the thing.
deleteThingShadow_thingName :: Lens.Lens' DeleteThingShadow Prelude.Text
deleteThingShadow_thingName = Lens.lens (\DeleteThingShadow' {thingName} -> thingName) (\s@DeleteThingShadow' {} a -> s {thingName = a} :: DeleteThingShadow)

instance Prelude.AWSRequest DeleteThingShadow where
  type Rs DeleteThingShadow = DeleteThingShadowResponse
  request = Request.delete defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          DeleteThingShadowResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable DeleteThingShadow

instance Prelude.NFData DeleteThingShadow

instance Prelude.ToHeaders DeleteThingShadow where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteThingShadow where
  toPath DeleteThingShadow' {..} =
    Prelude.mconcat
      ["/things/", Prelude.toBS thingName, "/shadow"]

instance Prelude.ToQuery DeleteThingShadow where
  toQuery DeleteThingShadow' {..} =
    Prelude.mconcat ["name" Prelude.=: shadowName]

-- | The output from the DeleteThingShadow operation.
--
-- /See:/ 'newDeleteThingShadowResponse' smart constructor.
data DeleteThingShadowResponse = DeleteThingShadowResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The state information, in JSON format.
    payload :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'payload'
  Prelude.ByteString ->
  DeleteThingShadowResponse
newDeleteThingShadowResponse pHttpStatus_ pPayload_ =
  DeleteThingShadowResponse'
    { httpStatus =
        pHttpStatus_,
      payload = pPayload_
    }

-- | The response's http status code.
deleteThingShadowResponse_httpStatus :: Lens.Lens' DeleteThingShadowResponse Prelude.Int
deleteThingShadowResponse_httpStatus = Lens.lens (\DeleteThingShadowResponse' {httpStatus} -> httpStatus) (\s@DeleteThingShadowResponse' {} a -> s {httpStatus = a} :: DeleteThingShadowResponse)

-- | The state information, in JSON format.
deleteThingShadowResponse_payload :: Lens.Lens' DeleteThingShadowResponse Prelude.ByteString
deleteThingShadowResponse_payload = Lens.lens (\DeleteThingShadowResponse' {payload} -> payload) (\s@DeleteThingShadowResponse' {} a -> s {payload = a} :: DeleteThingShadowResponse)

instance Prelude.NFData DeleteThingShadowResponse
