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
-- Module      : Amazonka.ChimeSDKIdentity.UpdateAppInstanceUserEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an @AppInstanceUserEndpoint@. You can update the
-- name and @AllowMessage@ values.
module Amazonka.ChimeSDKIdentity.UpdateAppInstanceUserEndpoint
  ( -- * Creating a Request
    UpdateAppInstanceUserEndpoint (..),
    newUpdateAppInstanceUserEndpoint,

    -- * Request Lenses
    updateAppInstanceUserEndpoint_allowMessages,
    updateAppInstanceUserEndpoint_name,
    updateAppInstanceUserEndpoint_appInstanceUserArn,
    updateAppInstanceUserEndpoint_endpointId,

    -- * Destructuring the Response
    UpdateAppInstanceUserEndpointResponse (..),
    newUpdateAppInstanceUserEndpointResponse,

    -- * Response Lenses
    updateAppInstanceUserEndpointResponse_appInstanceUserArn,
    updateAppInstanceUserEndpointResponse_endpointId,
    updateAppInstanceUserEndpointResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppInstanceUserEndpoint' smart constructor.
data UpdateAppInstanceUserEndpoint = UpdateAppInstanceUserEndpoint'
  { -- | Boolean that controls whether the @AppInstanceUserEndpoint@ is opted in
    -- to receive messages. @ALL@ indicates the endpoint will receive all
    -- messages. @NONE@ indicates the endpoint will receive no messages.
    allowMessages :: Prelude.Maybe AllowMessages,
    -- | The name of the @AppInstanceUserEndpoint@.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Data.Sensitive Prelude.Text,
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceUserEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowMessages', 'updateAppInstanceUserEndpoint_allowMessages' - Boolean that controls whether the @AppInstanceUserEndpoint@ is opted in
-- to receive messages. @ALL@ indicates the endpoint will receive all
-- messages. @NONE@ indicates the endpoint will receive no messages.
--
-- 'name', 'updateAppInstanceUserEndpoint_name' - The name of the @AppInstanceUserEndpoint@.
--
-- 'appInstanceUserArn', 'updateAppInstanceUserEndpoint_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'updateAppInstanceUserEndpoint_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
newUpdateAppInstanceUserEndpoint ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  UpdateAppInstanceUserEndpoint
newUpdateAppInstanceUserEndpoint
  pAppInstanceUserArn_
  pEndpointId_ =
    UpdateAppInstanceUserEndpoint'
      { allowMessages =
          Prelude.Nothing,
        name = Prelude.Nothing,
        appInstanceUserArn =
          Data._Sensitive
            Lens.# pAppInstanceUserArn_,
        endpointId =
          Data._Sensitive Lens.# pEndpointId_
      }

-- | Boolean that controls whether the @AppInstanceUserEndpoint@ is opted in
-- to receive messages. @ALL@ indicates the endpoint will receive all
-- messages. @NONE@ indicates the endpoint will receive no messages.
updateAppInstanceUserEndpoint_allowMessages :: Lens.Lens' UpdateAppInstanceUserEndpoint (Prelude.Maybe AllowMessages)
updateAppInstanceUserEndpoint_allowMessages = Lens.lens (\UpdateAppInstanceUserEndpoint' {allowMessages} -> allowMessages) (\s@UpdateAppInstanceUserEndpoint' {} a -> s {allowMessages = a} :: UpdateAppInstanceUserEndpoint)

-- | The name of the @AppInstanceUserEndpoint@.
updateAppInstanceUserEndpoint_name :: Lens.Lens' UpdateAppInstanceUserEndpoint (Prelude.Maybe Prelude.Text)
updateAppInstanceUserEndpoint_name = Lens.lens (\UpdateAppInstanceUserEndpoint' {name} -> name) (\s@UpdateAppInstanceUserEndpoint' {} a -> s {name = a} :: UpdateAppInstanceUserEndpoint) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the @AppInstanceUser@.
updateAppInstanceUserEndpoint_appInstanceUserArn :: Lens.Lens' UpdateAppInstanceUserEndpoint Prelude.Text
updateAppInstanceUserEndpoint_appInstanceUserArn = Lens.lens (\UpdateAppInstanceUserEndpoint' {appInstanceUserArn} -> appInstanceUserArn) (\s@UpdateAppInstanceUserEndpoint' {} a -> s {appInstanceUserArn = a} :: UpdateAppInstanceUserEndpoint) Prelude.. Data._Sensitive

-- | The unique identifier of the @AppInstanceUserEndpoint@.
updateAppInstanceUserEndpoint_endpointId :: Lens.Lens' UpdateAppInstanceUserEndpoint Prelude.Text
updateAppInstanceUserEndpoint_endpointId = Lens.lens (\UpdateAppInstanceUserEndpoint' {endpointId} -> endpointId) (\s@UpdateAppInstanceUserEndpoint' {} a -> s {endpointId = a} :: UpdateAppInstanceUserEndpoint) Prelude.. Data._Sensitive

instance
  Core.AWSRequest
    UpdateAppInstanceUserEndpoint
  where
  type
    AWSResponse UpdateAppInstanceUserEndpoint =
      UpdateAppInstanceUserEndpointResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppInstanceUserEndpointResponse'
            Prelude.<$> (x Data..?> "AppInstanceUserArn")
            Prelude.<*> (x Data..?> "EndpointId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAppInstanceUserEndpoint
  where
  hashWithSalt _salt UpdateAppInstanceUserEndpoint' {..} =
    _salt `Prelude.hashWithSalt` allowMessages
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` appInstanceUserArn
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData UpdateAppInstanceUserEndpoint where
  rnf UpdateAppInstanceUserEndpoint' {..} =
    Prelude.rnf allowMessages
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId

instance Data.ToHeaders UpdateAppInstanceUserEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAppInstanceUserEndpoint where
  toJSON UpdateAppInstanceUserEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowMessages" Data..=) Prelude.<$> allowMessages,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateAppInstanceUserEndpoint where
  toPath UpdateAppInstanceUserEndpoint' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn,
        "/endpoints/",
        Data.toBS endpointId
      ]

instance Data.ToQuery UpdateAppInstanceUserEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppInstanceUserEndpointResponse' smart constructor.
data UpdateAppInstanceUserEndpointResponse = UpdateAppInstanceUserEndpointResponse'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceUserEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'updateAppInstanceUserEndpointResponse_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'updateAppInstanceUserEndpointResponse_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
--
-- 'httpStatus', 'updateAppInstanceUserEndpointResponse_httpStatus' - The response's http status code.
newUpdateAppInstanceUserEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAppInstanceUserEndpointResponse
newUpdateAppInstanceUserEndpointResponse pHttpStatus_ =
  UpdateAppInstanceUserEndpointResponse'
    { appInstanceUserArn =
        Prelude.Nothing,
      endpointId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the @AppInstanceUser@.
updateAppInstanceUserEndpointResponse_appInstanceUserArn :: Lens.Lens' UpdateAppInstanceUserEndpointResponse (Prelude.Maybe Prelude.Text)
updateAppInstanceUserEndpointResponse_appInstanceUserArn = Lens.lens (\UpdateAppInstanceUserEndpointResponse' {appInstanceUserArn} -> appInstanceUserArn) (\s@UpdateAppInstanceUserEndpointResponse' {} a -> s {appInstanceUserArn = a} :: UpdateAppInstanceUserEndpointResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier of the @AppInstanceUserEndpoint@.
updateAppInstanceUserEndpointResponse_endpointId :: Lens.Lens' UpdateAppInstanceUserEndpointResponse (Prelude.Maybe Prelude.Text)
updateAppInstanceUserEndpointResponse_endpointId = Lens.lens (\UpdateAppInstanceUserEndpointResponse' {endpointId} -> endpointId) (\s@UpdateAppInstanceUserEndpointResponse' {} a -> s {endpointId = a} :: UpdateAppInstanceUserEndpointResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
updateAppInstanceUserEndpointResponse_httpStatus :: Lens.Lens' UpdateAppInstanceUserEndpointResponse Prelude.Int
updateAppInstanceUserEndpointResponse_httpStatus = Lens.lens (\UpdateAppInstanceUserEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateAppInstanceUserEndpointResponse' {} a -> s {httpStatus = a} :: UpdateAppInstanceUserEndpointResponse)

instance
  Prelude.NFData
    UpdateAppInstanceUserEndpointResponse
  where
  rnf UpdateAppInstanceUserEndpointResponse' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf httpStatus
