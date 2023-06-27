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
-- Module      : Amazonka.ChimeSDKIdentity.RegisterAppInstanceUserEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an endpoint under an Amazon Chime @AppInstanceUser@. The
-- endpoint receives messages for a user. For push notifications, the
-- endpoint is a mobile device used to receive mobile push notifications
-- for a user.
module Amazonka.ChimeSDKIdentity.RegisterAppInstanceUserEndpoint
  ( -- * Creating a Request
    RegisterAppInstanceUserEndpoint (..),
    newRegisterAppInstanceUserEndpoint,

    -- * Request Lenses
    registerAppInstanceUserEndpoint_allowMessages,
    registerAppInstanceUserEndpoint_name,
    registerAppInstanceUserEndpoint_appInstanceUserArn,
    registerAppInstanceUserEndpoint_type,
    registerAppInstanceUserEndpoint_resourceArn,
    registerAppInstanceUserEndpoint_endpointAttributes,
    registerAppInstanceUserEndpoint_clientRequestToken,

    -- * Destructuring the Response
    RegisterAppInstanceUserEndpointResponse (..),
    newRegisterAppInstanceUserEndpointResponse,

    -- * Response Lenses
    registerAppInstanceUserEndpointResponse_appInstanceUserArn,
    registerAppInstanceUserEndpointResponse_endpointId,
    registerAppInstanceUserEndpointResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterAppInstanceUserEndpoint' smart constructor.
data RegisterAppInstanceUserEndpoint = RegisterAppInstanceUserEndpoint'
  { -- | Boolean that controls whether the AppInstanceUserEndpoint is opted in to
    -- receive messages. @ALL@ indicates the endpoint receives all messages.
    -- @NONE@ indicates the endpoint receives no messages.
    allowMessages :: Prelude.Maybe AllowMessages,
    -- | The name of the @AppInstanceUserEndpoint@.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Data.Sensitive Prelude.Text,
    -- | The type of the @AppInstanceUserEndpoint@. Supported types:
    --
    -- -   @APNS@: The mobile notification service for an Apple device.
    --
    -- -   @APNS_SANDBOX@: The sandbox environment of the mobile notification
    --     service for an Apple device.
    --
    -- -   @GCM@: The mobile notification service for an Android device.
    --
    -- Populate the @ResourceArn@ value of each type as @PinpointAppArn@.
    type' :: AppInstanceUserEndpointType,
    -- | The ARN of the resource to which the endpoint belongs.
    resourceArn :: Prelude.Text,
    -- | The attributes of an @Endpoint@.
    endpointAttributes :: EndpointAttributes,
    -- | The unique ID assigned to the request. Use different tokens to register
    -- other endpoints.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAppInstanceUserEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowMessages', 'registerAppInstanceUserEndpoint_allowMessages' - Boolean that controls whether the AppInstanceUserEndpoint is opted in to
-- receive messages. @ALL@ indicates the endpoint receives all messages.
-- @NONE@ indicates the endpoint receives no messages.
--
-- 'name', 'registerAppInstanceUserEndpoint_name' - The name of the @AppInstanceUserEndpoint@.
--
-- 'appInstanceUserArn', 'registerAppInstanceUserEndpoint_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'type'', 'registerAppInstanceUserEndpoint_type' - The type of the @AppInstanceUserEndpoint@. Supported types:
--
-- -   @APNS@: The mobile notification service for an Apple device.
--
-- -   @APNS_SANDBOX@: The sandbox environment of the mobile notification
--     service for an Apple device.
--
-- -   @GCM@: The mobile notification service for an Android device.
--
-- Populate the @ResourceArn@ value of each type as @PinpointAppArn@.
--
-- 'resourceArn', 'registerAppInstanceUserEndpoint_resourceArn' - The ARN of the resource to which the endpoint belongs.
--
-- 'endpointAttributes', 'registerAppInstanceUserEndpoint_endpointAttributes' - The attributes of an @Endpoint@.
--
-- 'clientRequestToken', 'registerAppInstanceUserEndpoint_clientRequestToken' - The unique ID assigned to the request. Use different tokens to register
-- other endpoints.
newRegisterAppInstanceUserEndpoint ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  -- | 'type''
  AppInstanceUserEndpointType ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'endpointAttributes'
  EndpointAttributes ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  RegisterAppInstanceUserEndpoint
newRegisterAppInstanceUserEndpoint
  pAppInstanceUserArn_
  pType_
  pResourceArn_
  pEndpointAttributes_
  pClientRequestToken_ =
    RegisterAppInstanceUserEndpoint'
      { allowMessages =
          Prelude.Nothing,
        name = Prelude.Nothing,
        appInstanceUserArn =
          Data._Sensitive
            Lens.# pAppInstanceUserArn_,
        type' = pType_,
        resourceArn = pResourceArn_,
        endpointAttributes = pEndpointAttributes_,
        clientRequestToken = pClientRequestToken_
      }

-- | Boolean that controls whether the AppInstanceUserEndpoint is opted in to
-- receive messages. @ALL@ indicates the endpoint receives all messages.
-- @NONE@ indicates the endpoint receives no messages.
registerAppInstanceUserEndpoint_allowMessages :: Lens.Lens' RegisterAppInstanceUserEndpoint (Prelude.Maybe AllowMessages)
registerAppInstanceUserEndpoint_allowMessages = Lens.lens (\RegisterAppInstanceUserEndpoint' {allowMessages} -> allowMessages) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {allowMessages = a} :: RegisterAppInstanceUserEndpoint)

-- | The name of the @AppInstanceUserEndpoint@.
registerAppInstanceUserEndpoint_name :: Lens.Lens' RegisterAppInstanceUserEndpoint (Prelude.Maybe Prelude.Text)
registerAppInstanceUserEndpoint_name = Lens.lens (\RegisterAppInstanceUserEndpoint' {name} -> name) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {name = a} :: RegisterAppInstanceUserEndpoint) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the @AppInstanceUser@.
registerAppInstanceUserEndpoint_appInstanceUserArn :: Lens.Lens' RegisterAppInstanceUserEndpoint Prelude.Text
registerAppInstanceUserEndpoint_appInstanceUserArn = Lens.lens (\RegisterAppInstanceUserEndpoint' {appInstanceUserArn} -> appInstanceUserArn) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {appInstanceUserArn = a} :: RegisterAppInstanceUserEndpoint) Prelude.. Data._Sensitive

-- | The type of the @AppInstanceUserEndpoint@. Supported types:
--
-- -   @APNS@: The mobile notification service for an Apple device.
--
-- -   @APNS_SANDBOX@: The sandbox environment of the mobile notification
--     service for an Apple device.
--
-- -   @GCM@: The mobile notification service for an Android device.
--
-- Populate the @ResourceArn@ value of each type as @PinpointAppArn@.
registerAppInstanceUserEndpoint_type :: Lens.Lens' RegisterAppInstanceUserEndpoint AppInstanceUserEndpointType
registerAppInstanceUserEndpoint_type = Lens.lens (\RegisterAppInstanceUserEndpoint' {type'} -> type') (\s@RegisterAppInstanceUserEndpoint' {} a -> s {type' = a} :: RegisterAppInstanceUserEndpoint)

-- | The ARN of the resource to which the endpoint belongs.
registerAppInstanceUserEndpoint_resourceArn :: Lens.Lens' RegisterAppInstanceUserEndpoint Prelude.Text
registerAppInstanceUserEndpoint_resourceArn = Lens.lens (\RegisterAppInstanceUserEndpoint' {resourceArn} -> resourceArn) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {resourceArn = a} :: RegisterAppInstanceUserEndpoint)

-- | The attributes of an @Endpoint@.
registerAppInstanceUserEndpoint_endpointAttributes :: Lens.Lens' RegisterAppInstanceUserEndpoint EndpointAttributes
registerAppInstanceUserEndpoint_endpointAttributes = Lens.lens (\RegisterAppInstanceUserEndpoint' {endpointAttributes} -> endpointAttributes) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {endpointAttributes = a} :: RegisterAppInstanceUserEndpoint)

-- | The unique ID assigned to the request. Use different tokens to register
-- other endpoints.
registerAppInstanceUserEndpoint_clientRequestToken :: Lens.Lens' RegisterAppInstanceUserEndpoint Prelude.Text
registerAppInstanceUserEndpoint_clientRequestToken = Lens.lens (\RegisterAppInstanceUserEndpoint' {clientRequestToken} -> clientRequestToken) (\s@RegisterAppInstanceUserEndpoint' {} a -> s {clientRequestToken = a} :: RegisterAppInstanceUserEndpoint)

instance
  Core.AWSRequest
    RegisterAppInstanceUserEndpoint
  where
  type
    AWSResponse RegisterAppInstanceUserEndpoint =
      RegisterAppInstanceUserEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterAppInstanceUserEndpointResponse'
            Prelude.<$> (x Data..?> "AppInstanceUserArn")
            Prelude.<*> (x Data..?> "EndpointId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterAppInstanceUserEndpoint
  where
  hashWithSalt
    _salt
    RegisterAppInstanceUserEndpoint' {..} =
      _salt
        `Prelude.hashWithSalt` allowMessages
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` appInstanceUserArn
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` endpointAttributes
        `Prelude.hashWithSalt` clientRequestToken

instance
  Prelude.NFData
    RegisterAppInstanceUserEndpoint
  where
  rnf RegisterAppInstanceUserEndpoint' {..} =
    Prelude.rnf allowMessages
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf endpointAttributes
      `Prelude.seq` Prelude.rnf clientRequestToken

instance
  Data.ToHeaders
    RegisterAppInstanceUserEndpoint
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RegisterAppInstanceUserEndpoint where
  toJSON RegisterAppInstanceUserEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowMessages" Data..=) Prelude.<$> allowMessages,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just
              ("EndpointAttributes" Data..= endpointAttributes),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath RegisterAppInstanceUserEndpoint where
  toPath RegisterAppInstanceUserEndpoint' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn,
        "/endpoints"
      ]

instance Data.ToQuery RegisterAppInstanceUserEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterAppInstanceUserEndpointResponse' smart constructor.
data RegisterAppInstanceUserEndpointResponse = RegisterAppInstanceUserEndpointResponse'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAppInstanceUserEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'registerAppInstanceUserEndpointResponse_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'registerAppInstanceUserEndpointResponse_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
--
-- 'httpStatus', 'registerAppInstanceUserEndpointResponse_httpStatus' - The response's http status code.
newRegisterAppInstanceUserEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterAppInstanceUserEndpointResponse
newRegisterAppInstanceUserEndpointResponse
  pHttpStatus_ =
    RegisterAppInstanceUserEndpointResponse'
      { appInstanceUserArn =
          Prelude.Nothing,
        endpointId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the @AppInstanceUser@.
registerAppInstanceUserEndpointResponse_appInstanceUserArn :: Lens.Lens' RegisterAppInstanceUserEndpointResponse (Prelude.Maybe Prelude.Text)
registerAppInstanceUserEndpointResponse_appInstanceUserArn = Lens.lens (\RegisterAppInstanceUserEndpointResponse' {appInstanceUserArn} -> appInstanceUserArn) (\s@RegisterAppInstanceUserEndpointResponse' {} a -> s {appInstanceUserArn = a} :: RegisterAppInstanceUserEndpointResponse)

-- | The unique identifier of the @AppInstanceUserEndpoint@.
registerAppInstanceUserEndpointResponse_endpointId :: Lens.Lens' RegisterAppInstanceUserEndpointResponse (Prelude.Maybe Prelude.Text)
registerAppInstanceUserEndpointResponse_endpointId = Lens.lens (\RegisterAppInstanceUserEndpointResponse' {endpointId} -> endpointId) (\s@RegisterAppInstanceUserEndpointResponse' {} a -> s {endpointId = a} :: RegisterAppInstanceUserEndpointResponse)

-- | The response's http status code.
registerAppInstanceUserEndpointResponse_httpStatus :: Lens.Lens' RegisterAppInstanceUserEndpointResponse Prelude.Int
registerAppInstanceUserEndpointResponse_httpStatus = Lens.lens (\RegisterAppInstanceUserEndpointResponse' {httpStatus} -> httpStatus) (\s@RegisterAppInstanceUserEndpointResponse' {} a -> s {httpStatus = a} :: RegisterAppInstanceUserEndpointResponse)

instance
  Prelude.NFData
    RegisterAppInstanceUserEndpointResponse
  where
  rnf RegisterAppInstanceUserEndpointResponse' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf httpStatus
