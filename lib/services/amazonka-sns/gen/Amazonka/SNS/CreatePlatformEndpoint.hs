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
-- Module      : Amazonka.SNS.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a device and mobile app on one of the supported
-- push notification services, such as GCM (Firebase Cloud Messaging) and
-- APNS. @CreatePlatformEndpoint@ requires the @PlatformApplicationArn@
-- that is returned from @CreatePlatformApplication@. You can use the
-- returned @EndpointArn@ to send a message to a mobile app or by the
-- @Subscribe@ action for subscription to a topic. The
-- @CreatePlatformEndpoint@ action is idempotent, so if the requester
-- already owns an endpoint with the same device token and attributes, that
-- endpoint\'s ARN is returned without creating a new endpoint. For more
-- information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- When using @CreatePlatformEndpoint@ with Baidu, two attributes must be
-- provided: ChannelId and UserId. The token field must also contain the
-- ChannelId. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePushBaiduEndpoint.html Creating an Amazon SNS Endpoint for Baidu>.
module Amazonka.SNS.CreatePlatformEndpoint
  ( -- * Creating a Request
    CreatePlatformEndpoint (..),
    newCreatePlatformEndpoint,

    -- * Request Lenses
    createPlatformEndpoint_attributes,
    createPlatformEndpoint_customUserData,
    createPlatformEndpoint_platformApplicationArn,
    createPlatformEndpoint_token,

    -- * Destructuring the Response
    CreatePlatformEndpointResponse (..),
    newCreatePlatformEndpointResponse,

    -- * Response Lenses
    createPlatformEndpointResponse_endpointArn,
    createPlatformEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for CreatePlatformEndpoint action.
--
-- /See:/ 'newCreatePlatformEndpoint' smart constructor.
data CreatePlatformEndpoint = CreatePlatformEndpoint'
  { -- | For a list of attributes, see
    -- <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes>.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Arbitrary user data to associate with the endpoint. Amazon SNS does not
    -- use this data. The data must be in UTF-8 format and less than 2KB.
    customUserData :: Prelude.Maybe Prelude.Text,
    -- | PlatformApplicationArn returned from CreatePlatformApplication is used
    -- to create a an endpoint.
    platformApplicationArn :: Prelude.Text,
    -- | Unique identifier created by the notification service for an app on a
    -- device. The specific name for Token will vary, depending on which
    -- notification service is being used. For example, when using APNS as the
    -- notification service, you need the device token. Alternatively, when
    -- using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent
    -- is called the registration ID.
    token :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createPlatformEndpoint_attributes' - For a list of attributes, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes>.
--
-- 'customUserData', 'createPlatformEndpoint_customUserData' - Arbitrary user data to associate with the endpoint. Amazon SNS does not
-- use this data. The data must be in UTF-8 format and less than 2KB.
--
-- 'platformApplicationArn', 'createPlatformEndpoint_platformApplicationArn' - PlatformApplicationArn returned from CreatePlatformApplication is used
-- to create a an endpoint.
--
-- 'token', 'createPlatformEndpoint_token' - Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when
-- using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent
-- is called the registration ID.
newCreatePlatformEndpoint ::
  -- | 'platformApplicationArn'
  Prelude.Text ->
  -- | 'token'
  Prelude.Text ->
  CreatePlatformEndpoint
newCreatePlatformEndpoint
  pPlatformApplicationArn_
  pToken_ =
    CreatePlatformEndpoint'
      { attributes =
          Prelude.Nothing,
        customUserData = Prelude.Nothing,
        platformApplicationArn = pPlatformApplicationArn_,
        token = pToken_
      }

-- | For a list of attributes, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes>.
createPlatformEndpoint_attributes :: Lens.Lens' CreatePlatformEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPlatformEndpoint_attributes = Lens.lens (\CreatePlatformEndpoint' {attributes} -> attributes) (\s@CreatePlatformEndpoint' {} a -> s {attributes = a} :: CreatePlatformEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not
-- use this data. The data must be in UTF-8 format and less than 2KB.
createPlatformEndpoint_customUserData :: Lens.Lens' CreatePlatformEndpoint (Prelude.Maybe Prelude.Text)
createPlatformEndpoint_customUserData = Lens.lens (\CreatePlatformEndpoint' {customUserData} -> customUserData) (\s@CreatePlatformEndpoint' {} a -> s {customUserData = a} :: CreatePlatformEndpoint)

-- | PlatformApplicationArn returned from CreatePlatformApplication is used
-- to create a an endpoint.
createPlatformEndpoint_platformApplicationArn :: Lens.Lens' CreatePlatformEndpoint Prelude.Text
createPlatformEndpoint_platformApplicationArn = Lens.lens (\CreatePlatformEndpoint' {platformApplicationArn} -> platformApplicationArn) (\s@CreatePlatformEndpoint' {} a -> s {platformApplicationArn = a} :: CreatePlatformEndpoint)

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when
-- using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent
-- is called the registration ID.
createPlatformEndpoint_token :: Lens.Lens' CreatePlatformEndpoint Prelude.Text
createPlatformEndpoint_token = Lens.lens (\CreatePlatformEndpoint' {token} -> token) (\s@CreatePlatformEndpoint' {} a -> s {token = a} :: CreatePlatformEndpoint)

instance Core.AWSRequest CreatePlatformEndpoint where
  type
    AWSResponse CreatePlatformEndpoint =
      CreatePlatformEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreatePlatformEndpointResult"
      ( \s h x ->
          CreatePlatformEndpointResponse'
            Prelude.<$> (x Data..@? "EndpointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlatformEndpoint where
  hashWithSalt _salt CreatePlatformEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` customUserData
      `Prelude.hashWithSalt` platformApplicationArn
      `Prelude.hashWithSalt` token

instance Prelude.NFData CreatePlatformEndpoint where
  rnf CreatePlatformEndpoint' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf customUserData `Prelude.seq`
        Prelude.rnf platformApplicationArn `Prelude.seq`
          Prelude.rnf token

instance Data.ToHeaders CreatePlatformEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePlatformEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePlatformEndpoint where
  toQuery CreatePlatformEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePlatformEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "Attributes"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> attributes
            ),
        "CustomUserData" Data.=: customUserData,
        "PlatformApplicationArn"
          Data.=: platformApplicationArn,
        "Token" Data.=: token
      ]

-- | Response from CreateEndpoint action.
--
-- /See:/ 'newCreatePlatformEndpointResponse' smart constructor.
data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse'
  { -- | EndpointArn returned from CreateEndpoint action.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'createPlatformEndpointResponse_endpointArn' - EndpointArn returned from CreateEndpoint action.
--
-- 'httpStatus', 'createPlatformEndpointResponse_httpStatus' - The response's http status code.
newCreatePlatformEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePlatformEndpointResponse
newCreatePlatformEndpointResponse pHttpStatus_ =
  CreatePlatformEndpointResponse'
    { endpointArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | EndpointArn returned from CreateEndpoint action.
createPlatformEndpointResponse_endpointArn :: Lens.Lens' CreatePlatformEndpointResponse (Prelude.Maybe Prelude.Text)
createPlatformEndpointResponse_endpointArn = Lens.lens (\CreatePlatformEndpointResponse' {endpointArn} -> endpointArn) (\s@CreatePlatformEndpointResponse' {} a -> s {endpointArn = a} :: CreatePlatformEndpointResponse)

-- | The response's http status code.
createPlatformEndpointResponse_httpStatus :: Lens.Lens' CreatePlatformEndpointResponse Prelude.Int
createPlatformEndpointResponse_httpStatus = Lens.lens (\CreatePlatformEndpointResponse' {httpStatus} -> httpStatus) (\s@CreatePlatformEndpointResponse' {} a -> s {httpStatus = a} :: CreatePlatformEndpointResponse)

instance
  Prelude.NFData
    CreatePlatformEndpointResponse
  where
  rnf CreatePlatformEndpointResponse' {..} =
    Prelude.rnf endpointArn `Prelude.seq`
      Prelude.rnf httpStatus
