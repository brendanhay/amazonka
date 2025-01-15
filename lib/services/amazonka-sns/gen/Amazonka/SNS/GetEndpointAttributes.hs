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
-- Module      : Amazonka.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the endpoint attributes for a device on one of the supported
-- push notification services, such as GCM (Firebase Cloud Messaging) and
-- APNS. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
module Amazonka.SNS.GetEndpointAttributes
  ( -- * Creating a Request
    GetEndpointAttributes (..),
    newGetEndpointAttributes,

    -- * Request Lenses
    getEndpointAttributes_endpointArn,

    -- * Destructuring the Response
    GetEndpointAttributesResponse (..),
    newGetEndpointAttributesResponse,

    -- * Response Lenses
    getEndpointAttributesResponse_attributes,
    getEndpointAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for GetEndpointAttributes action.
--
-- /See:/ 'newGetEndpointAttributes' smart constructor.
data GetEndpointAttributes = GetEndpointAttributes'
  { -- | EndpointArn for GetEndpointAttributes input.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'getEndpointAttributes_endpointArn' - EndpointArn for GetEndpointAttributes input.
newGetEndpointAttributes ::
  -- | 'endpointArn'
  Prelude.Text ->
  GetEndpointAttributes
newGetEndpointAttributes pEndpointArn_ =
  GetEndpointAttributes' {endpointArn = pEndpointArn_}

-- | EndpointArn for GetEndpointAttributes input.
getEndpointAttributes_endpointArn :: Lens.Lens' GetEndpointAttributes Prelude.Text
getEndpointAttributes_endpointArn = Lens.lens (\GetEndpointAttributes' {endpointArn} -> endpointArn) (\s@GetEndpointAttributes' {} a -> s {endpointArn = a} :: GetEndpointAttributes)

instance Core.AWSRequest GetEndpointAttributes where
  type
    AWSResponse GetEndpointAttributes =
      GetEndpointAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetEndpointAttributesResult"
      ( \s h x ->
          GetEndpointAttributesResponse'
            Prelude.<$> ( x Data..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEndpointAttributes where
  hashWithSalt _salt GetEndpointAttributes' {..} =
    _salt `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData GetEndpointAttributes where
  rnf GetEndpointAttributes' {..} =
    Prelude.rnf endpointArn

instance Data.ToHeaders GetEndpointAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetEndpointAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEndpointAttributes where
  toQuery GetEndpointAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetEndpointAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "EndpointArn" Data.=: endpointArn
      ]

-- | Response from GetEndpointAttributes of the EndpointArn.
--
-- /See:/ 'newGetEndpointAttributesResponse' smart constructor.
data GetEndpointAttributesResponse = GetEndpointAttributesResponse'
  { -- | Attributes include the following:
    --
    -- -   @CustomUserData@ – arbitrary user data to associate with the
    --     endpoint. Amazon SNS does not use this data. The data must be in
    --     UTF-8 format and less than 2KB.
    --
    -- -   @Enabled@ – flag that enables\/disables delivery to the endpoint.
    --     Amazon SNS will set this to false when a notification service
    --     indicates to Amazon SNS that the endpoint is invalid. Users can set
    --     it back to true, typically after updating Token.
    --
    -- -   @Token@ – device token, also referred to as a registration id, for
    --     an app and mobile device. This is returned from the notification
    --     service when an app and mobile device are registered with the
    --     notification service.
    --
    --     The device token for the iOS platform is returned in lowercase.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getEndpointAttributesResponse_attributes' - Attributes include the following:
--
-- -   @CustomUserData@ – arbitrary user data to associate with the
--     endpoint. Amazon SNS does not use this data. The data must be in
--     UTF-8 format and less than 2KB.
--
-- -   @Enabled@ – flag that enables\/disables delivery to the endpoint.
--     Amazon SNS will set this to false when a notification service
--     indicates to Amazon SNS that the endpoint is invalid. Users can set
--     it back to true, typically after updating Token.
--
-- -   @Token@ – device token, also referred to as a registration id, for
--     an app and mobile device. This is returned from the notification
--     service when an app and mobile device are registered with the
--     notification service.
--
--     The device token for the iOS platform is returned in lowercase.
--
-- 'httpStatus', 'getEndpointAttributesResponse_httpStatus' - The response's http status code.
newGetEndpointAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEndpointAttributesResponse
newGetEndpointAttributesResponse pHttpStatus_ =
  GetEndpointAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Attributes include the following:
--
-- -   @CustomUserData@ – arbitrary user data to associate with the
--     endpoint. Amazon SNS does not use this data. The data must be in
--     UTF-8 format and less than 2KB.
--
-- -   @Enabled@ – flag that enables\/disables delivery to the endpoint.
--     Amazon SNS will set this to false when a notification service
--     indicates to Amazon SNS that the endpoint is invalid. Users can set
--     it back to true, typically after updating Token.
--
-- -   @Token@ – device token, also referred to as a registration id, for
--     an app and mobile device. This is returned from the notification
--     service when an app and mobile device are registered with the
--     notification service.
--
--     The device token for the iOS platform is returned in lowercase.
getEndpointAttributesResponse_attributes :: Lens.Lens' GetEndpointAttributesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEndpointAttributesResponse_attributes = Lens.lens (\GetEndpointAttributesResponse' {attributes} -> attributes) (\s@GetEndpointAttributesResponse' {} a -> s {attributes = a} :: GetEndpointAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEndpointAttributesResponse_httpStatus :: Lens.Lens' GetEndpointAttributesResponse Prelude.Int
getEndpointAttributesResponse_httpStatus = Lens.lens (\GetEndpointAttributesResponse' {httpStatus} -> httpStatus) (\s@GetEndpointAttributesResponse' {} a -> s {httpStatus = a} :: GetEndpointAttributesResponse)

instance Prelude.NFData GetEndpointAttributesResponse where
  rnf GetEndpointAttributesResponse' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf httpStatus
