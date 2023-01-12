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
-- Module      : Amazonka.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes for an endpoint for a device on one of the supported
-- push notification services, such as GCM (Firebase Cloud Messaging) and
-- APNS. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
module Amazonka.SNS.SetEndpointAttributes
  ( -- * Creating a Request
    SetEndpointAttributes (..),
    newSetEndpointAttributes,

    -- * Request Lenses
    setEndpointAttributes_endpointArn,
    setEndpointAttributes_attributes,

    -- * Destructuring the Response
    SetEndpointAttributesResponse (..),
    newSetEndpointAttributesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for SetEndpointAttributes action.
--
-- /See:/ 'newSetEndpointAttributes' smart constructor.
data SetEndpointAttributes = SetEndpointAttributes'
  { -- | EndpointArn used for SetEndpointAttributes action.
    endpointArn :: Prelude.Text,
    -- | A map of the endpoint attributes. Attributes in this map include the
    -- following:
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
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetEndpointAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'setEndpointAttributes_endpointArn' - EndpointArn used for SetEndpointAttributes action.
--
-- 'attributes', 'setEndpointAttributes_attributes' - A map of the endpoint attributes. Attributes in this map include the
-- following:
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
newSetEndpointAttributes ::
  -- | 'endpointArn'
  Prelude.Text ->
  SetEndpointAttributes
newSetEndpointAttributes pEndpointArn_ =
  SetEndpointAttributes'
    { endpointArn = pEndpointArn_,
      attributes = Prelude.mempty
    }

-- | EndpointArn used for SetEndpointAttributes action.
setEndpointAttributes_endpointArn :: Lens.Lens' SetEndpointAttributes Prelude.Text
setEndpointAttributes_endpointArn = Lens.lens (\SetEndpointAttributes' {endpointArn} -> endpointArn) (\s@SetEndpointAttributes' {} a -> s {endpointArn = a} :: SetEndpointAttributes)

-- | A map of the endpoint attributes. Attributes in this map include the
-- following:
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
setEndpointAttributes_attributes :: Lens.Lens' SetEndpointAttributes (Prelude.HashMap Prelude.Text Prelude.Text)
setEndpointAttributes_attributes = Lens.lens (\SetEndpointAttributes' {attributes} -> attributes) (\s@SetEndpointAttributes' {} a -> s {attributes = a} :: SetEndpointAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest SetEndpointAttributes where
  type
    AWSResponse SetEndpointAttributes =
      SetEndpointAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull SetEndpointAttributesResponse'

instance Prelude.Hashable SetEndpointAttributes where
  hashWithSalt _salt SetEndpointAttributes' {..} =
    _salt `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData SetEndpointAttributes where
  rnf SetEndpointAttributes' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders SetEndpointAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetEndpointAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery SetEndpointAttributes where
  toQuery SetEndpointAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetEndpointAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "EndpointArn" Data.=: endpointArn,
        "Attributes"
          Data.=: Data.toQueryMap "entry" "key" "value" attributes
      ]

-- | /See:/ 'newSetEndpointAttributesResponse' smart constructor.
data SetEndpointAttributesResponse = SetEndpointAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetEndpointAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetEndpointAttributesResponse ::
  SetEndpointAttributesResponse
newSetEndpointAttributesResponse =
  SetEndpointAttributesResponse'

instance Prelude.NFData SetEndpointAttributesResponse where
  rnf _ = ()
