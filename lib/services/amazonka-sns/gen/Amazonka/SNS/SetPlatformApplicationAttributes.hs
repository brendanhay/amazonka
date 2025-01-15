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
-- Module      : Amazonka.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes of the platform application object for the supported
-- push notification services, such as APNS and GCM (Firebase Cloud
-- Messaging). For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
-- For information on configuring attributes for message delivery status,
-- see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status>.
module Amazonka.SNS.SetPlatformApplicationAttributes
  ( -- * Creating a Request
    SetPlatformApplicationAttributes (..),
    newSetPlatformApplicationAttributes,

    -- * Request Lenses
    setPlatformApplicationAttributes_platformApplicationArn,
    setPlatformApplicationAttributes_attributes,

    -- * Destructuring the Response
    SetPlatformApplicationAttributesResponse (..),
    newSetPlatformApplicationAttributesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for SetPlatformApplicationAttributes action.
--
-- /See:/ 'newSetPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { -- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
    platformApplicationArn :: Prelude.Text,
    -- | A map of the platform application attributes. Attributes in this map
    -- include the following:
    --
    -- -   @PlatformCredential@ – The credential received from the notification
    --     service.
    --
    --     -   For ADM, @PlatformCredential@is client secret.
    --
    --     -   For Apple Services using certificate credentials,
    --         @PlatformCredential@ is private key.
    --
    --     -   For Apple Services using token credentials, @PlatformCredential@
    --         is signing key.
    --
    --     -   For GCM (Firebase Cloud Messaging), @PlatformCredential@ is API
    --         key.
    --
    -- -   @PlatformPrincipal@ – The principal received from the notification
    --     service.
    --
    --     -   For ADM, @PlatformPrincipal@is client id.
    --
    --     -   For Apple Services using certificate credentials,
    --         @PlatformPrincipal@ is SSL certificate.
    --
    --     -   For Apple Services using token credentials, @PlatformPrincipal@
    --         is signing key ID.
    --
    --     -   For GCM (Firebase Cloud Messaging), there is no
    --         @PlatformPrincipal@.
    --
    -- -   @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event
    --     notifications are sent.
    --
    -- -   @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event
    --     notifications are sent.
    --
    -- -   @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event
    --     notifications are sent.
    --
    -- -   @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event
    --     notifications are sent upon Direct Publish delivery failure
    --     (permanent) to one of the application\'s endpoints.
    --
    -- -   @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
    --     write access to use CloudWatch Logs on your behalf.
    --
    -- -   @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
    --     write access to use CloudWatch Logs on your behalf.
    --
    -- -   @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of
    --     successfully delivered messages.
    --
    -- The following attributes only apply to @APNs@ token-based
    -- authentication:
    --
    -- -   @ApplePlatformTeamID@ – The identifier that\'s assigned to your
    --     Apple developer account team.
    --
    -- -   @ApplePlatformBundleID@ – The bundle identifier that\'s assigned to
    --     your iOS app.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetPlatformApplicationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformApplicationArn', 'setPlatformApplicationAttributes_platformApplicationArn' - PlatformApplicationArn for SetPlatformApplicationAttributes action.
--
-- 'attributes', 'setPlatformApplicationAttributes_attributes' - A map of the platform application attributes. Attributes in this map
-- include the following:
--
-- -   @PlatformCredential@ – The credential received from the notification
--     service.
--
--     -   For ADM, @PlatformCredential@is client secret.
--
--     -   For Apple Services using certificate credentials,
--         @PlatformCredential@ is private key.
--
--     -   For Apple Services using token credentials, @PlatformCredential@
--         is signing key.
--
--     -   For GCM (Firebase Cloud Messaging), @PlatformCredential@ is API
--         key.
--
-- -   @PlatformPrincipal@ – The principal received from the notification
--     service.
--
--     -   For ADM, @PlatformPrincipal@is client id.
--
--     -   For Apple Services using certificate credentials,
--         @PlatformPrincipal@ is SSL certificate.
--
--     -   For Apple Services using token credentials, @PlatformPrincipal@
--         is signing key ID.
--
--     -   For GCM (Firebase Cloud Messaging), there is no
--         @PlatformPrincipal@.
--
-- -   @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event
--     notifications are sent.
--
-- -   @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event
--     notifications are sent.
--
-- -   @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event
--     notifications are sent.
--
-- -   @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event
--     notifications are sent upon Direct Publish delivery failure
--     (permanent) to one of the application\'s endpoints.
--
-- -   @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
--     write access to use CloudWatch Logs on your behalf.
--
-- -   @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
--     write access to use CloudWatch Logs on your behalf.
--
-- -   @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of
--     successfully delivered messages.
--
-- The following attributes only apply to @APNs@ token-based
-- authentication:
--
-- -   @ApplePlatformTeamID@ – The identifier that\'s assigned to your
--     Apple developer account team.
--
-- -   @ApplePlatformBundleID@ – The bundle identifier that\'s assigned to
--     your iOS app.
newSetPlatformApplicationAttributes ::
  -- | 'platformApplicationArn'
  Prelude.Text ->
  SetPlatformApplicationAttributes
newSetPlatformApplicationAttributes
  pPlatformApplicationArn_ =
    SetPlatformApplicationAttributes'
      { platformApplicationArn =
          pPlatformApplicationArn_,
        attributes = Prelude.mempty
      }

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
setPlatformApplicationAttributes_platformApplicationArn :: Lens.Lens' SetPlatformApplicationAttributes Prelude.Text
setPlatformApplicationAttributes_platformApplicationArn = Lens.lens (\SetPlatformApplicationAttributes' {platformApplicationArn} -> platformApplicationArn) (\s@SetPlatformApplicationAttributes' {} a -> s {platformApplicationArn = a} :: SetPlatformApplicationAttributes)

-- | A map of the platform application attributes. Attributes in this map
-- include the following:
--
-- -   @PlatformCredential@ – The credential received from the notification
--     service.
--
--     -   For ADM, @PlatformCredential@is client secret.
--
--     -   For Apple Services using certificate credentials,
--         @PlatformCredential@ is private key.
--
--     -   For Apple Services using token credentials, @PlatformCredential@
--         is signing key.
--
--     -   For GCM (Firebase Cloud Messaging), @PlatformCredential@ is API
--         key.
--
-- -   @PlatformPrincipal@ – The principal received from the notification
--     service.
--
--     -   For ADM, @PlatformPrincipal@is client id.
--
--     -   For Apple Services using certificate credentials,
--         @PlatformPrincipal@ is SSL certificate.
--
--     -   For Apple Services using token credentials, @PlatformPrincipal@
--         is signing key ID.
--
--     -   For GCM (Firebase Cloud Messaging), there is no
--         @PlatformPrincipal@.
--
-- -   @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event
--     notifications are sent.
--
-- -   @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event
--     notifications are sent.
--
-- -   @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event
--     notifications are sent.
--
-- -   @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event
--     notifications are sent upon Direct Publish delivery failure
--     (permanent) to one of the application\'s endpoints.
--
-- -   @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
--     write access to use CloudWatch Logs on your behalf.
--
-- -   @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS
--     write access to use CloudWatch Logs on your behalf.
--
-- -   @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of
--     successfully delivered messages.
--
-- The following attributes only apply to @APNs@ token-based
-- authentication:
--
-- -   @ApplePlatformTeamID@ – The identifier that\'s assigned to your
--     Apple developer account team.
--
-- -   @ApplePlatformBundleID@ – The bundle identifier that\'s assigned to
--     your iOS app.
setPlatformApplicationAttributes_attributes :: Lens.Lens' SetPlatformApplicationAttributes (Prelude.HashMap Prelude.Text Prelude.Text)
setPlatformApplicationAttributes_attributes = Lens.lens (\SetPlatformApplicationAttributes' {attributes} -> attributes) (\s@SetPlatformApplicationAttributes' {} a -> s {attributes = a} :: SetPlatformApplicationAttributes) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    SetPlatformApplicationAttributes
  where
  type
    AWSResponse SetPlatformApplicationAttributes =
      SetPlatformApplicationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      SetPlatformApplicationAttributesResponse'

instance
  Prelude.Hashable
    SetPlatformApplicationAttributes
  where
  hashWithSalt
    _salt
    SetPlatformApplicationAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` platformApplicationArn
        `Prelude.hashWithSalt` attributes

instance
  Prelude.NFData
    SetPlatformApplicationAttributes
  where
  rnf SetPlatformApplicationAttributes' {..} =
    Prelude.rnf platformApplicationArn `Prelude.seq`
      Prelude.rnf attributes

instance
  Data.ToHeaders
    SetPlatformApplicationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetPlatformApplicationAttributes where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SetPlatformApplicationAttributes
  where
  toQuery SetPlatformApplicationAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "SetPlatformApplicationAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "PlatformApplicationArn"
          Data.=: platformApplicationArn,
        "Attributes"
          Data.=: Data.toQueryMap "entry" "key" "value" attributes
      ]

-- | /See:/ 'newSetPlatformApplicationAttributesResponse' smart constructor.
data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetPlatformApplicationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetPlatformApplicationAttributesResponse ::
  SetPlatformApplicationAttributesResponse
newSetPlatformApplicationAttributesResponse =
  SetPlatformApplicationAttributesResponse'

instance
  Prelude.NFData
    SetPlatformApplicationAttributesResponse
  where
  rnf _ = ()
