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
-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.SNS.SetPlatformApplicationAttributes
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for SetPlatformApplicationAttributes action.
--
-- /See:/ 'newSetPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { -- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
    platformApplicationArn :: Core.Text,
    -- | A map of the platform application attributes. Attributes in this map
    -- include the following:
    --
    -- -   @PlatformCredential@ – The credential received from the notification
    --     service. For @APNS@ and @APNS_SANDBOX@, @PlatformCredential@ is
    --     @private key@. For @GCM@ (Firebase Cloud Messaging),
    --     @PlatformCredential@ is @API key@. For @ADM@, @PlatformCredential@
    --     is @client secret@.
    --
    -- -   @PlatformPrincipal@ – The principal received from the notification
    --     service. For @APNS@ and @APNS_SANDBOX@, @PlatformPrincipal@ is
    --     @SSL certificate@. For @GCM@ (Firebase Cloud Messaging), there is no
    --     @PlatformPrincipal@. For @ADM@, @PlatformPrincipal@ is @client id@.
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
    attributes :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
--     service. For @APNS@ and @APNS_SANDBOX@, @PlatformCredential@ is
--     @private key@. For @GCM@ (Firebase Cloud Messaging),
--     @PlatformCredential@ is @API key@. For @ADM@, @PlatformCredential@
--     is @client secret@.
--
-- -   @PlatformPrincipal@ – The principal received from the notification
--     service. For @APNS@ and @APNS_SANDBOX@, @PlatformPrincipal@ is
--     @SSL certificate@. For @GCM@ (Firebase Cloud Messaging), there is no
--     @PlatformPrincipal@. For @ADM@, @PlatformPrincipal@ is @client id@.
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
newSetPlatformApplicationAttributes ::
  -- | 'platformApplicationArn'
  Core.Text ->
  SetPlatformApplicationAttributes
newSetPlatformApplicationAttributes
  pPlatformApplicationArn_ =
    SetPlatformApplicationAttributes'
      { platformApplicationArn =
          pPlatformApplicationArn_,
        attributes = Core.mempty
      }

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
setPlatformApplicationAttributes_platformApplicationArn :: Lens.Lens' SetPlatformApplicationAttributes Core.Text
setPlatformApplicationAttributes_platformApplicationArn = Lens.lens (\SetPlatformApplicationAttributes' {platformApplicationArn} -> platformApplicationArn) (\s@SetPlatformApplicationAttributes' {} a -> s {platformApplicationArn = a} :: SetPlatformApplicationAttributes)

-- | A map of the platform application attributes. Attributes in this map
-- include the following:
--
-- -   @PlatformCredential@ – The credential received from the notification
--     service. For @APNS@ and @APNS_SANDBOX@, @PlatformCredential@ is
--     @private key@. For @GCM@ (Firebase Cloud Messaging),
--     @PlatformCredential@ is @API key@. For @ADM@, @PlatformCredential@
--     is @client secret@.
--
-- -   @PlatformPrincipal@ – The principal received from the notification
--     service. For @APNS@ and @APNS_SANDBOX@, @PlatformPrincipal@ is
--     @SSL certificate@. For @GCM@ (Firebase Cloud Messaging), there is no
--     @PlatformPrincipal@. For @ADM@, @PlatformPrincipal@ is @client id@.
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
setPlatformApplicationAttributes_attributes :: Lens.Lens' SetPlatformApplicationAttributes (Core.HashMap Core.Text Core.Text)
setPlatformApplicationAttributes_attributes = Lens.lens (\SetPlatformApplicationAttributes' {attributes} -> attributes) (\s@SetPlatformApplicationAttributes' {} a -> s {attributes = a} :: SetPlatformApplicationAttributes) Core.. Lens._Coerce

instance
  Core.AWSRequest
    SetPlatformApplicationAttributes
  where
  type
    AWSResponse SetPlatformApplicationAttributes =
      SetPlatformApplicationAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      SetPlatformApplicationAttributesResponse'

instance
  Core.Hashable
    SetPlatformApplicationAttributes

instance Core.NFData SetPlatformApplicationAttributes

instance
  Core.ToHeaders
    SetPlatformApplicationAttributes
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetPlatformApplicationAttributes where
  toPath = Core.const "/"

instance
  Core.ToQuery
    SetPlatformApplicationAttributes
  where
  toQuery SetPlatformApplicationAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "SetPlatformApplicationAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "PlatformApplicationArn"
          Core.=: platformApplicationArn,
        "Attributes"
          Core.=: Core.toQueryMap "entry" "key" "value" attributes
      ]

-- | /See:/ 'newSetPlatformApplicationAttributesResponse' smart constructor.
data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetPlatformApplicationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetPlatformApplicationAttributesResponse ::
  SetPlatformApplicationAttributesResponse
newSetPlatformApplicationAttributesResponse =
  SetPlatformApplicationAttributesResponse'

instance
  Core.NFData
    SetPlatformApplicationAttributesResponse
