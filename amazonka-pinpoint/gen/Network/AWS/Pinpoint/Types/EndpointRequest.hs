{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the channel type and other settings for an endpoint.
--
-- /See:/ 'newEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { -- | One or more custom attributes that describe the user who\'s associated
    -- with the endpoint.
    user :: Prelude.Maybe EndpointUser,
    -- | The destination address for messages or push notifications that you send
    -- to the endpoint. The address varies by channel. For a push-notification
    -- channel, use the token provided by the push notification service, such
    -- as an Apple Push Notification service (APNs) device token or a Firebase
    -- Cloud Messaging (FCM) registration token. For the SMS channel, use a
    -- phone number in E.164 format, such as +12065550100. For the email
    -- channel, use an email address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The channel to use when sending messages or push notifications to the
    -- endpoint.
    channelType :: Prelude.Maybe ChannelType,
    -- | Specifies whether the user who\'s associated with the endpoint has opted
    -- out of receiving messages and push notifications from you. Possible
    -- values are: ALL, the user has opted out and doesn\'t want to receive any
    -- messages or push notifications; and, NONE, the user hasn\'t opted out
    -- and wants to receive all messages and push notifications.
    optOut :: Prelude.Maybe Prelude.Text,
    -- | The demographic information for the endpoint, such as the time zone and
    -- platform.
    demographic :: Prelude.Maybe EndpointDemographic,
    -- | One or more custom attributes that describe the endpoint by associating
    -- a name with an array of values. For example, the value of a custom
    -- attribute named Interests might be: [\"Science\", \"Music\",
    -- \"Travel\"]. You can use these attributes as filter criteria when you
    -- create segments. Attribute names are case sensitive.
    --
    -- An attribute name can contain up to 50 characters. An attribute value
    -- can contain up to 100 characters. When you define the name of a custom
    -- attribute, avoid using the following characters: number sign (#), colon
    -- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
    -- Pinpoint console can\'t display attribute names that contain these
    -- characters. This restriction doesn\'t apply to attribute values.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | Specifies whether to send messages or push notifications to the
    -- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
    -- and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create
    -- an endpoint or update an existing endpoint. Amazon Pinpoint
    -- automatically sets this value to INACTIVE if you update another endpoint
    -- that has the same address specified by the Address property.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for
    -- the endpoint.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The unique identifier for the most recent request to update the
    -- endpoint.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint is updated.
    effectiveDate :: Prelude.Maybe Prelude.Text,
    -- | The geographic information for the endpoint.
    location :: Prelude.Maybe EndpointLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'endpointRequest_user' - One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
--
-- 'address', 'endpointRequest_address' - The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For a push-notification
-- channel, use the token provided by the push notification service, such
-- as an Apple Push Notification service (APNs) device token or a Firebase
-- Cloud Messaging (FCM) registration token. For the SMS channel, use a
-- phone number in E.164 format, such as +12065550100. For the email
-- channel, use an email address.
--
-- 'channelType', 'endpointRequest_channelType' - The channel to use when sending messages or push notifications to the
-- endpoint.
--
-- 'optOut', 'endpointRequest_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'demographic', 'endpointRequest_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'attributes', 'endpointRequest_attributes' - One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value
-- can contain up to 100 characters. When you define the name of a custom
-- attribute, avoid using the following characters: number sign (#), colon
-- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
-- Pinpoint console can\'t display attribute names that contain these
-- characters. This restriction doesn\'t apply to attribute values.
--
-- 'endpointStatus', 'endpointRequest_endpointStatus' - Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
--
-- 'metrics', 'endpointRequest_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'requestId', 'endpointRequest_requestId' - The unique identifier for the most recent request to update the
-- endpoint.
--
-- 'effectiveDate', 'endpointRequest_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint is updated.
--
-- 'location', 'endpointRequest_location' - The geographic information for the endpoint.
newEndpointRequest ::
  EndpointRequest
newEndpointRequest =
  EndpointRequest'
    { user = Prelude.Nothing,
      address = Prelude.Nothing,
      channelType = Prelude.Nothing,
      optOut = Prelude.Nothing,
      demographic = Prelude.Nothing,
      attributes = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      metrics = Prelude.Nothing,
      requestId = Prelude.Nothing,
      effectiveDate = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
endpointRequest_user :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointUser)
endpointRequest_user = Lens.lens (\EndpointRequest' {user} -> user) (\s@EndpointRequest' {} a -> s {user = a} :: EndpointRequest)

-- | The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For a push-notification
-- channel, use the token provided by the push notification service, such
-- as an Apple Push Notification service (APNs) device token or a Firebase
-- Cloud Messaging (FCM) registration token. For the SMS channel, use a
-- phone number in E.164 format, such as +12065550100. For the email
-- channel, use an email address.
endpointRequest_address :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_address = Lens.lens (\EndpointRequest' {address} -> address) (\s@EndpointRequest' {} a -> s {address = a} :: EndpointRequest)

-- | The channel to use when sending messages or push notifications to the
-- endpoint.
endpointRequest_channelType :: Lens.Lens' EndpointRequest (Prelude.Maybe ChannelType)
endpointRequest_channelType = Lens.lens (\EndpointRequest' {channelType} -> channelType) (\s@EndpointRequest' {} a -> s {channelType = a} :: EndpointRequest)

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
endpointRequest_optOut :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_optOut = Lens.lens (\EndpointRequest' {optOut} -> optOut) (\s@EndpointRequest' {} a -> s {optOut = a} :: EndpointRequest)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
endpointRequest_demographic :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointDemographic)
endpointRequest_demographic = Lens.lens (\EndpointRequest' {demographic} -> demographic) (\s@EndpointRequest' {} a -> s {demographic = a} :: EndpointRequest)

-- | One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value
-- can contain up to 100 characters. When you define the name of a custom
-- attribute, avoid using the following characters: number sign (#), colon
-- (:), question mark (?), backslash (\\), and slash (\/). The Amazon
-- Pinpoint console can\'t display attribute names that contain these
-- characters. This restriction doesn\'t apply to attribute values.
endpointRequest_attributes :: Lens.Lens' EndpointRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
endpointRequest_attributes = Lens.lens (\EndpointRequest' {attributes} -> attributes) (\s@EndpointRequest' {} a -> s {attributes = a} :: EndpointRequest) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
endpointRequest_endpointStatus :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_endpointStatus = Lens.lens (\EndpointRequest' {endpointStatus} -> endpointStatus) (\s@EndpointRequest' {} a -> s {endpointStatus = a} :: EndpointRequest)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
endpointRequest_metrics :: Lens.Lens' EndpointRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
endpointRequest_metrics = Lens.lens (\EndpointRequest' {metrics} -> metrics) (\s@EndpointRequest' {} a -> s {metrics = a} :: EndpointRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The unique identifier for the most recent request to update the
-- endpoint.
endpointRequest_requestId :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_requestId = Lens.lens (\EndpointRequest' {requestId} -> requestId) (\s@EndpointRequest' {} a -> s {requestId = a} :: EndpointRequest)

-- | The date and time, in ISO 8601 format, when the endpoint is updated.
endpointRequest_effectiveDate :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_effectiveDate = Lens.lens (\EndpointRequest' {effectiveDate} -> effectiveDate) (\s@EndpointRequest' {} a -> s {effectiveDate = a} :: EndpointRequest)

-- | The geographic information for the endpoint.
endpointRequest_location :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointLocation)
endpointRequest_location = Lens.lens (\EndpointRequest' {location} -> location) (\s@EndpointRequest' {} a -> s {location = a} :: EndpointRequest)

instance Prelude.Hashable EndpointRequest

instance Prelude.NFData EndpointRequest

instance Core.ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("User" Core..=) Prelude.<$> user,
            ("Address" Core..=) Prelude.<$> address,
            ("ChannelType" Core..=) Prelude.<$> channelType,
            ("OptOut" Core..=) Prelude.<$> optOut,
            ("Demographic" Core..=) Prelude.<$> demographic,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("EndpointStatus" Core..=)
              Prelude.<$> endpointStatus,
            ("Metrics" Core..=) Prelude.<$> metrics,
            ("RequestId" Core..=) Prelude.<$> requestId,
            ("EffectiveDate" Core..=) Prelude.<$> effectiveDate,
            ("Location" Core..=) Prelude.<$> location
          ]
      )
