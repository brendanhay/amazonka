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
-- Module      : Amazonka.Pinpoint.Types.EndpointRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ChannelType
import Amazonka.Pinpoint.Types.EndpointDemographic
import Amazonka.Pinpoint.Types.EndpointLocation
import Amazonka.Pinpoint.Types.EndpointUser
import qualified Amazonka.Prelude as Prelude

-- | Specifies the channel type and other settings for an endpoint.
--
-- /See:/ 'newEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { -- | The destination address for messages or push notifications that you send
    -- to the endpoint. The address varies by channel. For a push-notification
    -- channel, use the token provided by the push notification service, such
    -- as an Apple Push Notification service (APNs) device token or a Firebase
    -- Cloud Messaging (FCM) registration token. For the SMS channel, use a
    -- phone number in E.164 format, such as +12065550100. For the email
    -- channel, use an email address.
    address :: Prelude.Maybe Prelude.Text,
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
    -- | The channel to use when sending messages or push notifications to the
    -- endpoint.
    channelType :: Prelude.Maybe ChannelType,
    -- | The demographic information for the endpoint, such as the time zone and
    -- platform.
    demographic :: Prelude.Maybe EndpointDemographic,
    -- | The date and time, in ISO 8601 format, when the endpoint is updated.
    effectiveDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to send messages or push notifications to the
    -- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
    -- and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create
    -- an endpoint or update an existing endpoint. Amazon Pinpoint
    -- automatically sets this value to INACTIVE if you update another endpoint
    -- that has the same address specified by the Address property.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | The geographic information for the endpoint.
    location :: Prelude.Maybe EndpointLocation,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for
    -- the endpoint.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | Specifies whether the user who\'s associated with the endpoint has opted
    -- out of receiving messages and push notifications from you. Possible
    -- values are: ALL, the user has opted out and doesn\'t want to receive any
    -- messages or push notifications; and, NONE, the user hasn\'t opted out
    -- and wants to receive all messages and push notifications.
    optOut :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the most recent request to update the
    -- endpoint.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that describe the user who\'s associated
    -- with the endpoint.
    user :: Prelude.Maybe EndpointUser
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
-- 'address', 'endpointRequest_address' - The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For a push-notification
-- channel, use the token provided by the push notification service, such
-- as an Apple Push Notification service (APNs) device token or a Firebase
-- Cloud Messaging (FCM) registration token. For the SMS channel, use a
-- phone number in E.164 format, such as +12065550100. For the email
-- channel, use an email address.
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
-- 'channelType', 'endpointRequest_channelType' - The channel to use when sending messages or push notifications to the
-- endpoint.
--
-- 'demographic', 'endpointRequest_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'effectiveDate', 'endpointRequest_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint is updated.
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
-- 'location', 'endpointRequest_location' - The geographic information for the endpoint.
--
-- 'metrics', 'endpointRequest_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'optOut', 'endpointRequest_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'requestId', 'endpointRequest_requestId' - The unique identifier for the most recent request to update the
-- endpoint.
--
-- 'user', 'endpointRequest_user' - One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
newEndpointRequest ::
  EndpointRequest
newEndpointRequest =
  EndpointRequest'
    { address = Prelude.Nothing,
      attributes = Prelude.Nothing,
      channelType = Prelude.Nothing,
      demographic = Prelude.Nothing,
      effectiveDate = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      location = Prelude.Nothing,
      metrics = Prelude.Nothing,
      optOut = Prelude.Nothing,
      requestId = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For a push-notification
-- channel, use the token provided by the push notification service, such
-- as an Apple Push Notification service (APNs) device token or a Firebase
-- Cloud Messaging (FCM) registration token. For the SMS channel, use a
-- phone number in E.164 format, such as +12065550100. For the email
-- channel, use an email address.
endpointRequest_address :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_address = Lens.lens (\EndpointRequest' {address} -> address) (\s@EndpointRequest' {} a -> s {address = a} :: EndpointRequest)

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
endpointRequest_attributes = Lens.lens (\EndpointRequest' {attributes} -> attributes) (\s@EndpointRequest' {} a -> s {attributes = a} :: EndpointRequest) Prelude.. Lens.mapping Lens.coerced

-- | The channel to use when sending messages or push notifications to the
-- endpoint.
endpointRequest_channelType :: Lens.Lens' EndpointRequest (Prelude.Maybe ChannelType)
endpointRequest_channelType = Lens.lens (\EndpointRequest' {channelType} -> channelType) (\s@EndpointRequest' {} a -> s {channelType = a} :: EndpointRequest)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
endpointRequest_demographic :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointDemographic)
endpointRequest_demographic = Lens.lens (\EndpointRequest' {demographic} -> demographic) (\s@EndpointRequest' {} a -> s {demographic = a} :: EndpointRequest)

-- | The date and time, in ISO 8601 format, when the endpoint is updated.
endpointRequest_effectiveDate :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_effectiveDate = Lens.lens (\EndpointRequest' {effectiveDate} -> effectiveDate) (\s@EndpointRequest' {} a -> s {effectiveDate = a} :: EndpointRequest)

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

-- | The geographic information for the endpoint.
endpointRequest_location :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointLocation)
endpointRequest_location = Lens.lens (\EndpointRequest' {location} -> location) (\s@EndpointRequest' {} a -> s {location = a} :: EndpointRequest)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
endpointRequest_metrics :: Lens.Lens' EndpointRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
endpointRequest_metrics = Lens.lens (\EndpointRequest' {metrics} -> metrics) (\s@EndpointRequest' {} a -> s {metrics = a} :: EndpointRequest) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
endpointRequest_optOut :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_optOut = Lens.lens (\EndpointRequest' {optOut} -> optOut) (\s@EndpointRequest' {} a -> s {optOut = a} :: EndpointRequest)

-- | The unique identifier for the most recent request to update the
-- endpoint.
endpointRequest_requestId :: Lens.Lens' EndpointRequest (Prelude.Maybe Prelude.Text)
endpointRequest_requestId = Lens.lens (\EndpointRequest' {requestId} -> requestId) (\s@EndpointRequest' {} a -> s {requestId = a} :: EndpointRequest)

-- | One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
endpointRequest_user :: Lens.Lens' EndpointRequest (Prelude.Maybe EndpointUser)
endpointRequest_user = Lens.lens (\EndpointRequest' {user} -> user) (\s@EndpointRequest' {} a -> s {user = a} :: EndpointRequest)

instance Prelude.Hashable EndpointRequest where
  hashWithSalt _salt EndpointRequest' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` demographic
      `Prelude.hashWithSalt` effectiveDate
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` optOut
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` user

instance Prelude.NFData EndpointRequest where
  rnf EndpointRequest' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf demographic
      `Prelude.seq` Prelude.rnf effectiveDate
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf optOut
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf user

instance Data.ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("ChannelType" Data..=) Prelude.<$> channelType,
            ("Demographic" Data..=) Prelude.<$> demographic,
            ("EffectiveDate" Data..=) Prelude.<$> effectiveDate,
            ("EndpointStatus" Data..=)
              Prelude.<$> endpointStatus,
            ("Location" Data..=) Prelude.<$> location,
            ("Metrics" Data..=) Prelude.<$> metrics,
            ("OptOut" Data..=) Prelude.<$> optOut,
            ("RequestId" Data..=) Prelude.<$> requestId,
            ("User" Data..=) Prelude.<$> user
          ]
      )
