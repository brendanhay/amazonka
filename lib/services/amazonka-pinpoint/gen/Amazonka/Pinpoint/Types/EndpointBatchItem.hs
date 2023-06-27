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
-- Module      : Amazonka.Pinpoint.Types.EndpointBatchItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointBatchItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ChannelType
import Amazonka.Pinpoint.Types.EndpointDemographic
import Amazonka.Pinpoint.Types.EndpointLocation
import Amazonka.Pinpoint.Types.EndpointUser
import qualified Amazonka.Prelude as Prelude

-- | Specifies an endpoint to create or update and the settings and
-- attributes to set or change for the endpoint.
--
-- /See:/ 'newEndpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
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
    -- | The date and time, in ISO 8601 format, when the endpoint was created or
    -- updated.
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
    -- | The unique identifier for the endpoint in the context of the batch.
    id :: Prelude.Maybe Prelude.Text,
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
    -- | The unique identifier for the request to create or update the endpoint.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that describe the user who\'s associated
    -- with the endpoint.
    user :: Prelude.Maybe EndpointUser
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointBatchItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpointBatchItem_address' - The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For a push-notification
-- channel, use the token provided by the push notification service, such
-- as an Apple Push Notification service (APNs) device token or a Firebase
-- Cloud Messaging (FCM) registration token. For the SMS channel, use a
-- phone number in E.164 format, such as +12065550100. For the email
-- channel, use an email address.
--
-- 'attributes', 'endpointBatchItem_attributes' - One or more custom attributes that describe the endpoint by associating
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
-- 'channelType', 'endpointBatchItem_channelType' - The channel to use when sending messages or push notifications to the
-- endpoint.
--
-- 'demographic', 'endpointBatchItem_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'effectiveDate', 'endpointBatchItem_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was created or
-- updated.
--
-- 'endpointStatus', 'endpointBatchItem_endpointStatus' - Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
--
-- 'id', 'endpointBatchItem_id' - The unique identifier for the endpoint in the context of the batch.
--
-- 'location', 'endpointBatchItem_location' - The geographic information for the endpoint.
--
-- 'metrics', 'endpointBatchItem_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'optOut', 'endpointBatchItem_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'requestId', 'endpointBatchItem_requestId' - The unique identifier for the request to create or update the endpoint.
--
-- 'user', 'endpointBatchItem_user' - One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
newEndpointBatchItem ::
  EndpointBatchItem
newEndpointBatchItem =
  EndpointBatchItem'
    { address = Prelude.Nothing,
      attributes = Prelude.Nothing,
      channelType = Prelude.Nothing,
      demographic = Prelude.Nothing,
      effectiveDate = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      id = Prelude.Nothing,
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
endpointBatchItem_address :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_address = Lens.lens (\EndpointBatchItem' {address} -> address) (\s@EndpointBatchItem' {} a -> s {address = a} :: EndpointBatchItem)

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
endpointBatchItem_attributes :: Lens.Lens' EndpointBatchItem (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
endpointBatchItem_attributes = Lens.lens (\EndpointBatchItem' {attributes} -> attributes) (\s@EndpointBatchItem' {} a -> s {attributes = a} :: EndpointBatchItem) Prelude.. Lens.mapping Lens.coerced

-- | The channel to use when sending messages or push notifications to the
-- endpoint.
endpointBatchItem_channelType :: Lens.Lens' EndpointBatchItem (Prelude.Maybe ChannelType)
endpointBatchItem_channelType = Lens.lens (\EndpointBatchItem' {channelType} -> channelType) (\s@EndpointBatchItem' {} a -> s {channelType = a} :: EndpointBatchItem)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
endpointBatchItem_demographic :: Lens.Lens' EndpointBatchItem (Prelude.Maybe EndpointDemographic)
endpointBatchItem_demographic = Lens.lens (\EndpointBatchItem' {demographic} -> demographic) (\s@EndpointBatchItem' {} a -> s {demographic = a} :: EndpointBatchItem)

-- | The date and time, in ISO 8601 format, when the endpoint was created or
-- updated.
endpointBatchItem_effectiveDate :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_effectiveDate = Lens.lens (\EndpointBatchItem' {effectiveDate} -> effectiveDate) (\s@EndpointBatchItem' {} a -> s {effectiveDate = a} :: EndpointBatchItem)

-- | Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
endpointBatchItem_endpointStatus :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_endpointStatus = Lens.lens (\EndpointBatchItem' {endpointStatus} -> endpointStatus) (\s@EndpointBatchItem' {} a -> s {endpointStatus = a} :: EndpointBatchItem)

-- | The unique identifier for the endpoint in the context of the batch.
endpointBatchItem_id :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_id = Lens.lens (\EndpointBatchItem' {id} -> id) (\s@EndpointBatchItem' {} a -> s {id = a} :: EndpointBatchItem)

-- | The geographic information for the endpoint.
endpointBatchItem_location :: Lens.Lens' EndpointBatchItem (Prelude.Maybe EndpointLocation)
endpointBatchItem_location = Lens.lens (\EndpointBatchItem' {location} -> location) (\s@EndpointBatchItem' {} a -> s {location = a} :: EndpointBatchItem)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
endpointBatchItem_metrics :: Lens.Lens' EndpointBatchItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
endpointBatchItem_metrics = Lens.lens (\EndpointBatchItem' {metrics} -> metrics) (\s@EndpointBatchItem' {} a -> s {metrics = a} :: EndpointBatchItem) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
endpointBatchItem_optOut :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_optOut = Lens.lens (\EndpointBatchItem' {optOut} -> optOut) (\s@EndpointBatchItem' {} a -> s {optOut = a} :: EndpointBatchItem)

-- | The unique identifier for the request to create or update the endpoint.
endpointBatchItem_requestId :: Lens.Lens' EndpointBatchItem (Prelude.Maybe Prelude.Text)
endpointBatchItem_requestId = Lens.lens (\EndpointBatchItem' {requestId} -> requestId) (\s@EndpointBatchItem' {} a -> s {requestId = a} :: EndpointBatchItem)

-- | One or more custom attributes that describe the user who\'s associated
-- with the endpoint.
endpointBatchItem_user :: Lens.Lens' EndpointBatchItem (Prelude.Maybe EndpointUser)
endpointBatchItem_user = Lens.lens (\EndpointBatchItem' {user} -> user) (\s@EndpointBatchItem' {} a -> s {user = a} :: EndpointBatchItem)

instance Prelude.Hashable EndpointBatchItem where
  hashWithSalt _salt EndpointBatchItem' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` demographic
      `Prelude.hashWithSalt` effectiveDate
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` optOut
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` user

instance Prelude.NFData EndpointBatchItem where
  rnf EndpointBatchItem' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf demographic
      `Prelude.seq` Prelude.rnf effectiveDate
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf optOut
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf user

instance Data.ToJSON EndpointBatchItem where
  toJSON EndpointBatchItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("ChannelType" Data..=) Prelude.<$> channelType,
            ("Demographic" Data..=) Prelude.<$> demographic,
            ("EffectiveDate" Data..=) Prelude.<$> effectiveDate,
            ("EndpointStatus" Data..=)
              Prelude.<$> endpointStatus,
            ("Id" Data..=) Prelude.<$> id,
            ("Location" Data..=) Prelude.<$> location,
            ("Metrics" Data..=) Prelude.<$> metrics,
            ("OptOut" Data..=) Prelude.<$> optOut,
            ("RequestId" Data..=) Prelude.<$> requestId,
            ("User" Data..=) Prelude.<$> user
          ]
      )
