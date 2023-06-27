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
-- Module      : Amazonka.Pinpoint.Types.PublicEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.PublicEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ChannelType
import Amazonka.Pinpoint.Types.EndpointDemographic
import Amazonka.Pinpoint.Types.EndpointLocation
import Amazonka.Pinpoint.Types.EndpointUser
import qualified Amazonka.Prelude as Prelude

-- | Specifies the properties and attributes of an endpoint that\'s
-- associated with an event.
--
-- /See:/ 'newPublicEndpoint' smart constructor.
data PublicEndpoint = PublicEndpoint'
  { -- | The unique identifier for the recipient, such as a device token, email
    -- address, or mobile phone number.
    address :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that describe the endpoint by associating
    -- a name with an array of values. You can use these attributes as filter
    -- criteria when you create segments.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The channel that\'s used when sending messages or push notifications to
    -- the endpoint.
    channelType :: Prelude.Maybe ChannelType,
    -- | The demographic information for the endpoint, such as the time zone and
    -- platform.
    demographic :: Prelude.Maybe EndpointDemographic,
    -- | The date and time, in ISO 8601 format, when the endpoint was last
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
    -- | A unique identifier that\'s generated each time the endpoint is updated.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | One or more custom user attributes that your app reports to Amazon
    -- Pinpoint for the user who\'s associated with the endpoint.
    user :: Prelude.Maybe EndpointUser
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'publicEndpoint_address' - The unique identifier for the recipient, such as a device token, email
-- address, or mobile phone number.
--
-- 'attributes', 'publicEndpoint_attributes' - One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. You can use these attributes as filter
-- criteria when you create segments.
--
-- 'channelType', 'publicEndpoint_channelType' - The channel that\'s used when sending messages or push notifications to
-- the endpoint.
--
-- 'demographic', 'publicEndpoint_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'effectiveDate', 'publicEndpoint_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
--
-- 'endpointStatus', 'publicEndpoint_endpointStatus' - Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
--
-- 'location', 'publicEndpoint_location' - The geographic information for the endpoint.
--
-- 'metrics', 'publicEndpoint_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'optOut', 'publicEndpoint_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'requestId', 'publicEndpoint_requestId' - A unique identifier that\'s generated each time the endpoint is updated.
--
-- 'user', 'publicEndpoint_user' - One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
newPublicEndpoint ::
  PublicEndpoint
newPublicEndpoint =
  PublicEndpoint'
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

-- | The unique identifier for the recipient, such as a device token, email
-- address, or mobile phone number.
publicEndpoint_address :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_address = Lens.lens (\PublicEndpoint' {address} -> address) (\s@PublicEndpoint' {} a -> s {address = a} :: PublicEndpoint)

-- | One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. You can use these attributes as filter
-- criteria when you create segments.
publicEndpoint_attributes :: Lens.Lens' PublicEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
publicEndpoint_attributes = Lens.lens (\PublicEndpoint' {attributes} -> attributes) (\s@PublicEndpoint' {} a -> s {attributes = a} :: PublicEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The channel that\'s used when sending messages or push notifications to
-- the endpoint.
publicEndpoint_channelType :: Lens.Lens' PublicEndpoint (Prelude.Maybe ChannelType)
publicEndpoint_channelType = Lens.lens (\PublicEndpoint' {channelType} -> channelType) (\s@PublicEndpoint' {} a -> s {channelType = a} :: PublicEndpoint)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
publicEndpoint_demographic :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointDemographic)
publicEndpoint_demographic = Lens.lens (\PublicEndpoint' {demographic} -> demographic) (\s@PublicEndpoint' {} a -> s {demographic = a} :: PublicEndpoint)

-- | The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
publicEndpoint_effectiveDate :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_effectiveDate = Lens.lens (\PublicEndpoint' {effectiveDate} -> effectiveDate) (\s@PublicEndpoint' {} a -> s {effectiveDate = a} :: PublicEndpoint)

-- | Specifies whether to send messages or push notifications to the
-- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
-- and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
publicEndpoint_endpointStatus :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_endpointStatus = Lens.lens (\PublicEndpoint' {endpointStatus} -> endpointStatus) (\s@PublicEndpoint' {} a -> s {endpointStatus = a} :: PublicEndpoint)

-- | The geographic information for the endpoint.
publicEndpoint_location :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointLocation)
publicEndpoint_location = Lens.lens (\PublicEndpoint' {location} -> location) (\s@PublicEndpoint' {} a -> s {location = a} :: PublicEndpoint)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
publicEndpoint_metrics :: Lens.Lens' PublicEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
publicEndpoint_metrics = Lens.lens (\PublicEndpoint' {metrics} -> metrics) (\s@PublicEndpoint' {} a -> s {metrics = a} :: PublicEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
publicEndpoint_optOut :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_optOut = Lens.lens (\PublicEndpoint' {optOut} -> optOut) (\s@PublicEndpoint' {} a -> s {optOut = a} :: PublicEndpoint)

-- | A unique identifier that\'s generated each time the endpoint is updated.
publicEndpoint_requestId :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_requestId = Lens.lens (\PublicEndpoint' {requestId} -> requestId) (\s@PublicEndpoint' {} a -> s {requestId = a} :: PublicEndpoint)

-- | One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
publicEndpoint_user :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointUser)
publicEndpoint_user = Lens.lens (\PublicEndpoint' {user} -> user) (\s@PublicEndpoint' {} a -> s {user = a} :: PublicEndpoint)

instance Prelude.Hashable PublicEndpoint where
  hashWithSalt _salt PublicEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` address
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

instance Prelude.NFData PublicEndpoint where
  rnf PublicEndpoint' {..} =
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

instance Data.ToJSON PublicEndpoint where
  toJSON PublicEndpoint' {..} =
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
