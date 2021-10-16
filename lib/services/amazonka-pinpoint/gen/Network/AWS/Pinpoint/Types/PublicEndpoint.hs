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
-- Module      : Network.AWS.Pinpoint.Types.PublicEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PublicEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the properties and attributes of an endpoint that\'s
-- associated with an event.
--
-- /See:/ 'newPublicEndpoint' smart constructor.
data PublicEndpoint = PublicEndpoint'
  { -- | One or more custom user attributes that your app reports to Amazon
    -- Pinpoint for the user who\'s associated with the endpoint.
    user :: Prelude.Maybe EndpointUser,
    -- | The unique identifier for the recipient, such as a device token, email
    -- address, or mobile phone number.
    address :: Prelude.Maybe Prelude.Text,
    -- | The channel that\'s used when sending messages or push notifications to
    -- the endpoint.
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
    -- a name with an array of values. You can use these attributes as filter
    -- criteria when you create segments.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for
    -- the endpoint.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | Specifies whether to send messages or push notifications to the
    -- endpoint. Valid values are: ACTIVE, messages are sent to the endpoint;
    -- and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create
    -- an endpoint or update an existing endpoint. Amazon Pinpoint
    -- automatically sets this value to INACTIVE if you update another endpoint
    -- that has the same address specified by the Address property.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that\'s generated each time the endpoint is updated.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint was last
    -- updated.
    effectiveDate :: Prelude.Maybe Prelude.Text,
    -- | The geographic information for the endpoint.
    location :: Prelude.Maybe EndpointLocation
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
-- 'user', 'publicEndpoint_user' - One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
--
-- 'address', 'publicEndpoint_address' - The unique identifier for the recipient, such as a device token, email
-- address, or mobile phone number.
--
-- 'channelType', 'publicEndpoint_channelType' - The channel that\'s used when sending messages or push notifications to
-- the endpoint.
--
-- 'optOut', 'publicEndpoint_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'demographic', 'publicEndpoint_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'attributes', 'publicEndpoint_attributes' - One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. You can use these attributes as filter
-- criteria when you create segments.
--
-- 'metrics', 'publicEndpoint_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
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
-- 'requestId', 'publicEndpoint_requestId' - A unique identifier that\'s generated each time the endpoint is updated.
--
-- 'effectiveDate', 'publicEndpoint_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
--
-- 'location', 'publicEndpoint_location' - The geographic information for the endpoint.
newPublicEndpoint ::
  PublicEndpoint
newPublicEndpoint =
  PublicEndpoint'
    { user = Prelude.Nothing,
      address = Prelude.Nothing,
      channelType = Prelude.Nothing,
      optOut = Prelude.Nothing,
      demographic = Prelude.Nothing,
      attributes = Prelude.Nothing,
      metrics = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      effectiveDate = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
publicEndpoint_user :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointUser)
publicEndpoint_user = Lens.lens (\PublicEndpoint' {user} -> user) (\s@PublicEndpoint' {} a -> s {user = a} :: PublicEndpoint)

-- | The unique identifier for the recipient, such as a device token, email
-- address, or mobile phone number.
publicEndpoint_address :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_address = Lens.lens (\PublicEndpoint' {address} -> address) (\s@PublicEndpoint' {} a -> s {address = a} :: PublicEndpoint)

-- | The channel that\'s used when sending messages or push notifications to
-- the endpoint.
publicEndpoint_channelType :: Lens.Lens' PublicEndpoint (Prelude.Maybe ChannelType)
publicEndpoint_channelType = Lens.lens (\PublicEndpoint' {channelType} -> channelType) (\s@PublicEndpoint' {} a -> s {channelType = a} :: PublicEndpoint)

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
publicEndpoint_optOut :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_optOut = Lens.lens (\PublicEndpoint' {optOut} -> optOut) (\s@PublicEndpoint' {} a -> s {optOut = a} :: PublicEndpoint)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
publicEndpoint_demographic :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointDemographic)
publicEndpoint_demographic = Lens.lens (\PublicEndpoint' {demographic} -> demographic) (\s@PublicEndpoint' {} a -> s {demographic = a} :: PublicEndpoint)

-- | One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. You can use these attributes as filter
-- criteria when you create segments.
publicEndpoint_attributes :: Lens.Lens' PublicEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
publicEndpoint_attributes = Lens.lens (\PublicEndpoint' {attributes} -> attributes) (\s@PublicEndpoint' {} a -> s {attributes = a} :: PublicEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
publicEndpoint_metrics :: Lens.Lens' PublicEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
publicEndpoint_metrics = Lens.lens (\PublicEndpoint' {metrics} -> metrics) (\s@PublicEndpoint' {} a -> s {metrics = a} :: PublicEndpoint) Prelude.. Lens.mapping Lens._Coerce

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

-- | A unique identifier that\'s generated each time the endpoint is updated.
publicEndpoint_requestId :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_requestId = Lens.lens (\PublicEndpoint' {requestId} -> requestId) (\s@PublicEndpoint' {} a -> s {requestId = a} :: PublicEndpoint)

-- | The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
publicEndpoint_effectiveDate :: Lens.Lens' PublicEndpoint (Prelude.Maybe Prelude.Text)
publicEndpoint_effectiveDate = Lens.lens (\PublicEndpoint' {effectiveDate} -> effectiveDate) (\s@PublicEndpoint' {} a -> s {effectiveDate = a} :: PublicEndpoint)

-- | The geographic information for the endpoint.
publicEndpoint_location :: Lens.Lens' PublicEndpoint (Prelude.Maybe EndpointLocation)
publicEndpoint_location = Lens.lens (\PublicEndpoint' {location} -> location) (\s@PublicEndpoint' {} a -> s {location = a} :: PublicEndpoint)

instance Prelude.Hashable PublicEndpoint

instance Prelude.NFData PublicEndpoint

instance Core.ToJSON PublicEndpoint where
  toJSON PublicEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("User" Core..=) Prelude.<$> user,
            ("Address" Core..=) Prelude.<$> address,
            ("ChannelType" Core..=) Prelude.<$> channelType,
            ("OptOut" Core..=) Prelude.<$> optOut,
            ("Demographic" Core..=) Prelude.<$> demographic,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Metrics" Core..=) Prelude.<$> metrics,
            ("EndpointStatus" Core..=)
              Prelude.<$> endpointStatus,
            ("RequestId" Core..=) Prelude.<$> requestId,
            ("EffectiveDate" Core..=) Prelude.<$> effectiveDate,
            ("Location" Core..=) Prelude.<$> location
          ]
      )
