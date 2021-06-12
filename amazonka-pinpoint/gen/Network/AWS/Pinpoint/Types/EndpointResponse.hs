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
-- Module      : Network.AWS.Pinpoint.Types.EndpointResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser

-- | Provides information about the channel type and other settings for an
-- endpoint.
--
-- /See:/ 'newEndpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { -- | The unique identifier for the application that\'s associated with the
    -- endpoint.
    applicationId :: Core.Maybe Core.Text,
    -- | One or more custom user attributes that your app reports to Amazon
    -- Pinpoint for the user who\'s associated with the endpoint.
    user :: Core.Maybe EndpointUser,
    -- | The destination address for messages or push notifications that you send
    -- to the endpoint. The address varies by channel. For example, the address
    -- for a push-notification channel is typically the token provided by a
    -- push notification service, such as an Apple Push Notification service
    -- (APNs) device token or a Firebase Cloud Messaging (FCM) registration
    -- token. The address for the SMS channel is a phone number in E.164
    -- format, such as +12065550100. The address for the email channel is an
    -- email address.
    address :: Core.Maybe Core.Text,
    -- | The channel that\'s used when sending messages or push notifications to
    -- the endpoint.
    channelType :: Core.Maybe ChannelType,
    -- | A number from 0-99 that represents the cohort that the endpoint is
    -- assigned to. Endpoints are grouped into cohorts randomly, and each
    -- cohort contains approximately 1 percent of the endpoints for an
    -- application. Amazon Pinpoint assigns cohorts to the holdout or treatment
    -- allocations for campaigns.
    cohortId :: Core.Maybe Core.Text,
    -- | The unique identifier that you assigned to the endpoint. The identifier
    -- should be a globally unique identifier (GUID) to ensure that it doesn\'t
    -- conflict with other endpoint identifiers that are associated with the
    -- application.
    id :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint was created.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the user who\'s associated with the endpoint has opted
    -- out of receiving messages and push notifications from you. Possible
    -- values are: ALL, the user has opted out and doesn\'t want to receive any
    -- messages or push notifications; and, NONE, the user hasn\'t opted out
    -- and wants to receive all messages and push notifications.
    optOut :: Core.Maybe Core.Text,
    -- | The demographic information for the endpoint, such as the time zone and
    -- platform.
    demographic :: Core.Maybe EndpointDemographic,
    -- | One or more custom attributes that describe the endpoint by associating
    -- a name with an array of values. For example, the value of a custom
    -- attribute named Interests might be: [\"Science\", \"Music\",
    -- \"Travel\"]. You can use these attributes as filter criteria when you
    -- create segments.
    attributes :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | Specifies whether messages or push notifications are sent to the
    -- endpoint. Possible values are: ACTIVE, messages are sent to the
    -- endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create
    -- an endpoint or update an existing endpoint. Amazon Pinpoint
    -- automatically sets this value to INACTIVE if you update another endpoint
    -- that has the same address specified by the Address property.
    endpointStatus :: Core.Maybe Core.Text,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for
    -- the endpoint.
    metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double),
    -- | The unique identifier for the most recent request to update the
    -- endpoint.
    requestId :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint was last
    -- updated.
    effectiveDate :: Core.Maybe Core.Text,
    -- | The geographic information for the endpoint.
    location :: Core.Maybe EndpointLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'endpointResponse_applicationId' - The unique identifier for the application that\'s associated with the
-- endpoint.
--
-- 'user', 'endpointResponse_user' - One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
--
-- 'address', 'endpointResponse_address' - The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For example, the address
-- for a push-notification channel is typically the token provided by a
-- push notification service, such as an Apple Push Notification service
-- (APNs) device token or a Firebase Cloud Messaging (FCM) registration
-- token. The address for the SMS channel is a phone number in E.164
-- format, such as +12065550100. The address for the email channel is an
-- email address.
--
-- 'channelType', 'endpointResponse_channelType' - The channel that\'s used when sending messages or push notifications to
-- the endpoint.
--
-- 'cohortId', 'endpointResponse_cohortId' - A number from 0-99 that represents the cohort that the endpoint is
-- assigned to. Endpoints are grouped into cohorts randomly, and each
-- cohort contains approximately 1 percent of the endpoints for an
-- application. Amazon Pinpoint assigns cohorts to the holdout or treatment
-- allocations for campaigns.
--
-- 'id', 'endpointResponse_id' - The unique identifier that you assigned to the endpoint. The identifier
-- should be a globally unique identifier (GUID) to ensure that it doesn\'t
-- conflict with other endpoint identifiers that are associated with the
-- application.
--
-- 'creationDate', 'endpointResponse_creationDate' - The date and time, in ISO 8601 format, when the endpoint was created.
--
-- 'optOut', 'endpointResponse_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'demographic', 'endpointResponse_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'attributes', 'endpointResponse_attributes' - One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments.
--
-- 'endpointStatus', 'endpointResponse_endpointStatus' - Specifies whether messages or push notifications are sent to the
-- endpoint. Possible values are: ACTIVE, messages are sent to the
-- endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
--
-- 'metrics', 'endpointResponse_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'requestId', 'endpointResponse_requestId' - The unique identifier for the most recent request to update the
-- endpoint.
--
-- 'effectiveDate', 'endpointResponse_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
--
-- 'location', 'endpointResponse_location' - The geographic information for the endpoint.
newEndpointResponse ::
  EndpointResponse
newEndpointResponse =
  EndpointResponse'
    { applicationId = Core.Nothing,
      user = Core.Nothing,
      address = Core.Nothing,
      channelType = Core.Nothing,
      cohortId = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      optOut = Core.Nothing,
      demographic = Core.Nothing,
      attributes = Core.Nothing,
      endpointStatus = Core.Nothing,
      metrics = Core.Nothing,
      requestId = Core.Nothing,
      effectiveDate = Core.Nothing,
      location = Core.Nothing
    }

-- | The unique identifier for the application that\'s associated with the
-- endpoint.
endpointResponse_applicationId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_applicationId = Lens.lens (\EndpointResponse' {applicationId} -> applicationId) (\s@EndpointResponse' {} a -> s {applicationId = a} :: EndpointResponse)

-- | One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
endpointResponse_user :: Lens.Lens' EndpointResponse (Core.Maybe EndpointUser)
endpointResponse_user = Lens.lens (\EndpointResponse' {user} -> user) (\s@EndpointResponse' {} a -> s {user = a} :: EndpointResponse)

-- | The destination address for messages or push notifications that you send
-- to the endpoint. The address varies by channel. For example, the address
-- for a push-notification channel is typically the token provided by a
-- push notification service, such as an Apple Push Notification service
-- (APNs) device token or a Firebase Cloud Messaging (FCM) registration
-- token. The address for the SMS channel is a phone number in E.164
-- format, such as +12065550100. The address for the email channel is an
-- email address.
endpointResponse_address :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_address = Lens.lens (\EndpointResponse' {address} -> address) (\s@EndpointResponse' {} a -> s {address = a} :: EndpointResponse)

-- | The channel that\'s used when sending messages or push notifications to
-- the endpoint.
endpointResponse_channelType :: Lens.Lens' EndpointResponse (Core.Maybe ChannelType)
endpointResponse_channelType = Lens.lens (\EndpointResponse' {channelType} -> channelType) (\s@EndpointResponse' {} a -> s {channelType = a} :: EndpointResponse)

-- | A number from 0-99 that represents the cohort that the endpoint is
-- assigned to. Endpoints are grouped into cohorts randomly, and each
-- cohort contains approximately 1 percent of the endpoints for an
-- application. Amazon Pinpoint assigns cohorts to the holdout or treatment
-- allocations for campaigns.
endpointResponse_cohortId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_cohortId = Lens.lens (\EndpointResponse' {cohortId} -> cohortId) (\s@EndpointResponse' {} a -> s {cohortId = a} :: EndpointResponse)

-- | The unique identifier that you assigned to the endpoint. The identifier
-- should be a globally unique identifier (GUID) to ensure that it doesn\'t
-- conflict with other endpoint identifiers that are associated with the
-- application.
endpointResponse_id :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_id = Lens.lens (\EndpointResponse' {id} -> id) (\s@EndpointResponse' {} a -> s {id = a} :: EndpointResponse)

-- | The date and time, in ISO 8601 format, when the endpoint was created.
endpointResponse_creationDate :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_creationDate = Lens.lens (\EndpointResponse' {creationDate} -> creationDate) (\s@EndpointResponse' {} a -> s {creationDate = a} :: EndpointResponse)

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
endpointResponse_optOut :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_optOut = Lens.lens (\EndpointResponse' {optOut} -> optOut) (\s@EndpointResponse' {} a -> s {optOut = a} :: EndpointResponse)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
endpointResponse_demographic :: Lens.Lens' EndpointResponse (Core.Maybe EndpointDemographic)
endpointResponse_demographic = Lens.lens (\EndpointResponse' {demographic} -> demographic) (\s@EndpointResponse' {} a -> s {demographic = a} :: EndpointResponse)

-- | One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments.
endpointResponse_attributes :: Lens.Lens' EndpointResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
endpointResponse_attributes = Lens.lens (\EndpointResponse' {attributes} -> attributes) (\s@EndpointResponse' {} a -> s {attributes = a} :: EndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether messages or push notifications are sent to the
-- endpoint. Possible values are: ACTIVE, messages are sent to the
-- endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
endpointResponse_endpointStatus :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_endpointStatus = Lens.lens (\EndpointResponse' {endpointStatus} -> endpointStatus) (\s@EndpointResponse' {} a -> s {endpointStatus = a} :: EndpointResponse)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
endpointResponse_metrics :: Lens.Lens' EndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Double))
endpointResponse_metrics = Lens.lens (\EndpointResponse' {metrics} -> metrics) (\s@EndpointResponse' {} a -> s {metrics = a} :: EndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the most recent request to update the
-- endpoint.
endpointResponse_requestId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_requestId = Lens.lens (\EndpointResponse' {requestId} -> requestId) (\s@EndpointResponse' {} a -> s {requestId = a} :: EndpointResponse)

-- | The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
endpointResponse_effectiveDate :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
endpointResponse_effectiveDate = Lens.lens (\EndpointResponse' {effectiveDate} -> effectiveDate) (\s@EndpointResponse' {} a -> s {effectiveDate = a} :: EndpointResponse)

-- | The geographic information for the endpoint.
endpointResponse_location :: Lens.Lens' EndpointResponse (Core.Maybe EndpointLocation)
endpointResponse_location = Lens.lens (\EndpointResponse' {location} -> location) (\s@EndpointResponse' {} a -> s {location = a} :: EndpointResponse)

instance Core.FromJSON EndpointResponse where
  parseJSON =
    Core.withObject
      "EndpointResponse"
      ( \x ->
          EndpointResponse'
            Core.<$> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "User")
            Core.<*> (x Core..:? "Address")
            Core.<*> (x Core..:? "ChannelType")
            Core.<*> (x Core..:? "CohortId")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "OptOut")
            Core.<*> (x Core..:? "Demographic")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "EndpointStatus")
            Core.<*> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RequestId")
            Core.<*> (x Core..:? "EffectiveDate")
            Core.<*> (x Core..:? "Location")
      )

instance Core.Hashable EndpointResponse

instance Core.NFData EndpointResponse
