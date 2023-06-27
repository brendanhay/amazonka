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
-- Module      : Amazonka.Pinpoint.Types.EndpointResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ChannelType
import Amazonka.Pinpoint.Types.EndpointDemographic
import Amazonka.Pinpoint.Types.EndpointLocation
import Amazonka.Pinpoint.Types.EndpointUser
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the channel type and other settings for an
-- endpoint.
--
-- /See:/ 'newEndpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { -- | The destination address for messages or push notifications that you send
    -- to the endpoint. The address varies by channel. For example, the address
    -- for a push-notification channel is typically the token provided by a
    -- push notification service, such as an Apple Push Notification service
    -- (APNs) device token or a Firebase Cloud Messaging (FCM) registration
    -- token. The address for the SMS channel is a phone number in E.164
    -- format, such as +12065550100. The address for the email channel is an
    -- email address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that\'s associated with the
    -- endpoint.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that describe the endpoint by associating
    -- a name with an array of values. For example, the value of a custom
    -- attribute named Interests might be: [\"Science\", \"Music\",
    -- \"Travel\"]. You can use these attributes as filter criteria when you
    -- create segments.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The channel that\'s used when sending messages or push notifications to
    -- the endpoint.
    channelType :: Prelude.Maybe ChannelType,
    -- | A number from 0-99 that represents the cohort that the endpoint is
    -- assigned to. Endpoints are grouped into cohorts randomly, and each
    -- cohort contains approximately 1 percent of the endpoints for an
    -- application. Amazon Pinpoint assigns cohorts to the holdout or treatment
    -- allocations for campaigns.
    cohortId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The demographic information for the endpoint, such as the time zone and
    -- platform.
    demographic :: Prelude.Maybe EndpointDemographic,
    -- | The date and time, in ISO 8601 format, when the endpoint was last
    -- updated.
    effectiveDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether messages or push notifications are sent to the
    -- endpoint. Possible values are: ACTIVE, messages are sent to the
    -- endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create
    -- an endpoint or update an existing endpoint. Amazon Pinpoint
    -- automatically sets this value to INACTIVE if you update another endpoint
    -- that has the same address specified by the Address property.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that you assigned to the endpoint. The identifier
    -- should be a globally unique identifier (GUID) to ensure that it doesn\'t
    -- conflict with other endpoint identifiers that are associated with the
    -- application.
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
    -- | The unique identifier for the most recent request to update the
    -- endpoint.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | One or more custom user attributes that your app reports to Amazon
    -- Pinpoint for the user who\'s associated with the endpoint.
    user :: Prelude.Maybe EndpointUser
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'applicationId', 'endpointResponse_applicationId' - The unique identifier for the application that\'s associated with the
-- endpoint.
--
-- 'attributes', 'endpointResponse_attributes' - One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments.
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
-- 'creationDate', 'endpointResponse_creationDate' - The date and time, in ISO 8601 format, when the endpoint was created.
--
-- 'demographic', 'endpointResponse_demographic' - The demographic information for the endpoint, such as the time zone and
-- platform.
--
-- 'effectiveDate', 'endpointResponse_effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
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
-- 'id', 'endpointResponse_id' - The unique identifier that you assigned to the endpoint. The identifier
-- should be a globally unique identifier (GUID) to ensure that it doesn\'t
-- conflict with other endpoint identifiers that are associated with the
-- application.
--
-- 'location', 'endpointResponse_location' - The geographic information for the endpoint.
--
-- 'metrics', 'endpointResponse_metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
--
-- 'optOut', 'endpointResponse_optOut' - Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
--
-- 'requestId', 'endpointResponse_requestId' - The unique identifier for the most recent request to update the
-- endpoint.
--
-- 'user', 'endpointResponse_user' - One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
newEndpointResponse ::
  EndpointResponse
newEndpointResponse =
  EndpointResponse'
    { address = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      attributes = Prelude.Nothing,
      channelType = Prelude.Nothing,
      cohortId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
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
-- to the endpoint. The address varies by channel. For example, the address
-- for a push-notification channel is typically the token provided by a
-- push notification service, such as an Apple Push Notification service
-- (APNs) device token or a Firebase Cloud Messaging (FCM) registration
-- token. The address for the SMS channel is a phone number in E.164
-- format, such as +12065550100. The address for the email channel is an
-- email address.
endpointResponse_address :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_address = Lens.lens (\EndpointResponse' {address} -> address) (\s@EndpointResponse' {} a -> s {address = a} :: EndpointResponse)

-- | The unique identifier for the application that\'s associated with the
-- endpoint.
endpointResponse_applicationId :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_applicationId = Lens.lens (\EndpointResponse' {applicationId} -> applicationId) (\s@EndpointResponse' {} a -> s {applicationId = a} :: EndpointResponse)

-- | One or more custom attributes that describe the endpoint by associating
-- a name with an array of values. For example, the value of a custom
-- attribute named Interests might be: [\"Science\", \"Music\",
-- \"Travel\"]. You can use these attributes as filter criteria when you
-- create segments.
endpointResponse_attributes :: Lens.Lens' EndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
endpointResponse_attributes = Lens.lens (\EndpointResponse' {attributes} -> attributes) (\s@EndpointResponse' {} a -> s {attributes = a} :: EndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The channel that\'s used when sending messages or push notifications to
-- the endpoint.
endpointResponse_channelType :: Lens.Lens' EndpointResponse (Prelude.Maybe ChannelType)
endpointResponse_channelType = Lens.lens (\EndpointResponse' {channelType} -> channelType) (\s@EndpointResponse' {} a -> s {channelType = a} :: EndpointResponse)

-- | A number from 0-99 that represents the cohort that the endpoint is
-- assigned to. Endpoints are grouped into cohorts randomly, and each
-- cohort contains approximately 1 percent of the endpoints for an
-- application. Amazon Pinpoint assigns cohorts to the holdout or treatment
-- allocations for campaigns.
endpointResponse_cohortId :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_cohortId = Lens.lens (\EndpointResponse' {cohortId} -> cohortId) (\s@EndpointResponse' {} a -> s {cohortId = a} :: EndpointResponse)

-- | The date and time, in ISO 8601 format, when the endpoint was created.
endpointResponse_creationDate :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_creationDate = Lens.lens (\EndpointResponse' {creationDate} -> creationDate) (\s@EndpointResponse' {} a -> s {creationDate = a} :: EndpointResponse)

-- | The demographic information for the endpoint, such as the time zone and
-- platform.
endpointResponse_demographic :: Lens.Lens' EndpointResponse (Prelude.Maybe EndpointDemographic)
endpointResponse_demographic = Lens.lens (\EndpointResponse' {demographic} -> demographic) (\s@EndpointResponse' {} a -> s {demographic = a} :: EndpointResponse)

-- | The date and time, in ISO 8601 format, when the endpoint was last
-- updated.
endpointResponse_effectiveDate :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_effectiveDate = Lens.lens (\EndpointResponse' {effectiveDate} -> effectiveDate) (\s@EndpointResponse' {} a -> s {effectiveDate = a} :: EndpointResponse)

-- | Specifies whether messages or push notifications are sent to the
-- endpoint. Possible values are: ACTIVE, messages are sent to the
-- endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create
-- an endpoint or update an existing endpoint. Amazon Pinpoint
-- automatically sets this value to INACTIVE if you update another endpoint
-- that has the same address specified by the Address property.
endpointResponse_endpointStatus :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_endpointStatus = Lens.lens (\EndpointResponse' {endpointStatus} -> endpointStatus) (\s@EndpointResponse' {} a -> s {endpointStatus = a} :: EndpointResponse)

-- | The unique identifier that you assigned to the endpoint. The identifier
-- should be a globally unique identifier (GUID) to ensure that it doesn\'t
-- conflict with other endpoint identifiers that are associated with the
-- application.
endpointResponse_id :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_id = Lens.lens (\EndpointResponse' {id} -> id) (\s@EndpointResponse' {} a -> s {id = a} :: EndpointResponse)

-- | The geographic information for the endpoint.
endpointResponse_location :: Lens.Lens' EndpointResponse (Prelude.Maybe EndpointLocation)
endpointResponse_location = Lens.lens (\EndpointResponse' {location} -> location) (\s@EndpointResponse' {} a -> s {location = a} :: EndpointResponse)

-- | One or more custom metrics that your app reports to Amazon Pinpoint for
-- the endpoint.
endpointResponse_metrics :: Lens.Lens' EndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
endpointResponse_metrics = Lens.lens (\EndpointResponse' {metrics} -> metrics) (\s@EndpointResponse' {} a -> s {metrics = a} :: EndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user who\'s associated with the endpoint has opted
-- out of receiving messages and push notifications from you. Possible
-- values are: ALL, the user has opted out and doesn\'t want to receive any
-- messages or push notifications; and, NONE, the user hasn\'t opted out
-- and wants to receive all messages and push notifications.
endpointResponse_optOut :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_optOut = Lens.lens (\EndpointResponse' {optOut} -> optOut) (\s@EndpointResponse' {} a -> s {optOut = a} :: EndpointResponse)

-- | The unique identifier for the most recent request to update the
-- endpoint.
endpointResponse_requestId :: Lens.Lens' EndpointResponse (Prelude.Maybe Prelude.Text)
endpointResponse_requestId = Lens.lens (\EndpointResponse' {requestId} -> requestId) (\s@EndpointResponse' {} a -> s {requestId = a} :: EndpointResponse)

-- | One or more custom user attributes that your app reports to Amazon
-- Pinpoint for the user who\'s associated with the endpoint.
endpointResponse_user :: Lens.Lens' EndpointResponse (Prelude.Maybe EndpointUser)
endpointResponse_user = Lens.lens (\EndpointResponse' {user} -> user) (\s@EndpointResponse' {} a -> s {user = a} :: EndpointResponse)

instance Data.FromJSON EndpointResponse where
  parseJSON =
    Data.withObject
      "EndpointResponse"
      ( \x ->
          EndpointResponse'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChannelType")
            Prelude.<*> (x Data..:? "CohortId")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Demographic")
            Prelude.<*> (x Data..:? "EffectiveDate")
            Prelude.<*> (x Data..:? "EndpointStatus")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OptOut")
            Prelude.<*> (x Data..:? "RequestId")
            Prelude.<*> (x Data..:? "User")
      )

instance Prelude.Hashable EndpointResponse where
  hashWithSalt _salt EndpointResponse' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` cohortId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` demographic
      `Prelude.hashWithSalt` effectiveDate
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` optOut
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` user

instance Prelude.NFData EndpointResponse where
  rnf EndpointResponse' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf cohortId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf demographic
      `Prelude.seq` Prelude.rnf effectiveDate
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf optOut
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf user
