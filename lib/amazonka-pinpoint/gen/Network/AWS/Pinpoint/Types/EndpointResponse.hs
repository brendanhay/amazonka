-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointResponse
  ( EndpointResponse (..),

    -- * Smart constructor
    mkEndpointResponse,

    -- * Lenses
    endRequestId,
    endMetrics,
    endLocation,
    endDemographic,
    endCohortId,
    endAddress,
    endEffectiveDate,
    endUser,
    endApplicationId,
    endAttributes,
    endEndpointStatus,
    endOptOut,
    endId,
    endCreationDate,
    endChannelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the channel type and other settings for an endpoint.
--
-- /See:/ 'mkEndpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { requestId ::
      Lude.Maybe Lude.Text,
    metrics ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    location :: Lude.Maybe EndpointLocation,
    demographic :: Lude.Maybe EndpointDemographic,
    cohortId :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
    effectiveDate :: Lude.Maybe Lude.Text,
    user :: Lude.Maybe EndpointUser,
    applicationId :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    endpointStatus :: Lude.Maybe Lude.Text,
    optOut :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    channelType :: Lude.Maybe ChannelType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointResponse' with the minimum fields required to make a request.
--
-- * 'address' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
-- * 'applicationId' - The unique identifier for the application that's associated with the endpoint.
-- * 'attributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
-- * 'channelType' - The channel that's used when sending messages or push notifications to the endpoint.
-- * 'cohortId' - A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the endpoint was created.
-- * 'demographic' - The demographic information for the endpoint, such as the time zone and platform.
-- * 'effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last updated.
-- * 'endpointStatus' - Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
-- * 'id' - The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
-- * 'location' - The geographic information for the endpoint.
-- * 'metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
-- * 'optOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
-- * 'requestId' - The unique identifier for the most recent request to update the endpoint.
-- * 'user' - One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
mkEndpointResponse ::
  EndpointResponse
mkEndpointResponse =
  EndpointResponse'
    { requestId = Lude.Nothing,
      metrics = Lude.Nothing,
      location = Lude.Nothing,
      demographic = Lude.Nothing,
      cohortId = Lude.Nothing,
      address = Lude.Nothing,
      effectiveDate = Lude.Nothing,
      user = Lude.Nothing,
      applicationId = Lude.Nothing,
      attributes = Lude.Nothing,
      endpointStatus = Lude.Nothing,
      optOut = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      channelType = Lude.Nothing
    }

-- | The unique identifier for the most recent request to update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endRequestId :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endRequestId = Lens.lens (requestId :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: EndpointResponse)
{-# DEPRECATED endRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endMetrics :: Lens.Lens' EndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
endMetrics = Lens.lens (metrics :: EndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {metrics = a} :: EndpointResponse)
{-# DEPRECATED endMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endLocation :: Lens.Lens' EndpointResponse (Lude.Maybe EndpointLocation)
endLocation = Lens.lens (location :: EndpointResponse -> Lude.Maybe EndpointLocation) (\s a -> s {location = a} :: EndpointResponse)
{-# DEPRECATED endLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endDemographic :: Lens.Lens' EndpointResponse (Lude.Maybe EndpointDemographic)
endDemographic = Lens.lens (demographic :: EndpointResponse -> Lude.Maybe EndpointDemographic) (\s a -> s {demographic = a} :: EndpointResponse)
{-# DEPRECATED endDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
--
-- /Note:/ Consider using 'cohortId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endCohortId :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endCohortId = Lens.lens (cohortId :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {cohortId = a} :: EndpointResponse)
{-# DEPRECATED endCohortId "Use generic-lens or generic-optics with 'cohortId' instead." #-}

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endAddress :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endAddress = Lens.lens (address :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: EndpointResponse)
{-# DEPRECATED endAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endEffectiveDate :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endEffectiveDate = Lens.lens (effectiveDate :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {effectiveDate = a} :: EndpointResponse)
{-# DEPRECATED endEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endUser :: Lens.Lens' EndpointResponse (Lude.Maybe EndpointUser)
endUser = Lens.lens (user :: EndpointResponse -> Lude.Maybe EndpointUser) (\s a -> s {user = a} :: EndpointResponse)
{-# DEPRECATED endUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The unique identifier for the application that's associated with the endpoint.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endApplicationId :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endApplicationId = Lens.lens (applicationId :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: EndpointResponse)
{-# DEPRECATED endApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endAttributes :: Lens.Lens' EndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
endAttributes = Lens.lens (attributes :: EndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {attributes = a} :: EndpointResponse)
{-# DEPRECATED endAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endEndpointStatus :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endEndpointStatus = Lens.lens (endpointStatus :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointStatus = a} :: EndpointResponse)
{-# DEPRECATED endEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endOptOut :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endOptOut = Lens.lens (optOut :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {optOut = a} :: EndpointResponse)
{-# DEPRECATED endOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endId :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endId = Lens.lens (id :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: EndpointResponse)
{-# DEPRECATED endId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endCreationDate :: Lens.Lens' EndpointResponse (Lude.Maybe Lude.Text)
endCreationDate = Lens.lens (creationDate :: EndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: EndpointResponse)
{-# DEPRECATED endCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The channel that's used when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
endChannelType :: Lens.Lens' EndpointResponse (Lude.Maybe ChannelType)
endChannelType = Lens.lens (channelType :: EndpointResponse -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: EndpointResponse)
{-# DEPRECATED endChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.FromJSON EndpointResponse where
  parseJSON =
    Lude.withObject
      "EndpointResponse"
      ( \x ->
          EndpointResponse'
            Lude.<$> (x Lude..:? "RequestId")
            Lude.<*> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Location")
            Lude.<*> (x Lude..:? "Demographic")
            Lude.<*> (x Lude..:? "CohortId")
            Lude.<*> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "EffectiveDate")
            Lude.<*> (x Lude..:? "User")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EndpointStatus")
            Lude.<*> (x Lude..:? "OptOut")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "ChannelType")
      )
