{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointResponse
  ( EndpointResponse (..)
  -- * Smart constructor
  , mkEndpointResponse
  -- * Lenses
  , erAddress
  , erApplicationId
  , erAttributes
  , erChannelType
  , erCohortId
  , erCreationDate
  , erDemographic
  , erEffectiveDate
  , erEndpointStatus
  , erId
  , erLocation
  , erMetrics
  , erOptOut
  , erRequestId
  , erUser
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelType as Types
import qualified Network.AWS.Pinpoint.Types.EndpointDemographic as Types
import qualified Network.AWS.Pinpoint.Types.EndpointLocation as Types
import qualified Network.AWS.Pinpoint.Types.EndpointUser as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the channel type and other settings for an endpoint.
--
-- /See:/ 'mkEndpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { address :: Core.Maybe Core.Text
    -- ^ The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that's associated with the endpoint.
  , attributes :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
  , channelType :: Core.Maybe Types.ChannelType
    -- ^ The channel that's used when sending messages or push notifications to the endpoint.
  , cohortId :: Core.Maybe Core.Text
    -- ^ A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the endpoint was created.
  , demographic :: Core.Maybe Types.EndpointDemographic
    -- ^ The demographic information for the endpoint, such as the time zone and platform.
  , effectiveDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the endpoint was last updated.
  , endpointStatus :: Core.Maybe Core.Text
    -- ^ Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
  , id :: Core.Maybe Core.Text
    -- ^ The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
  , location :: Core.Maybe Types.EndpointLocation
    -- ^ The geographic information for the endpoint.
  , metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double)
    -- ^ One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
  , optOut :: Core.Maybe Core.Text
    -- ^ Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
  , requestId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the most recent request to update the endpoint.
  , user :: Core.Maybe Types.EndpointUser
    -- ^ One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointResponse' value with any optional fields omitted.
mkEndpointResponse
    :: EndpointResponse
mkEndpointResponse
  = EndpointResponse'{address = Core.Nothing,
                      applicationId = Core.Nothing, attributes = Core.Nothing,
                      channelType = Core.Nothing, cohortId = Core.Nothing,
                      creationDate = Core.Nothing, demographic = Core.Nothing,
                      effectiveDate = Core.Nothing, endpointStatus = Core.Nothing,
                      id = Core.Nothing, location = Core.Nothing, metrics = Core.Nothing,
                      optOut = Core.Nothing, requestId = Core.Nothing,
                      user = Core.Nothing}

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For example, the address for a push-notification channel is typically the token provided by a push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. The address for the SMS channel is a phone number in E.164 format, such as +12065550100. The address for the email channel is an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAddress :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erAddress = Lens.field @"address"
{-# INLINEABLE erAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The unique identifier for the application that's associated with the endpoint.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erApplicationId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erApplicationId = Lens.field @"applicationId"
{-# INLINEABLE erApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAttributes :: Lens.Lens' EndpointResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
erAttributes = Lens.field @"attributes"
{-# INLINEABLE erAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The channel that's used when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erChannelType :: Lens.Lens' EndpointResponse (Core.Maybe Types.ChannelType)
erChannelType = Lens.field @"channelType"
{-# INLINEABLE erChannelType #-}
{-# DEPRECATED channelType "Use generic-lens or generic-optics with 'channelType' instead"  #-}

-- | A number from 0-99 that represents the cohort that the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an application. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for campaigns.
--
-- /Note:/ Consider using 'cohortId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erCohortId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erCohortId = Lens.field @"cohortId"
{-# INLINEABLE erCohortId #-}
{-# DEPRECATED cohortId "Use generic-lens or generic-optics with 'cohortId' instead"  #-}

-- | The date and time, in ISO 8601 format, when the endpoint was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erCreationDate :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erCreationDate = Lens.field @"creationDate"
{-# INLINEABLE erCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erDemographic :: Lens.Lens' EndpointResponse (Core.Maybe Types.EndpointDemographic)
erDemographic = Lens.field @"demographic"
{-# INLINEABLE erDemographic #-}
{-# DEPRECATED demographic "Use generic-lens or generic-optics with 'demographic' instead"  #-}

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEffectiveDate :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erEffectiveDate = Lens.field @"effectiveDate"
{-# INLINEABLE erEffectiveDate #-}
{-# DEPRECATED effectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead"  #-}

-- | Specifies whether messages or push notifications are sent to the endpoint. Possible values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEndpointStatus :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erEndpointStatus = Lens.field @"endpointStatus"
{-# INLINEABLE erEndpointStatus #-}
{-# DEPRECATED endpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead"  #-}

-- | The unique identifier that you assigned to the endpoint. The identifier should be a globally unique identifier (GUID) to ensure that it doesn't conflict with other endpoint identifiers that are associated with the application.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erId = Lens.field @"id"
{-# INLINEABLE erId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erLocation :: Lens.Lens' EndpointResponse (Core.Maybe Types.EndpointLocation)
erLocation = Lens.field @"location"
{-# INLINEABLE erLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMetrics :: Lens.Lens' EndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Double))
erMetrics = Lens.field @"metrics"
{-# INLINEABLE erMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erOptOut :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erOptOut = Lens.field @"optOut"
{-# INLINEABLE erOptOut #-}
{-# DEPRECATED optOut "Use generic-lens or generic-optics with 'optOut' instead"  #-}

-- | The unique identifier for the most recent request to update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRequestId :: Lens.Lens' EndpointResponse (Core.Maybe Core.Text)
erRequestId = Lens.field @"requestId"
{-# INLINEABLE erRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erUser :: Lens.Lens' EndpointResponse (Core.Maybe Types.EndpointUser)
erUser = Lens.field @"user"
{-# INLINEABLE erUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

instance Core.FromJSON EndpointResponse where
        parseJSON
          = Core.withObject "EndpointResponse" Core.$
              \ x ->
                EndpointResponse' Core.<$>
                  (x Core..:? "Address") Core.<*> x Core..:? "ApplicationId" Core.<*>
                    x Core..:? "Attributes"
                    Core.<*> x Core..:? "ChannelType"
                    Core.<*> x Core..:? "CohortId"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Demographic"
                    Core.<*> x Core..:? "EffectiveDate"
                    Core.<*> x Core..:? "EndpointStatus"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Location"
                    Core.<*> x Core..:? "Metrics"
                    Core.<*> x Core..:? "OptOut"
                    Core.<*> x Core..:? "RequestId"
                    Core.<*> x Core..:? "User"
