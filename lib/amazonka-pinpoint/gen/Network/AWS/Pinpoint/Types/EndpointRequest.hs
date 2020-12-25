{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointRequest
  ( EndpointRequest (..),

    -- * Smart constructor
    mkEndpointRequest,

    -- * Lenses
    erfAddress,
    erfAttributes,
    erfChannelType,
    erfDemographic,
    erfEffectiveDate,
    erfEndpointStatus,
    erfLocation,
    erfMetrics,
    erfOptOut,
    erfRequestId,
    erfUser,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelType as Types
import qualified Network.AWS.Pinpoint.Types.EndpointDemographic as Types
import qualified Network.AWS.Pinpoint.Types.EndpointLocation as Types
import qualified Network.AWS.Pinpoint.Types.EndpointUser as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the channel type and other settings for an endpoint.
--
-- /See:/ 'mkEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { -- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
    address :: Core.Maybe Core.Text,
    -- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
    --
    -- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
    attributes :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The channel to use when sending messages or push notifications to the endpoint.
    channelType :: Core.Maybe Types.ChannelType,
    -- | The demographic information for the endpoint, such as the time zone and platform.
    demographic :: Core.Maybe Types.EndpointDemographic,
    -- | The date and time, in ISO 8601 format, when the endpoint is updated.
    effectiveDate :: Core.Maybe Core.Text,
    -- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
    endpointStatus :: Core.Maybe Core.Text,
    -- | The geographic information for the endpoint.
    location :: Core.Maybe Types.EndpointLocation,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
    metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double),
    -- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
    optOut :: Core.Maybe Core.Text,
    -- | The unique identifier for the most recent request to update the endpoint.
    requestId :: Core.Maybe Core.Text,
    -- | One or more custom attributes that describe the user who's associated with the endpoint.
    user :: Core.Maybe Types.EndpointUser
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointRequest' value with any optional fields omitted.
mkEndpointRequest ::
  EndpointRequest
mkEndpointRequest =
  EndpointRequest'
    { address = Core.Nothing,
      attributes = Core.Nothing,
      channelType = Core.Nothing,
      demographic = Core.Nothing,
      effectiveDate = Core.Nothing,
      endpointStatus = Core.Nothing,
      location = Core.Nothing,
      metrics = Core.Nothing,
      optOut = Core.Nothing,
      requestId = Core.Nothing,
      user = Core.Nothing
    }

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfAddress :: Lens.Lens' EndpointRequest (Core.Maybe Core.Text)
erfAddress = Lens.field @"address"
{-# DEPRECATED erfAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfAttributes :: Lens.Lens' EndpointRequest (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
erfAttributes = Lens.field @"attributes"
{-# DEPRECATED erfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The channel to use when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfChannelType :: Lens.Lens' EndpointRequest (Core.Maybe Types.ChannelType)
erfChannelType = Lens.field @"channelType"
{-# DEPRECATED erfChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfDemographic :: Lens.Lens' EndpointRequest (Core.Maybe Types.EndpointDemographic)
erfDemographic = Lens.field @"demographic"
{-# DEPRECATED erfDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint is updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfEffectiveDate :: Lens.Lens' EndpointRequest (Core.Maybe Core.Text)
erfEffectiveDate = Lens.field @"effectiveDate"
{-# DEPRECATED erfEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfEndpointStatus :: Lens.Lens' EndpointRequest (Core.Maybe Core.Text)
erfEndpointStatus = Lens.field @"endpointStatus"
{-# DEPRECATED erfEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfLocation :: Lens.Lens' EndpointRequest (Core.Maybe Types.EndpointLocation)
erfLocation = Lens.field @"location"
{-# DEPRECATED erfLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfMetrics :: Lens.Lens' EndpointRequest (Core.Maybe (Core.HashMap Core.Text Core.Double))
erfMetrics = Lens.field @"metrics"
{-# DEPRECATED erfMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfOptOut :: Lens.Lens' EndpointRequest (Core.Maybe Core.Text)
erfOptOut = Lens.field @"optOut"
{-# DEPRECATED erfOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The unique identifier for the most recent request to update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfRequestId :: Lens.Lens' EndpointRequest (Core.Maybe Core.Text)
erfRequestId = Lens.field @"requestId"
{-# DEPRECATED erfRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom attributes that describe the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfUser :: Lens.Lens' EndpointRequest (Core.Maybe Types.EndpointUser)
erfUser = Lens.field @"user"
{-# DEPRECATED erfUser "Use generic-lens or generic-optics with 'user' instead." #-}

instance Core.FromJSON EndpointRequest where
  toJSON EndpointRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("Address" Core..=) Core.<$> address,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ChannelType" Core..=) Core.<$> channelType,
            ("Demographic" Core..=) Core.<$> demographic,
            ("EffectiveDate" Core..=) Core.<$> effectiveDate,
            ("EndpointStatus" Core..=) Core.<$> endpointStatus,
            ("Location" Core..=) Core.<$> location,
            ("Metrics" Core..=) Core.<$> metrics,
            ("OptOut" Core..=) Core.<$> optOut,
            ("RequestId" Core..=) Core.<$> requestId,
            ("User" Core..=) Core.<$> user
          ]
      )
