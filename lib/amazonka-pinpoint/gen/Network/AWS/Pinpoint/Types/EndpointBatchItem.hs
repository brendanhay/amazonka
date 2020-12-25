{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointBatchItem
  ( EndpointBatchItem (..),

    -- * Smart constructor
    mkEndpointBatchItem,

    -- * Lenses
    ebiAddress,
    ebiAttributes,
    ebiChannelType,
    ebiDemographic,
    ebiEffectiveDate,
    ebiEndpointStatus,
    ebiId,
    ebiLocation,
    ebiMetrics,
    ebiOptOut,
    ebiRequestId,
    ebiUser,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelType as Types
import qualified Network.AWS.Pinpoint.Types.EndpointDemographic as Types
import qualified Network.AWS.Pinpoint.Types.EndpointLocation as Types
import qualified Network.AWS.Pinpoint.Types.EndpointUser as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies an endpoint to create or update and the settings and attributes to set or change for the endpoint.
--
-- /See:/ 'mkEndpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
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
    -- | The date and time, in ISO 8601 format, when the endpoint was created or updated.
    effectiveDate :: Core.Maybe Core.Text,
    -- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
    endpointStatus :: Core.Maybe Core.Text,
    -- | The unique identifier for the endpoint in the context of the batch.
    id :: Core.Maybe Core.Text,
    -- | The geographic information for the endpoint.
    location :: Core.Maybe Types.EndpointLocation,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
    metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double),
    -- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
    optOut :: Core.Maybe Core.Text,
    -- | The unique identifier for the request to create or update the endpoint.
    requestId :: Core.Maybe Core.Text,
    -- | One or more custom attributes that describe the user who's associated with the endpoint.
    user :: Core.Maybe Types.EndpointUser
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointBatchItem' value with any optional fields omitted.
mkEndpointBatchItem ::
  EndpointBatchItem
mkEndpointBatchItem =
  EndpointBatchItem'
    { address = Core.Nothing,
      attributes = Core.Nothing,
      channelType = Core.Nothing,
      demographic = Core.Nothing,
      effectiveDate = Core.Nothing,
      endpointStatus = Core.Nothing,
      id = Core.Nothing,
      location = Core.Nothing,
      metrics = Core.Nothing,
      optOut = Core.Nothing,
      requestId = Core.Nothing,
      user = Core.Nothing
    }

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiAddress :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiAddress = Lens.field @"address"
{-# DEPRECATED ebiAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiAttributes :: Lens.Lens' EndpointBatchItem (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
ebiAttributes = Lens.field @"attributes"
{-# DEPRECATED ebiAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The channel to use when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiChannelType :: Lens.Lens' EndpointBatchItem (Core.Maybe Types.ChannelType)
ebiChannelType = Lens.field @"channelType"
{-# DEPRECATED ebiChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiDemographic :: Lens.Lens' EndpointBatchItem (Core.Maybe Types.EndpointDemographic)
ebiDemographic = Lens.field @"demographic"
{-# DEPRECATED ebiDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint was created or updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiEffectiveDate :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiEffectiveDate = Lens.field @"effectiveDate"
{-# DEPRECATED ebiEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiEndpointStatus :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiEndpointStatus = Lens.field @"endpointStatus"
{-# DEPRECATED ebiEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | The unique identifier for the endpoint in the context of the batch.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiId :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiId = Lens.field @"id"
{-# DEPRECATED ebiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiLocation :: Lens.Lens' EndpointBatchItem (Core.Maybe Types.EndpointLocation)
ebiLocation = Lens.field @"location"
{-# DEPRECATED ebiLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiMetrics :: Lens.Lens' EndpointBatchItem (Core.Maybe (Core.HashMap Core.Text Core.Double))
ebiMetrics = Lens.field @"metrics"
{-# DEPRECATED ebiMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiOptOut :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiOptOut = Lens.field @"optOut"
{-# DEPRECATED ebiOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The unique identifier for the request to create or update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiRequestId :: Lens.Lens' EndpointBatchItem (Core.Maybe Core.Text)
ebiRequestId = Lens.field @"requestId"
{-# DEPRECATED ebiRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom attributes that describe the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiUser :: Lens.Lens' EndpointBatchItem (Core.Maybe Types.EndpointUser)
ebiUser = Lens.field @"user"
{-# DEPRECATED ebiUser "Use generic-lens or generic-optics with 'user' instead." #-}

instance Core.FromJSON EndpointBatchItem where
  toJSON EndpointBatchItem {..} =
    Core.object
      ( Core.catMaybes
          [ ("Address" Core..=) Core.<$> address,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ChannelType" Core..=) Core.<$> channelType,
            ("Demographic" Core..=) Core.<$> demographic,
            ("EffectiveDate" Core..=) Core.<$> effectiveDate,
            ("EndpointStatus" Core..=) Core.<$> endpointStatus,
            ("Id" Core..=) Core.<$> id,
            ("Location" Core..=) Core.<$> location,
            ("Metrics" Core..=) Core.<$> metrics,
            ("OptOut" Core..=) Core.<$> optOut,
            ("RequestId" Core..=) Core.<$> requestId,
            ("User" Core..=) Core.<$> user
          ]
      )
