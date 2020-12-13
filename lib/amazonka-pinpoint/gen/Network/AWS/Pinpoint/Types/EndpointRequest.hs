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
    erRequestId,
    erMetrics,
    erLocation,
    erDemographic,
    erAddress,
    erEffectiveDate,
    erUser,
    erAttributes,
    erEndpointStatus,
    erOptOut,
    erChannelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Lude

-- | Specifies the channel type and other settings for an endpoint.
--
-- /See:/ 'mkEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { -- | The unique identifier for the most recent request to update the endpoint.
    requestId :: Lude.Maybe Lude.Text,
    -- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
    metrics :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    -- | The geographic information for the endpoint.
    location :: Lude.Maybe EndpointLocation,
    -- | The demographic information for the endpoint, such as the time zone and platform.
    demographic :: Lude.Maybe EndpointDemographic,
    -- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
    address :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO 8601 format, when the endpoint is updated.
    effectiveDate :: Lude.Maybe Lude.Text,
    -- | One or more custom attributes that describe the user who's associated with the endpoint.
    user :: Lude.Maybe EndpointUser,
    -- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
    --
    -- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
    --
    -- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
    endpointStatus :: Lude.Maybe Lude.Text,
    -- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
    optOut :: Lude.Maybe Lude.Text,
    -- | The channel to use when sending messages or push notifications to the endpoint.
    channelType :: Lude.Maybe ChannelType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- * 'requestId' - The unique identifier for the most recent request to update the endpoint.
-- * 'metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
-- * 'location' - The geographic information for the endpoint.
-- * 'demographic' - The demographic information for the endpoint, such as the time zone and platform.
-- * 'address' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
-- * 'effectiveDate' - The date and time, in ISO 8601 format, when the endpoint is updated.
-- * 'user' - One or more custom attributes that describe the user who's associated with the endpoint.
-- * 'attributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
-- * 'endpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
-- * 'optOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
-- * 'channelType' - The channel to use when sending messages or push notifications to the endpoint.
mkEndpointRequest ::
  EndpointRequest
mkEndpointRequest =
  EndpointRequest'
    { requestId = Lude.Nothing,
      metrics = Lude.Nothing,
      location = Lude.Nothing,
      demographic = Lude.Nothing,
      address = Lude.Nothing,
      effectiveDate = Lude.Nothing,
      user = Lude.Nothing,
      attributes = Lude.Nothing,
      endpointStatus = Lude.Nothing,
      optOut = Lude.Nothing,
      channelType = Lude.Nothing
    }

-- | The unique identifier for the most recent request to update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRequestId :: Lens.Lens' EndpointRequest (Lude.Maybe Lude.Text)
erRequestId = Lens.lens (requestId :: EndpointRequest -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: EndpointRequest)
{-# DEPRECATED erRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMetrics :: Lens.Lens' EndpointRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
erMetrics = Lens.lens (metrics :: EndpointRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {metrics = a} :: EndpointRequest)
{-# DEPRECATED erMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erLocation :: Lens.Lens' EndpointRequest (Lude.Maybe EndpointLocation)
erLocation = Lens.lens (location :: EndpointRequest -> Lude.Maybe EndpointLocation) (\s a -> s {location = a} :: EndpointRequest)
{-# DEPRECATED erLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erDemographic :: Lens.Lens' EndpointRequest (Lude.Maybe EndpointDemographic)
erDemographic = Lens.lens (demographic :: EndpointRequest -> Lude.Maybe EndpointDemographic) (\s a -> s {demographic = a} :: EndpointRequest)
{-# DEPRECATED erDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAddress :: Lens.Lens' EndpointRequest (Lude.Maybe Lude.Text)
erAddress = Lens.lens (address :: EndpointRequest -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: EndpointRequest)
{-# DEPRECATED erAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint is updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEffectiveDate :: Lens.Lens' EndpointRequest (Lude.Maybe Lude.Text)
erEffectiveDate = Lens.lens (effectiveDate :: EndpointRequest -> Lude.Maybe Lude.Text) (\s a -> s {effectiveDate = a} :: EndpointRequest)
{-# DEPRECATED erEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | One or more custom attributes that describe the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erUser :: Lens.Lens' EndpointRequest (Lude.Maybe EndpointUser)
erUser = Lens.lens (user :: EndpointRequest -> Lude.Maybe EndpointUser) (\s a -> s {user = a} :: EndpointRequest)
{-# DEPRECATED erUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAttributes :: Lens.Lens' EndpointRequest (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
erAttributes = Lens.lens (attributes :: EndpointRequest -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {attributes = a} :: EndpointRequest)
{-# DEPRECATED erAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEndpointStatus :: Lens.Lens' EndpointRequest (Lude.Maybe Lude.Text)
erEndpointStatus = Lens.lens (endpointStatus :: EndpointRequest -> Lude.Maybe Lude.Text) (\s a -> s {endpointStatus = a} :: EndpointRequest)
{-# DEPRECATED erEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erOptOut :: Lens.Lens' EndpointRequest (Lude.Maybe Lude.Text)
erOptOut = Lens.lens (optOut :: EndpointRequest -> Lude.Maybe Lude.Text) (\s a -> s {optOut = a} :: EndpointRequest)
{-# DEPRECATED erOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The channel to use when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erChannelType :: Lens.Lens' EndpointRequest (Lude.Maybe ChannelType)
erChannelType = Lens.lens (channelType :: EndpointRequest -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: EndpointRequest)
{-# DEPRECATED erChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RequestId" Lude..=) Lude.<$> requestId,
            ("Metrics" Lude..=) Lude.<$> metrics,
            ("Location" Lude..=) Lude.<$> location,
            ("Demographic" Lude..=) Lude.<$> demographic,
            ("Address" Lude..=) Lude.<$> address,
            ("EffectiveDate" Lude..=) Lude.<$> effectiveDate,
            ("User" Lude..=) Lude.<$> user,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("EndpointStatus" Lude..=) Lude.<$> endpointStatus,
            ("OptOut" Lude..=) Lude.<$> optOut,
            ("ChannelType" Lude..=) Lude.<$> channelType
          ]
      )
