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
    ebiRequestId,
    ebiMetrics,
    ebiLocation,
    ebiDemographic,
    ebiAddress,
    ebiEffectiveDate,
    ebiUser,
    ebiAttributes,
    ebiEndpointStatus,
    ebiOptOut,
    ebiId,
    ebiChannelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Lude

-- | Specifies an endpoint to create or update and the settings and attributes to set or change for the endpoint.
--
-- /See:/ 'mkEndpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
  { requestId ::
      Lude.Maybe Lude.Text,
    metrics ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    location :: Lude.Maybe EndpointLocation,
    demographic :: Lude.Maybe EndpointDemographic,
    address :: Lude.Maybe Lude.Text,
    effectiveDate :: Lude.Maybe Lude.Text,
    user :: Lude.Maybe EndpointUser,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    endpointStatus :: Lude.Maybe Lude.Text,
    optOut :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'EndpointBatchItem' with the minimum fields required to make a request.
--
-- * 'address' - The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
-- * 'attributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
-- * 'channelType' - The channel to use when sending messages or push notifications to the endpoint.
-- * 'demographic' - The demographic information for the endpoint, such as the time zone and platform.
-- * 'effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was created or updated.
-- * 'endpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
-- * 'id' - The unique identifier for the endpoint in the context of the batch.
-- * 'location' - The geographic information for the endpoint.
-- * 'metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
-- * 'optOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
-- * 'requestId' - The unique identifier for the request to create or update the endpoint.
-- * 'user' - One or more custom attributes that describe the user who's associated with the endpoint.
mkEndpointBatchItem ::
  EndpointBatchItem
mkEndpointBatchItem =
  EndpointBatchItem'
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
      id = Lude.Nothing,
      channelType = Lude.Nothing
    }

-- | The unique identifier for the request to create or update the endpoint.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiRequestId :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiRequestId = Lens.lens (requestId :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: EndpointBatchItem)
{-# DEPRECATED ebiRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiMetrics :: Lens.Lens' EndpointBatchItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
ebiMetrics = Lens.lens (metrics :: EndpointBatchItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {metrics = a} :: EndpointBatchItem)
{-# DEPRECATED ebiMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiLocation :: Lens.Lens' EndpointBatchItem (Lude.Maybe EndpointLocation)
ebiLocation = Lens.lens (location :: EndpointBatchItem -> Lude.Maybe EndpointLocation) (\s a -> s {location = a} :: EndpointBatchItem)
{-# DEPRECATED ebiLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiDemographic :: Lens.Lens' EndpointBatchItem (Lude.Maybe EndpointDemographic)
ebiDemographic = Lens.lens (demographic :: EndpointBatchItem -> Lude.Maybe EndpointDemographic) (\s a -> s {demographic = a} :: EndpointBatchItem)
{-# DEPRECATED ebiDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | The destination address for messages or push notifications that you send to the endpoint. The address varies by channel. For a push-notification channel, use the token provided by the push notification service, such as an Apple Push Notification service (APNs) device token or a Firebase Cloud Messaging (FCM) registration token. For the SMS channel, use a phone number in E.164 format, such as +12065550100. For the email channel, use an email address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiAddress :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiAddress = Lens.lens (address :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: EndpointBatchItem)
{-# DEPRECATED ebiAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint was created or updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiEffectiveDate :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiEffectiveDate = Lens.lens (effectiveDate :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {effectiveDate = a} :: EndpointBatchItem)
{-# DEPRECATED ebiEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | One or more custom attributes that describe the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiUser :: Lens.Lens' EndpointBatchItem (Lude.Maybe EndpointUser)
ebiUser = Lens.lens (user :: EndpointBatchItem -> Lude.Maybe EndpointUser) (\s a -> s {user = a} :: EndpointBatchItem)
{-# DEPRECATED ebiUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. For example, the value of a custom attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiAttributes :: Lens.Lens' EndpointBatchItem (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
ebiAttributes = Lens.lens (attributes :: EndpointBatchItem -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {attributes = a} :: EndpointBatchItem)
{-# DEPRECATED ebiAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiEndpointStatus :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiEndpointStatus = Lens.lens (endpointStatus :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {endpointStatus = a} :: EndpointBatchItem)
{-# DEPRECATED ebiEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiOptOut :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiOptOut = Lens.lens (optOut :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {optOut = a} :: EndpointBatchItem)
{-# DEPRECATED ebiOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The unique identifier for the endpoint in the context of the batch.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiId :: Lens.Lens' EndpointBatchItem (Lude.Maybe Lude.Text)
ebiId = Lens.lens (id :: EndpointBatchItem -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: EndpointBatchItem)
{-# DEPRECATED ebiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The channel to use when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebiChannelType :: Lens.Lens' EndpointBatchItem (Lude.Maybe ChannelType)
ebiChannelType = Lens.lens (channelType :: EndpointBatchItem -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: EndpointBatchItem)
{-# DEPRECATED ebiChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.ToJSON EndpointBatchItem where
  toJSON EndpointBatchItem' {..} =
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
            ("Id" Lude..=) Lude.<$> id,
            ("ChannelType" Lude..=) Lude.<$> channelType
          ]
      )
