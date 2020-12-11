-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PublicEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PublicEndpoint
  ( PublicEndpoint (..),

    -- * Smart constructor
    mkPublicEndpoint,

    -- * Lenses
    peRequestId,
    peMetrics,
    peLocation,
    peDemographic,
    peAddress,
    peEffectiveDate,
    peUser,
    peAttributes,
    peEndpointStatus,
    peOptOut,
    peChannelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointUser
import qualified Network.AWS.Prelude as Lude

-- | Specifies the properties and attributes of an endpoint that's associated with an event.
--
-- /See:/ 'mkPublicEndpoint' smart constructor.
data PublicEndpoint = PublicEndpoint'
  { requestId ::
      Lude.Maybe Lude.Text,
    metrics :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    location :: Lude.Maybe EndpointLocation,
    demographic :: Lude.Maybe EndpointDemographic,
    address :: Lude.Maybe Lude.Text,
    effectiveDate :: Lude.Maybe Lude.Text,
    user :: Lude.Maybe EndpointUser,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    endpointStatus :: Lude.Maybe Lude.Text,
    optOut :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PublicEndpoint' with the minimum fields required to make a request.
--
-- * 'address' - The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
-- * 'attributes' - One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
-- * 'channelType' - The channel that's used when sending messages or push notifications to the endpoint.
-- * 'demographic' - The demographic information for the endpoint, such as the time zone and platform.
-- * 'effectiveDate' - The date and time, in ISO 8601 format, when the endpoint was last updated.
-- * 'endpointStatus' - Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
-- * 'location' - The geographic information for the endpoint.
-- * 'metrics' - One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
-- * 'optOut' - Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
-- * 'requestId' - A unique identifier that's generated each time the endpoint is updated.
-- * 'user' - One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
mkPublicEndpoint ::
  PublicEndpoint
mkPublicEndpoint =
  PublicEndpoint'
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

-- | A unique identifier that's generated each time the endpoint is updated.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peRequestId :: Lens.Lens' PublicEndpoint (Lude.Maybe Lude.Text)
peRequestId = Lens.lens (requestId :: PublicEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: PublicEndpoint)
{-# DEPRECATED peRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peMetrics :: Lens.Lens' PublicEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
peMetrics = Lens.lens (metrics :: PublicEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {metrics = a} :: PublicEndpoint)
{-# DEPRECATED peMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peLocation :: Lens.Lens' PublicEndpoint (Lude.Maybe EndpointLocation)
peLocation = Lens.lens (location :: PublicEndpoint -> Lude.Maybe EndpointLocation) (\s a -> s {location = a} :: PublicEndpoint)
{-# DEPRECATED peLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peDemographic :: Lens.Lens' PublicEndpoint (Lude.Maybe EndpointDemographic)
peDemographic = Lens.lens (demographic :: PublicEndpoint -> Lude.Maybe EndpointDemographic) (\s a -> s {demographic = a} :: PublicEndpoint)
{-# DEPRECATED peDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peAddress :: Lens.Lens' PublicEndpoint (Lude.Maybe Lude.Text)
peAddress = Lens.lens (address :: PublicEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: PublicEndpoint)
{-# DEPRECATED peAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEffectiveDate :: Lens.Lens' PublicEndpoint (Lude.Maybe Lude.Text)
peEffectiveDate = Lens.lens (effectiveDate :: PublicEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {effectiveDate = a} :: PublicEndpoint)
{-# DEPRECATED peEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peUser :: Lens.Lens' PublicEndpoint (Lude.Maybe EndpointUser)
peUser = Lens.lens (user :: PublicEndpoint -> Lude.Maybe EndpointUser) (\s a -> s {user = a} :: PublicEndpoint)
{-# DEPRECATED peUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peAttributes :: Lens.Lens' PublicEndpoint (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
peAttributes = Lens.lens (attributes :: PublicEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {attributes = a} :: PublicEndpoint)
{-# DEPRECATED peAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEndpointStatus :: Lens.Lens' PublicEndpoint (Lude.Maybe Lude.Text)
peEndpointStatus = Lens.lens (endpointStatus :: PublicEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointStatus = a} :: PublicEndpoint)
{-# DEPRECATED peEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peOptOut :: Lens.Lens' PublicEndpoint (Lude.Maybe Lude.Text)
peOptOut = Lens.lens (optOut :: PublicEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {optOut = a} :: PublicEndpoint)
{-# DEPRECATED peOptOut "Use generic-lens or generic-optics with 'optOut' instead." #-}

-- | The channel that's used when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peChannelType :: Lens.Lens' PublicEndpoint (Lude.Maybe ChannelType)
peChannelType = Lens.lens (channelType :: PublicEndpoint -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: PublicEndpoint)
{-# DEPRECATED peChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.ToJSON PublicEndpoint where
  toJSON PublicEndpoint' {..} =
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
