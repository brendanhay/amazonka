{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PublicEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.PublicEndpoint
  ( PublicEndpoint (..)
  -- * Smart constructor
  , mkPublicEndpoint
  -- * Lenses
  , peAddress
  , peAttributes
  , peChannelType
  , peDemographic
  , peEffectiveDate
  , peEndpointStatus
  , peLocation
  , peMetrics
  , peOptOut
  , peRequestId
  , peUser
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelType as Types
import qualified Network.AWS.Pinpoint.Types.EndpointDemographic as Types
import qualified Network.AWS.Pinpoint.Types.EndpointLocation as Types
import qualified Network.AWS.Pinpoint.Types.EndpointUser as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the properties and attributes of an endpoint that's associated with an event.
--
-- /See:/ 'mkPublicEndpoint' smart constructor.
data PublicEndpoint = PublicEndpoint'
  { address :: Core.Maybe Core.Text
    -- ^ The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
  , attributes :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
  , channelType :: Core.Maybe Types.ChannelType
    -- ^ The channel that's used when sending messages or push notifications to the endpoint.
  , demographic :: Core.Maybe Types.EndpointDemographic
    -- ^ The demographic information for the endpoint, such as the time zone and platform.
  , effectiveDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the endpoint was last updated.
  , endpointStatus :: Core.Maybe Core.Text
    -- ^ Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
  , location :: Core.Maybe Types.EndpointLocation
    -- ^ The geographic information for the endpoint.
  , metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double)
    -- ^ One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
  , optOut :: Core.Maybe Core.Text
    -- ^ Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
  , requestId :: Core.Maybe Core.Text
    -- ^ A unique identifier that's generated each time the endpoint is updated.
  , user :: Core.Maybe Types.EndpointUser
    -- ^ One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicEndpoint' value with any optional fields omitted.
mkPublicEndpoint
    :: PublicEndpoint
mkPublicEndpoint
  = PublicEndpoint'{address = Core.Nothing,
                    attributes = Core.Nothing, channelType = Core.Nothing,
                    demographic = Core.Nothing, effectiveDate = Core.Nothing,
                    endpointStatus = Core.Nothing, location = Core.Nothing,
                    metrics = Core.Nothing, optOut = Core.Nothing,
                    requestId = Core.Nothing, user = Core.Nothing}

-- | The unique identifier for the recipient, such as a device token, email address, or mobile phone number.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peAddress :: Lens.Lens' PublicEndpoint (Core.Maybe Core.Text)
peAddress = Lens.field @"address"
{-# INLINEABLE peAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | One or more custom attributes that describe the endpoint by associating a name with an array of values. You can use these attributes as filter criteria when you create segments.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peAttributes :: Lens.Lens' PublicEndpoint (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
peAttributes = Lens.field @"attributes"
{-# INLINEABLE peAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The channel that's used when sending messages or push notifications to the endpoint.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peChannelType :: Lens.Lens' PublicEndpoint (Core.Maybe Types.ChannelType)
peChannelType = Lens.field @"channelType"
{-# INLINEABLE peChannelType #-}
{-# DEPRECATED channelType "Use generic-lens or generic-optics with 'channelType' instead"  #-}

-- | The demographic information for the endpoint, such as the time zone and platform.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peDemographic :: Lens.Lens' PublicEndpoint (Core.Maybe Types.EndpointDemographic)
peDemographic = Lens.field @"demographic"
{-# INLINEABLE peDemographic #-}
{-# DEPRECATED demographic "Use generic-lens or generic-optics with 'demographic' instead"  #-}

-- | The date and time, in ISO 8601 format, when the endpoint was last updated.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEffectiveDate :: Lens.Lens' PublicEndpoint (Core.Maybe Core.Text)
peEffectiveDate = Lens.field @"effectiveDate"
{-# INLINEABLE peEffectiveDate #-}
{-# DEPRECATED effectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead"  #-}

-- | Specifies whether to send messages or push notifications to the endpoint. Valid values are: ACTIVE, messages are sent to the endpoint; and, INACTIVE, messages aren’t sent to the endpoint.
--
-- Amazon Pinpoint automatically sets this value to ACTIVE when you create an endpoint or update an existing endpoint. Amazon Pinpoint automatically sets this value to INACTIVE if you update another endpoint that has the same address specified by the Address property.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEndpointStatus :: Lens.Lens' PublicEndpoint (Core.Maybe Core.Text)
peEndpointStatus = Lens.field @"endpointStatus"
{-# INLINEABLE peEndpointStatus #-}
{-# DEPRECATED endpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead"  #-}

-- | The geographic information for the endpoint.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peLocation :: Lens.Lens' PublicEndpoint (Core.Maybe Types.EndpointLocation)
peLocation = Lens.field @"location"
{-# INLINEABLE peLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | One or more custom metrics that your app reports to Amazon Pinpoint for the endpoint.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peMetrics :: Lens.Lens' PublicEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Double))
peMetrics = Lens.field @"metrics"
{-# INLINEABLE peMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | Specifies whether the user who's associated with the endpoint has opted out of receiving messages and push notifications from you. Possible values are: ALL, the user has opted out and doesn't want to receive any messages or push notifications; and, NONE, the user hasn't opted out and wants to receive all messages and push notifications.
--
-- /Note:/ Consider using 'optOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peOptOut :: Lens.Lens' PublicEndpoint (Core.Maybe Core.Text)
peOptOut = Lens.field @"optOut"
{-# INLINEABLE peOptOut #-}
{-# DEPRECATED optOut "Use generic-lens or generic-optics with 'optOut' instead"  #-}

-- | A unique identifier that's generated each time the endpoint is updated.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peRequestId :: Lens.Lens' PublicEndpoint (Core.Maybe Core.Text)
peRequestId = Lens.field @"requestId"
{-# INLINEABLE peRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | One or more custom user attributes that your app reports to Amazon Pinpoint for the user who's associated with the endpoint.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peUser :: Lens.Lens' PublicEndpoint (Core.Maybe Types.EndpointUser)
peUser = Lens.field @"user"
{-# INLINEABLE peUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

instance Core.FromJSON PublicEndpoint where
        toJSON PublicEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [("Address" Core..=) Core.<$> address,
                  ("Attributes" Core..=) Core.<$> attributes,
                  ("ChannelType" Core..=) Core.<$> channelType,
                  ("Demographic" Core..=) Core.<$> demographic,
                  ("EffectiveDate" Core..=) Core.<$> effectiveDate,
                  ("EndpointStatus" Core..=) Core.<$> endpointStatus,
                  ("Location" Core..=) Core.<$> location,
                  ("Metrics" Core..=) Core.<$> metrics,
                  ("OptOut" Core..=) Core.<$> optOut,
                  ("RequestId" Core..=) Core.<$> requestId,
                  ("User" Core..=) Core.<$> user])
