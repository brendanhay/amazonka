{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeConfigurationSet
    (
    -- * Creating a request
      DescribeConfigurationSet (..)
    , mkDescribeConfigurationSet
    -- ** Request lenses
    , dcsConfigurationSetName
    , dcsConfigurationSetAttributeNames

    -- * Destructuring the response
    , DescribeConfigurationSetResponse (..)
    , mkDescribeConfigurationSetResponse
    -- ** Response lenses
    , dcsrrsConfigurationSet
    , dcsrrsDeliveryOptions
    , dcsrrsEventDestinations
    , dcsrrsReputationOptions
    , dcsrrsTrackingOptions
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeConfigurationSet' smart constructor.
data DescribeConfigurationSet = DescribeConfigurationSet'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set to describe.
  , configurationSetAttributeNames :: Core.Maybe [Types.ConfigurationSetAttribute]
    -- ^ A list of configuration set attributes to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationSet' value with any optional fields omitted.
mkDescribeConfigurationSet
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> DescribeConfigurationSet
mkDescribeConfigurationSet configurationSetName
  = DescribeConfigurationSet'{configurationSetName,
                              configurationSetAttributeNames = Core.Nothing}

-- | The name of the configuration set to describe.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigurationSetName :: Lens.Lens' DescribeConfigurationSet Types.ConfigurationSetName
dcsConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE dcsConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | A list of configuration set attributes to return.
--
-- /Note:/ Consider using 'configurationSetAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigurationSetAttributeNames :: Lens.Lens' DescribeConfigurationSet (Core.Maybe [Types.ConfigurationSetAttribute])
dcsConfigurationSetAttributeNames = Lens.field @"configurationSetAttributeNames"
{-# INLINEABLE dcsConfigurationSetAttributeNames #-}
{-# DEPRECATED configurationSetAttributeNames "Use generic-lens or generic-optics with 'configurationSetAttributeNames' instead"  #-}

instance Core.ToQuery DescribeConfigurationSet where
        toQuery DescribeConfigurationSet{..}
          = Core.toQueryPair "Action"
              ("DescribeConfigurationSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<>
              Core.toQueryPair "ConfigurationSetAttributeNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   configurationSetAttributeNames)

instance Core.ToHeaders DescribeConfigurationSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeConfigurationSet where
        type Rs DescribeConfigurationSet = DescribeConfigurationSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeConfigurationSetResult"
              (\ s h x ->
                 DescribeConfigurationSetResponse' Core.<$>
                   (x Core..@? "ConfigurationSet") Core.<*>
                     x Core..@? "DeliveryOptions"
                     Core.<*>
                     x Core..@? "EventDestinations" Core..<@> Core.parseXMLList "member"
                     Core.<*> x Core..@? "ReputationOptions"
                     Core.<*> x Core..@? "TrackingOptions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeConfigurationSetResponse' smart constructor.
data DescribeConfigurationSetResponse = DescribeConfigurationSetResponse'
  { configurationSet :: Core.Maybe Types.ConfigurationSet
    -- ^ The configuration set object associated with the specified configuration set.
  , deliveryOptions :: Core.Maybe Types.DeliveryOptions
  , eventDestinations :: Core.Maybe [Types.EventDestination]
    -- ^ A list of event destinations associated with the configuration set. 
  , reputationOptions :: Core.Maybe Types.ReputationOptions
    -- ^ An object that represents the reputation settings for the configuration set. 
  , trackingOptions :: Core.Maybe Types.TrackingOptions
    -- ^ The name of the custom open and click tracking domain associated with the configuration set.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConfigurationSetResponse' value with any optional fields omitted.
mkDescribeConfigurationSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigurationSetResponse
mkDescribeConfigurationSetResponse responseStatus
  = DescribeConfigurationSetResponse'{configurationSet =
                                        Core.Nothing,
                                      deliveryOptions = Core.Nothing,
                                      eventDestinations = Core.Nothing,
                                      reputationOptions = Core.Nothing,
                                      trackingOptions = Core.Nothing, responseStatus}

-- | The configuration set object associated with the specified configuration set.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsConfigurationSet :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe Types.ConfigurationSet)
dcsrrsConfigurationSet = Lens.field @"configurationSet"
{-# INLINEABLE dcsrrsConfigurationSet #-}
{-# DEPRECATED configurationSet "Use generic-lens or generic-optics with 'configurationSet' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'deliveryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsDeliveryOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe Types.DeliveryOptions)
dcsrrsDeliveryOptions = Lens.field @"deliveryOptions"
{-# INLINEABLE dcsrrsDeliveryOptions #-}
{-# DEPRECATED deliveryOptions "Use generic-lens or generic-optics with 'deliveryOptions' instead"  #-}

-- | A list of event destinations associated with the configuration set. 
--
-- /Note:/ Consider using 'eventDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsEventDestinations :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe [Types.EventDestination])
dcsrrsEventDestinations = Lens.field @"eventDestinations"
{-# INLINEABLE dcsrrsEventDestinations #-}
{-# DEPRECATED eventDestinations "Use generic-lens or generic-optics with 'eventDestinations' instead"  #-}

-- | An object that represents the reputation settings for the configuration set. 
--
-- /Note:/ Consider using 'reputationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsReputationOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe Types.ReputationOptions)
dcsrrsReputationOptions = Lens.field @"reputationOptions"
{-# INLINEABLE dcsrrsReputationOptions #-}
{-# DEPRECATED reputationOptions "Use generic-lens or generic-optics with 'reputationOptions' instead"  #-}

-- | The name of the custom open and click tracking domain associated with the configuration set.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsTrackingOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe Types.TrackingOptions)
dcsrrsTrackingOptions = Lens.field @"trackingOptions"
{-# INLINEABLE dcsrrsTrackingOptions #-}
{-# DEPRECATED trackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DescribeConfigurationSetResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
