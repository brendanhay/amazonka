{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteConfigurationSetEventDestination
    (
    -- * Creating a request
      DeleteConfigurationSetEventDestination (..)
    , mkDeleteConfigurationSetEventDestination
    -- ** Request lenses
    , dcsedConfigurationSetName
    , dcsedEventDestinationName

    -- * Destructuring the response
    , DeleteConfigurationSetEventDestinationResponse (..)
    , mkDeleteConfigurationSetEventDestinationResponse
    -- ** Response lenses
    , dcsedrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set from which to delete the event destination.
  , eventDestinationName :: Types.EventDestinationName
    -- ^ The name of the event destination to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetEventDestination' value with any optional fields omitted.
mkDeleteConfigurationSetEventDestination
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> Types.EventDestinationName -- ^ 'eventDestinationName'
    -> DeleteConfigurationSetEventDestination
mkDeleteConfigurationSetEventDestination configurationSetName
  eventDestinationName
  = DeleteConfigurationSetEventDestination'{configurationSetName,
                                            eventDestinationName}

-- | The name of the configuration set from which to delete the event destination.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedConfigurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Types.ConfigurationSetName
dcsedConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE dcsedConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | The name of the event destination to delete.
--
-- /Note:/ Consider using 'eventDestinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedEventDestinationName :: Lens.Lens' DeleteConfigurationSetEventDestination Types.EventDestinationName
dcsedEventDestinationName = Lens.field @"eventDestinationName"
{-# INLINEABLE dcsedEventDestinationName #-}
{-# DEPRECATED eventDestinationName "Use generic-lens or generic-optics with 'eventDestinationName' instead"  #-}

instance Core.ToQuery DeleteConfigurationSetEventDestination where
        toQuery DeleteConfigurationSetEventDestination{..}
          = Core.toQueryPair "Action"
              ("DeleteConfigurationSetEventDestination" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<>
              Core.toQueryPair "EventDestinationName" eventDestinationName

instance Core.ToHeaders DeleteConfigurationSetEventDestination
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteConfigurationSetEventDestination
         where
        type Rs DeleteConfigurationSetEventDestination =
             DeleteConfigurationSetEventDestinationResponse
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
          = Response.receiveXMLWrapper
              "DeleteConfigurationSetEventDestinationResult"
              (\ s h x ->
                 DeleteConfigurationSetEventDestinationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetEventDestinationResponse' smart constructor.
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetEventDestinationResponse' value with any optional fields omitted.
mkDeleteConfigurationSetEventDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConfigurationSetEventDestinationResponse
mkDeleteConfigurationSetEventDestinationResponse responseStatus
  = DeleteConfigurationSetEventDestinationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedrrsResponseStatus :: Lens.Lens' DeleteConfigurationSetEventDestinationResponse Core.Int
dcsedrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsedrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
