{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteConfigurationSetEventDestination (..),
    mkDeleteConfigurationSetEventDestination,

    -- ** Request lenses
    dcsedConfigurationSetName,
    dcsedEventDestinationName,

    -- * Destructuring the response
    DeleteConfigurationSetEventDestinationResponse (..),
    mkDeleteConfigurationSetEventDestinationResponse,

    -- ** Response lenses
    dcsedrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { -- | The name of the configuration set from which to delete the event destination.
    configurationSetName :: Types.ConfigurationSetName,
    -- | The name of the event destination to delete.
    eventDestinationName :: Types.EventDestinationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetEventDestination' value with any optional fields omitted.
mkDeleteConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Types.ConfigurationSetName ->
  -- | 'eventDestinationName'
  Types.EventDestinationName ->
  DeleteConfigurationSetEventDestination
mkDeleteConfigurationSetEventDestination
  configurationSetName
  eventDestinationName =
    DeleteConfigurationSetEventDestination'
      { configurationSetName,
        eventDestinationName
      }

-- | The name of the configuration set from which to delete the event destination.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedConfigurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Types.ConfigurationSetName
dcsedConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED dcsedConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | The name of the event destination to delete.
--
-- /Note:/ Consider using 'eventDestinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedEventDestinationName :: Lens.Lens' DeleteConfigurationSetEventDestination Types.EventDestinationName
dcsedEventDestinationName = Lens.field @"eventDestinationName"
{-# DEPRECATED dcsedEventDestinationName "Use generic-lens or generic-optics with 'eventDestinationName' instead." #-}

instance Core.AWSRequest DeleteConfigurationSetEventDestination where
  type
    Rs DeleteConfigurationSetEventDestination =
      DeleteConfigurationSetEventDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteConfigurationSetEventDestination")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ConfigurationSetName" configurationSetName)
                Core.<> (Core.toQueryValue "EventDestinationName" eventDestinationName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteConfigurationSetEventDestinationResult"
      ( \s h x ->
          DeleteConfigurationSetEventDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetEventDestinationResponse' smart constructor.
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetEventDestinationResponse' value with any optional fields omitted.
mkDeleteConfigurationSetEventDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConfigurationSetEventDestinationResponse
mkDeleteConfigurationSetEventDestinationResponse responseStatus =
  DeleteConfigurationSetEventDestinationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedrrsResponseStatus :: Lens.Lens' DeleteConfigurationSetEventDestinationResponse Core.Int
dcsedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
