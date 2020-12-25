{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set event destination.
--
-- An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateConfigurationSetEventDestination
  ( -- * Creating a request
    CreateConfigurationSetEventDestination (..),
    mkCreateConfigurationSetEventDestination,

    -- ** Request lenses
    ccsedConfigurationSetName,
    ccsedEventDestination,

    -- * Destructuring the response
    CreateConfigurationSetEventDestinationResponse (..),
    mkCreateConfigurationSetEventDestinationResponse,

    -- ** Response lenses
    ccsedrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { -- | The name of the configuration set that the event destination should be associated with.
    configurationSetName :: Types.ConfigurationSetName,
    -- | An object that describes the AWS service that email sending event information will be published to.
    eventDestination :: Types.EventDestination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSetEventDestination' value with any optional fields omitted.
mkCreateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Types.ConfigurationSetName ->
  -- | 'eventDestination'
  Types.EventDestination ->
  CreateConfigurationSetEventDestination
mkCreateConfigurationSetEventDestination
  configurationSetName
  eventDestination =
    CreateConfigurationSetEventDestination'
      { configurationSetName,
        eventDestination
      }

-- | The name of the configuration set that the event destination should be associated with.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedConfigurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Types.ConfigurationSetName
ccsedConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED ccsedConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | An object that describes the AWS service that email sending event information will be published to.
--
-- /Note:/ Consider using 'eventDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedEventDestination :: Lens.Lens' CreateConfigurationSetEventDestination Types.EventDestination
ccsedEventDestination = Lens.field @"eventDestination"
{-# DEPRECATED ccsedEventDestination "Use generic-lens or generic-optics with 'eventDestination' instead." #-}

instance Core.AWSRequest CreateConfigurationSetEventDestination where
  type
    Rs CreateConfigurationSetEventDestination =
      CreateConfigurationSetEventDestinationResponse
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
            ( Core.pure ("Action", "CreateConfigurationSetEventDestination")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ConfigurationSetName" configurationSetName)
                Core.<> (Core.toQueryValue "EventDestination" eventDestination)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetEventDestinationResult"
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetEventDestinationResponse' smart constructor.
newtype CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSetEventDestinationResponse' value with any optional fields omitted.
mkCreateConfigurationSetEventDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateConfigurationSetEventDestinationResponse
mkCreateConfigurationSetEventDestinationResponse responseStatus =
  CreateConfigurationSetEventDestinationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedrrsResponseStatus :: Lens.Lens' CreateConfigurationSetEventDestinationResponse Core.Int
ccsedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
