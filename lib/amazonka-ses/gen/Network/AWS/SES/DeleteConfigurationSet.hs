{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteConfigurationSet
  ( -- * Creating a request
    DeleteConfigurationSet (..),
    mkDeleteConfigurationSet,

    -- ** Request lenses
    dConfigurationSetName,

    -- * Destructuring the response
    DeleteConfigurationSetResponse (..),
    mkDeleteConfigurationSetResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteConfigurationSet' smart constructor.
newtype DeleteConfigurationSet = DeleteConfigurationSet'
  { -- | The name of the configuration set to delete.
    configurationSetName :: Types.ConfigurationSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSet' value with any optional fields omitted.
mkDeleteConfigurationSet ::
  -- | 'configurationSetName'
  Types.ConfigurationSetName ->
  DeleteConfigurationSet
mkDeleteConfigurationSet configurationSetName =
  DeleteConfigurationSet' {configurationSetName}

-- | The name of the configuration set to delete.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigurationSetName :: Lens.Lens' DeleteConfigurationSet Types.ConfigurationSetName
dConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED dConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Core.AWSRequest DeleteConfigurationSet where
  type Rs DeleteConfigurationSet = DeleteConfigurationSetResponse
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
            ( Core.pure ("Action", "DeleteConfigurationSet")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ConfigurationSetName" configurationSetName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteConfigurationSetResult"
      ( \s h x ->
          DeleteConfigurationSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetResponse' smart constructor.
newtype DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationSetResponse' value with any optional fields omitted.
mkDeleteConfigurationSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConfigurationSetResponse
mkDeleteConfigurationSetResponse responseStatus =
  DeleteConfigurationSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteConfigurationSetResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
