{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RebootInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific instance.
--
-- The @reboot instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.RebootInstance
  ( -- * Creating a request
    RebootInstance (..),
    mkRebootInstance,

    -- ** Request lenses
    riInstanceName,

    -- * Destructuring the response
    RebootInstanceResponse (..),
    mkRebootInstanceResponse,

    -- ** Response lenses
    rirrsOperations,
    rirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootInstance' smart constructor.
newtype RebootInstance = RebootInstance'
  { -- | The name of the instance to reboot.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootInstance' value with any optional fields omitted.
mkRebootInstance ::
  -- | 'instanceName'
  Types.ResourceName ->
  RebootInstance
mkRebootInstance instanceName = RebootInstance' {instanceName}

-- | The name of the instance to reboot.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceName :: Lens.Lens' RebootInstance Types.ResourceName
riInstanceName = Lens.field @"instanceName"
{-# DEPRECATED riInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON RebootInstance where
  toJSON RebootInstance {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest RebootInstance where
  type Rs RebootInstance = RebootInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.RebootInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootInstanceResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RebootInstanceResponse' value with any optional fields omitted.
mkRebootInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebootInstanceResponse
mkRebootInstanceResponse responseStatus =
  RebootInstanceResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsOperations :: Lens.Lens' RebootInstanceResponse (Core.Maybe [Types.Operation])
rirrsOperations = Lens.field @"operations"
{-# DEPRECATED rirrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RebootInstanceResponse Core.Int
rirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
