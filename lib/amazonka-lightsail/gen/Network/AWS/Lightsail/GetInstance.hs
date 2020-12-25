{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is a virtual private server.
module Network.AWS.Lightsail.GetInstance
  ( -- * Creating a request
    GetInstance (..),
    mkGetInstance,

    -- ** Request lenses
    giInstanceName,

    -- * Destructuring the response
    GetInstanceResponse (..),
    mkGetInstanceResponse,

    -- ** Response lenses
    grsInstance,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstance' smart constructor.
newtype GetInstance = GetInstance'
  { -- | The name of the instance.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstance' value with any optional fields omitted.
mkGetInstance ::
  -- | 'instanceName'
  Types.ResourceName ->
  GetInstance
mkGetInstance instanceName = GetInstance' {instanceName}

-- | The name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInstanceName :: Lens.Lens' GetInstance Types.ResourceName
giInstanceName = Lens.field @"instanceName"
{-# DEPRECATED giInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON GetInstance where
  toJSON GetInstance {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetInstance where
  type Rs GetInstance = GetInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Core.<$> (x Core..:? "instance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { -- | An array of key-value pairs containing information about the specified instance.
    instance' :: Core.Maybe Types.Instance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstanceResponse' value with any optional fields omitted.
mkGetInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceResponse
mkGetInstanceResponse responseStatus =
  GetInstanceResponse' {instance' = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the specified instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsInstance :: Lens.Lens' GetInstanceResponse (Core.Maybe Types.Instance)
grsInstance = Lens.field @"instance'"
{-# DEPRECATED grsInstance "Use generic-lens or generic-optics with 'instance'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetInstanceResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
