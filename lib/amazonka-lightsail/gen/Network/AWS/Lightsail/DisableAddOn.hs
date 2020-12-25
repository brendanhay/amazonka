{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DisableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DisableAddOn
  ( -- * Creating a request
    DisableAddOn (..),
    mkDisableAddOn,

    -- ** Request lenses
    daoAddOnType,
    daoResourceName,

    -- * Destructuring the response
    DisableAddOnResponse (..),
    mkDisableAddOnResponse,

    -- ** Response lenses
    daorrsOperations,
    daorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableAddOn' smart constructor.
data DisableAddOn = DisableAddOn'
  { -- | The add-on type to disable.
    addOnType :: Types.AddOnType,
    -- | The name of the source resource for which to disable the add-on.
    resourceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAddOn' value with any optional fields omitted.
mkDisableAddOn ::
  -- | 'addOnType'
  Types.AddOnType ->
  -- | 'resourceName'
  Types.ResourceName ->
  DisableAddOn
mkDisableAddOn addOnType resourceName =
  DisableAddOn' {addOnType, resourceName}

-- | The add-on type to disable.
--
-- /Note:/ Consider using 'addOnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoAddOnType :: Lens.Lens' DisableAddOn Types.AddOnType
daoAddOnType = Lens.field @"addOnType"
{-# DEPRECATED daoAddOnType "Use generic-lens or generic-optics with 'addOnType' instead." #-}

-- | The name of the source resource for which to disable the add-on.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoResourceName :: Lens.Lens' DisableAddOn Types.ResourceName
daoResourceName = Lens.field @"resourceName"
{-# DEPRECATED daoResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Core.FromJSON DisableAddOn where
  toJSON DisableAddOn {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("addOnType" Core..= addOnType),
            Core.Just ("resourceName" Core..= resourceName)
          ]
      )

instance Core.AWSRequest DisableAddOn where
  type Rs DisableAddOn = DisableAddOnResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DisableAddOn")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableAddOnResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableAddOnResponse' smart constructor.
data DisableAddOnResponse = DisableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DisableAddOnResponse' value with any optional fields omitted.
mkDisableAddOnResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableAddOnResponse
mkDisableAddOnResponse responseStatus =
  DisableAddOnResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorrsOperations :: Lens.Lens' DisableAddOnResponse (Core.Maybe [Types.Operation])
daorrsOperations = Lens.field @"operations"
{-# DEPRECATED daorrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorrsResponseStatus :: Lens.Lens' DisableAddOnResponse Core.Int
daorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
