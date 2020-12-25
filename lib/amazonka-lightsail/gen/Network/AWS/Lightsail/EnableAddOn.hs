{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.EnableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or modifies an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.EnableAddOn
  ( -- * Creating a request
    EnableAddOn (..),
    mkEnableAddOn,

    -- ** Request lenses
    eaoResourceName,
    eaoAddOnRequest,

    -- * Destructuring the response
    EnableAddOnResponse (..),
    mkEnableAddOnResponse,

    -- ** Response lenses
    eaorrsOperations,
    eaorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableAddOn' smart constructor.
data EnableAddOn = EnableAddOn'
  { -- | The name of the source resource for which to enable or modify the add-on.
    resourceName :: Types.ResourceName,
    -- | An array of strings representing the add-on to enable or modify.
    addOnRequest :: Types.AddOnRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAddOn' value with any optional fields omitted.
mkEnableAddOn ::
  -- | 'resourceName'
  Types.ResourceName ->
  -- | 'addOnRequest'
  Types.AddOnRequest ->
  EnableAddOn
mkEnableAddOn resourceName addOnRequest =
  EnableAddOn' {resourceName, addOnRequest}

-- | The name of the source resource for which to enable or modify the add-on.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoResourceName :: Lens.Lens' EnableAddOn Types.ResourceName
eaoResourceName = Lens.field @"resourceName"
{-# DEPRECATED eaoResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | An array of strings representing the add-on to enable or modify.
--
-- /Note:/ Consider using 'addOnRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoAddOnRequest :: Lens.Lens' EnableAddOn Types.AddOnRequest
eaoAddOnRequest = Lens.field @"addOnRequest"
{-# DEPRECATED eaoAddOnRequest "Use generic-lens or generic-optics with 'addOnRequest' instead." #-}

instance Core.FromJSON EnableAddOn where
  toJSON EnableAddOn {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceName" Core..= resourceName),
            Core.Just ("addOnRequest" Core..= addOnRequest)
          ]
      )

instance Core.AWSRequest EnableAddOn where
  type Rs EnableAddOn = EnableAddOnResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.EnableAddOn")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAddOnResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableAddOnResponse' smart constructor.
data EnableAddOnResponse = EnableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnableAddOnResponse' value with any optional fields omitted.
mkEnableAddOnResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableAddOnResponse
mkEnableAddOnResponse responseStatus =
  EnableAddOnResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorrsOperations :: Lens.Lens' EnableAddOnResponse (Core.Maybe [Types.Operation])
eaorrsOperations = Lens.field @"operations"
{-# DEPRECATED eaorrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorrsResponseStatus :: Lens.Lens' EnableAddOnResponse Core.Int
eaorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eaorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
