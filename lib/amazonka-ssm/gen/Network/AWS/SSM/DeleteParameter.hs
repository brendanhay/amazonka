{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system.
module Network.AWS.SSM.DeleteParameter
  ( -- * Creating a request
    DeleteParameter (..),
    mkDeleteParameter,

    -- ** Request lenses
    dpfName,

    -- * Destructuring the response
    DeleteParameterResponse (..),
    mkDeleteParameterResponse,

    -- ** Response lenses
    dprgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteParameter' smart constructor.
newtype DeleteParameter = DeleteParameter'
  { -- | The name of the parameter to delete.
    name :: Types.PSParameterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameter' value with any optional fields omitted.
mkDeleteParameter ::
  -- | 'name'
  Types.PSParameterName ->
  DeleteParameter
mkDeleteParameter name = DeleteParameter' {name}

-- | The name of the parameter to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfName :: Lens.Lens' DeleteParameter Types.PSParameterName
dpfName = Lens.field @"name"
{-# DEPRECATED dpfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteParameter where
  toJSON DeleteParameter {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteParameter where
  type Rs DeleteParameter = DeleteParameterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeleteParameter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteParameterResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteParameterResponse' smart constructor.
newtype DeleteParameterResponse = DeleteParameterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameterResponse' value with any optional fields omitted.
mkDeleteParameterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteParameterResponse
mkDeleteParameterResponse responseStatus =
  DeleteParameterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DeleteParameterResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
