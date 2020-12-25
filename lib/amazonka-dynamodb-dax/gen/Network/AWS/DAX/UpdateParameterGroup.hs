{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
module Network.AWS.DAX.UpdateParameterGroup
  ( -- * Creating a request
    UpdateParameterGroup (..),
    mkUpdateParameterGroup,

    -- ** Request lenses
    upgParameterGroupName,
    upgParameterNameValues,

    -- * Destructuring the response
    UpdateParameterGroupResponse (..),
    mkUpdateParameterGroupResponse,

    -- ** Response lenses
    upgrrsParameterGroup,
    upgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateParameterGroup' smart constructor.
data UpdateParameterGroup = UpdateParameterGroup'
  { -- | The name of the parameter group.
    parameterGroupName :: Types.String,
    -- | An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
    parameterNameValues :: [Types.ParameterNameValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateParameterGroup' value with any optional fields omitted.
mkUpdateParameterGroup ::
  -- | 'parameterGroupName'
  Types.String ->
  UpdateParameterGroup
mkUpdateParameterGroup parameterGroupName =
  UpdateParameterGroup'
    { parameterGroupName,
      parameterNameValues = Core.mempty
    }

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgParameterGroupName :: Lens.Lens' UpdateParameterGroup Types.String
upgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED upgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgParameterNameValues :: Lens.Lens' UpdateParameterGroup [Types.ParameterNameValue]
upgParameterNameValues = Lens.field @"parameterNameValues"
{-# DEPRECATED upgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

instance Core.FromJSON UpdateParameterGroup where
  toJSON UpdateParameterGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterGroupName" Core..= parameterGroupName),
            Core.Just ("ParameterNameValues" Core..= parameterNameValues)
          ]
      )

instance Core.AWSRequest UpdateParameterGroup where
  type Rs UpdateParameterGroup = UpdateParameterGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.UpdateParameterGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateParameterGroupResponse'
            Core.<$> (x Core..:? "ParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateParameterGroupResponse' smart constructor.
data UpdateParameterGroupResponse = UpdateParameterGroupResponse'
  { -- | The parameter group that has been modified.
    parameterGroup :: Core.Maybe Types.ParameterGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateParameterGroupResponse' value with any optional fields omitted.
mkUpdateParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateParameterGroupResponse
mkUpdateParameterGroupResponse responseStatus =
  UpdateParameterGroupResponse'
    { parameterGroup = Core.Nothing,
      responseStatus
    }

-- | The parameter group that has been modified.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrrsParameterGroup :: Lens.Lens' UpdateParameterGroupResponse (Core.Maybe Types.ParameterGroup)
upgrrsParameterGroup = Lens.field @"parameterGroup"
{-# DEPRECATED upgrrsParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrrsResponseStatus :: Lens.Lens' UpdateParameterGroupResponse Core.Int
upgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
