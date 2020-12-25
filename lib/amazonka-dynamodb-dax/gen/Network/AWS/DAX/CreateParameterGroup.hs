{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.CreateParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new parameter group. A parameter group is a collection of parameters that you apply to all of the nodes in a DAX cluster.
module Network.AWS.DAX.CreateParameterGroup
  ( -- * Creating a request
    CreateParameterGroup (..),
    mkCreateParameterGroup,

    -- ** Request lenses
    cpgParameterGroupName,
    cpgDescription,

    -- * Destructuring the response
    CreateParameterGroupResponse (..),
    mkCreateParameterGroupResponse,

    -- ** Response lenses
    cpgrrsParameterGroup,
    cpgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateParameterGroup' smart constructor.
data CreateParameterGroup = CreateParameterGroup'
  { -- | The name of the parameter group to apply to all of the clusters in this replication group.
    parameterGroupName :: Types.String,
    -- | A description of the parameter group.
    description :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateParameterGroup' value with any optional fields omitted.
mkCreateParameterGroup ::
  -- | 'parameterGroupName'
  Types.String ->
  CreateParameterGroup
mkCreateParameterGroup parameterGroupName =
  CreateParameterGroup'
    { parameterGroupName,
      description = Core.Nothing
    }

-- | The name of the parameter group to apply to all of the clusters in this replication group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupName :: Lens.Lens' CreateParameterGroup Types.String
cpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED cpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDescription :: Lens.Lens' CreateParameterGroup (Core.Maybe Types.String)
cpgDescription = Lens.field @"description"
{-# DEPRECATED cpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateParameterGroup where
  toJSON CreateParameterGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterGroupName" Core..= parameterGroupName),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateParameterGroup where
  type Rs CreateParameterGroup = CreateParameterGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.CreateParameterGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParameterGroupResponse'
            Core.<$> (x Core..:? "ParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateParameterGroupResponse' smart constructor.
data CreateParameterGroupResponse = CreateParameterGroupResponse'
  { -- | Represents the output of a /CreateParameterGroup/ action.
    parameterGroup :: Core.Maybe Types.ParameterGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateParameterGroupResponse' value with any optional fields omitted.
mkCreateParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateParameterGroupResponse
mkCreateParameterGroupResponse responseStatus =
  CreateParameterGroupResponse'
    { parameterGroup = Core.Nothing,
      responseStatus
    }

-- | Represents the output of a /CreateParameterGroup/ action.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsParameterGroup :: Lens.Lens' CreateParameterGroupResponse (Core.Maybe Types.ParameterGroup)
cpgrrsParameterGroup = Lens.field @"parameterGroup"
{-# DEPRECATED cpgrrsParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsResponseStatus :: Lens.Lens' CreateParameterGroupResponse Core.Int
cpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
