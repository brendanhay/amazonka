{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /workflow type/ . After a workflow type has been undeprecated, you can create new executions of that type.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.UndeprecateWorkflowType
  ( -- * Creating a request
    UndeprecateWorkflowType (..),
    mkUndeprecateWorkflowType,

    -- ** Request lenses
    uwtDomain,
    uwtWorkflowType,

    -- * Destructuring the response
    UndeprecateWorkflowTypeResponse (..),
    mkUndeprecateWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkUndeprecateWorkflowType' smart constructor.
data UndeprecateWorkflowType = UndeprecateWorkflowType'
  { -- | The name of the domain of the deprecated workflow type.
    domain :: Types.DomainName,
    -- | The name of the domain of the deprecated workflow type.
    workflowType :: Types.WorkflowType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateWorkflowType' value with any optional fields omitted.
mkUndeprecateWorkflowType ::
  -- | 'domain'
  Types.DomainName ->
  -- | 'workflowType'
  Types.WorkflowType ->
  UndeprecateWorkflowType
mkUndeprecateWorkflowType domain workflowType =
  UndeprecateWorkflowType' {domain, workflowType}

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwtDomain :: Lens.Lens' UndeprecateWorkflowType Types.DomainName
uwtDomain = Lens.field @"domain"
{-# DEPRECATED uwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwtWorkflowType :: Lens.Lens' UndeprecateWorkflowType Types.WorkflowType
uwtWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED uwtWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Core.FromJSON UndeprecateWorkflowType where
  toJSON UndeprecateWorkflowType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("workflowType" Core..= workflowType)
          ]
      )

instance Core.AWSRequest UndeprecateWorkflowType where
  type Rs UndeprecateWorkflowType = UndeprecateWorkflowTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.UndeprecateWorkflowType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UndeprecateWorkflowTypeResponse'

-- | /See:/ 'mkUndeprecateWorkflowTypeResponse' smart constructor.
data UndeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateWorkflowTypeResponse' value with any optional fields omitted.
mkUndeprecateWorkflowTypeResponse ::
  UndeprecateWorkflowTypeResponse
mkUndeprecateWorkflowTypeResponse =
  UndeprecateWorkflowTypeResponse'
