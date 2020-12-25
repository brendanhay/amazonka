{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified /workflow type/ . This includes configuration settings specified when the type was registered and other information such as creation date, current status, etc.
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
module Network.AWS.SWF.DescribeWorkflowType
  ( -- * Creating a request
    DescribeWorkflowType (..),
    mkDescribeWorkflowType,

    -- ** Request lenses
    dwtDomain,
    dwtWorkflowType,

    -- * Destructuring the response
    DescribeWorkflowTypeResponse (..),
    mkDescribeWorkflowTypeResponse,

    -- ** Response lenses
    dwtrrsTypeInfo,
    dwtrrsConfiguration,
    dwtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDescribeWorkflowType' smart constructor.
data DescribeWorkflowType = DescribeWorkflowType'
  { -- | The name of the domain in which this workflow type is registered.
    domain :: Types.Domain,
    -- | The workflow type to describe.
    workflowType :: Types.WorkflowType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkflowType' value with any optional fields omitted.
mkDescribeWorkflowType ::
  -- | 'domain'
  Types.Domain ->
  -- | 'workflowType'
  Types.WorkflowType ->
  DescribeWorkflowType
mkDescribeWorkflowType domain workflowType =
  DescribeWorkflowType' {domain, workflowType}

-- | The name of the domain in which this workflow type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtDomain :: Lens.Lens' DescribeWorkflowType Types.Domain
dwtDomain = Lens.field @"domain"
{-# DEPRECATED dwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflow type to describe.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtWorkflowType :: Lens.Lens' DescribeWorkflowType Types.WorkflowType
dwtWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED dwtWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Core.FromJSON DescribeWorkflowType where
  toJSON DescribeWorkflowType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("workflowType" Core..= workflowType)
          ]
      )

instance Core.AWSRequest DescribeWorkflowType where
  type Rs DescribeWorkflowType = DescribeWorkflowTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.DescribeWorkflowType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkflowTypeResponse'
            Core.<$> (x Core..: "typeInfo")
            Core.<*> (x Core..: "configuration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains details about a workflow type.
--
-- /See:/ 'mkDescribeWorkflowTypeResponse' smart constructor.
data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse'
  { -- | General information about the workflow type.
    --
    -- The status of the workflow type (returned in the WorkflowTypeInfo structure) can be one of the following.
    --
    --     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running.
    --
    --
    --     * @DEPRECATED@ – The type was deprecated using 'DeprecateWorkflowType' , but is still in use. You should keep workers supporting this type running. You cannot create new workflow executions of this type.
    typeInfo :: Types.WorkflowTypeInfo,
    -- | Configuration settings of the workflow type registered through 'RegisterWorkflowType'
    configuration :: Types.WorkflowTypeConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeWorkflowTypeResponse' value with any optional fields omitted.
mkDescribeWorkflowTypeResponse ::
  -- | 'typeInfo'
  Types.WorkflowTypeInfo ->
  -- | 'configuration'
  Types.WorkflowTypeConfiguration ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkflowTypeResponse
mkDescribeWorkflowTypeResponse
  typeInfo
  configuration
  responseStatus =
    DescribeWorkflowTypeResponse'
      { typeInfo,
        configuration,
        responseStatus
      }

-- | General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo structure) can be one of the following.
--
--     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running.
--
--
--     * @DEPRECATED@ – The type was deprecated using 'DeprecateWorkflowType' , but is still in use. You should keep workers supporting this type running. You cannot create new workflow executions of this type.
--
--
--
-- /Note:/ Consider using 'typeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrrsTypeInfo :: Lens.Lens' DescribeWorkflowTypeResponse Types.WorkflowTypeInfo
dwtrrsTypeInfo = Lens.field @"typeInfo"
{-# DEPRECATED dwtrrsTypeInfo "Use generic-lens or generic-optics with 'typeInfo' instead." #-}

-- | Configuration settings of the workflow type registered through 'RegisterWorkflowType'
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrrsConfiguration :: Lens.Lens' DescribeWorkflowTypeResponse Types.WorkflowTypeConfiguration
dwtrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED dwtrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrrsResponseStatus :: Lens.Lens' DescribeWorkflowTypeResponse Core.Int
dwtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
