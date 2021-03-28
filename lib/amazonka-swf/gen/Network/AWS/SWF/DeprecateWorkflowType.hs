{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /workflow type/ . After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.
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
module Network.AWS.SWF.DeprecateWorkflowType
    (
    -- * Creating a request
      DeprecateWorkflowType (..)
    , mkDeprecateWorkflowType
    -- ** Request lenses
    , dDomain
    , dWorkflowType

    -- * Destructuring the response
    , DeprecateWorkflowTypeResponse (..)
    , mkDeprecateWorkflowTypeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDeprecateWorkflowType' smart constructor.
data DeprecateWorkflowType = DeprecateWorkflowType'
  { domain :: Types.Domain
    -- ^ The name of the domain in which the workflow type is registered.
  , workflowType :: Types.WorkflowType
    -- ^ The workflow type to deprecate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateWorkflowType' value with any optional fields omitted.
mkDeprecateWorkflowType
    :: Types.Domain -- ^ 'domain'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> DeprecateWorkflowType
mkDeprecateWorkflowType domain workflowType
  = DeprecateWorkflowType'{domain, workflowType}

-- | The name of the domain in which the workflow type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomain :: Lens.Lens' DeprecateWorkflowType Types.Domain
dDomain = Lens.field @"domain"
{-# INLINEABLE dDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The workflow type to deprecate.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkflowType :: Lens.Lens' DeprecateWorkflowType Types.WorkflowType
dWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE dWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

instance Core.ToQuery DeprecateWorkflowType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeprecateWorkflowType where
        toHeaders DeprecateWorkflowType{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.DeprecateWorkflowType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DeprecateWorkflowType where
        toJSON DeprecateWorkflowType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("workflowType" Core..= workflowType)])

instance Core.AWSRequest DeprecateWorkflowType where
        type Rs DeprecateWorkflowType = DeprecateWorkflowTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeprecateWorkflowTypeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeprecateWorkflowTypeResponse' smart constructor.
data DeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateWorkflowTypeResponse' value with any optional fields omitted.
mkDeprecateWorkflowTypeResponse
    :: DeprecateWorkflowTypeResponse
mkDeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'
