{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain.
--
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.RegisterDomain
    (
    -- * Creating a request
      RegisterDomain (..)
    , mkRegisterDomain
    -- ** Request lenses
    , rdName
    , rdWorkflowExecutionRetentionPeriodInDays
    , rdDescription
    , rdTags

    -- * Destructuring the response
    , RegisterDomainResponse (..)
    , mkRegisterDomainResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { name :: Types.DomainName
    -- ^ Name of the domain to register. The name must be unique in the region that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , workflowExecutionRetentionPeriodInDays :: Types.WorkflowExecutionRetentionPeriodInDays
    -- ^ The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted.
-- The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
  , description :: Core.Maybe Types.Description
    -- ^ A text description of the domain.
  , tags :: Core.Maybe [Types.ResourceTag]
    -- ^ Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDomain' value with any optional fields omitted.
mkRegisterDomain
    :: Types.DomainName -- ^ 'name'
    -> Types.WorkflowExecutionRetentionPeriodInDays -- ^ 'workflowExecutionRetentionPeriodInDays'
    -> RegisterDomain
mkRegisterDomain name workflowExecutionRetentionPeriodInDays
  = RegisterDomain'{name, workflowExecutionRetentionPeriodInDays,
                    description = Core.Nothing, tags = Core.Nothing}

-- | Name of the domain to register. The name must be unique in the region that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdName :: Lens.Lens' RegisterDomain Types.DomainName
rdName = Lens.field @"name"
{-# INLINEABLE rdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted.
-- The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'workflowExecutionRetentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdWorkflowExecutionRetentionPeriodInDays :: Lens.Lens' RegisterDomain Types.WorkflowExecutionRetentionPeriodInDays
rdWorkflowExecutionRetentionPeriodInDays = Lens.field @"workflowExecutionRetentionPeriodInDays"
{-# INLINEABLE rdWorkflowExecutionRetentionPeriodInDays #-}
{-# DEPRECATED workflowExecutionRetentionPeriodInDays "Use generic-lens or generic-optics with 'workflowExecutionRetentionPeriodInDays' instead"  #-}

-- | A text description of the domain.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDescription :: Lens.Lens' RegisterDomain (Core.Maybe Types.Description)
rdDescription = Lens.field @"description"
{-# INLINEABLE rdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RegisterDomain (Core.Maybe [Types.ResourceTag])
rdTags = Lens.field @"tags"
{-# INLINEABLE rdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery RegisterDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterDomain where
        toHeaders RegisterDomain{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.RegisterDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON RegisterDomain where
        toJSON RegisterDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just
                    ("workflowExecutionRetentionPeriodInDays" Core..=
                       workflowExecutionRetentionPeriodInDays),
                  ("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest RegisterDomain where
        type Rs RegisterDomain = RegisterDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RegisterDomainResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDomainResponse' value with any optional fields omitted.
mkRegisterDomainResponse
    :: RegisterDomainResponse
mkRegisterDomainResponse = RegisterDomainResponse'
