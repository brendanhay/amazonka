{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified constraint.
module Network.AWS.ServiceCatalog.UpdateConstraint
    (
    -- * Creating a request
      UpdateConstraint (..)
    , mkUpdateConstraint
    -- ** Request lenses
    , ucId
    , ucAcceptLanguage
    , ucDescription
    , ucParameters

    -- * Destructuring the response
    , UpdateConstraintResponse (..)
    , mkUpdateConstraintResponse
    -- ** Response lenses
    , ucrrsConstraintDetail
    , ucrrsConstraintParameters
    , ucrrsStatus
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { id :: Types.Id
    -- ^ The identifier of the constraint.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  , description :: Core.Maybe Types.ConstraintDescription
    -- ^ The updated description of the constraint.
  , parameters :: Core.Maybe Types.ConstraintParameters
    -- ^ The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:
--
--
--     * LAUNCH
--
--     * You are required to specify either the @RoleArn@ or the @LocalRoleName@ but can't use both.
-- Specify the @RoleArn@ property as follows:
-- @{"RoleArn" : "arn:aws:iam::123456789012:role/LaunchRole"}@ 
-- Specify the @LocalRoleName@ property as follows:
-- @{"LocalRoleName": "SCBasicLaunchRole"}@ 
-- If you specify the @LocalRoleName@ property, when an account uses the launch constraint, the IAM role with that name in the account will be used. This allows launch-role constraints to be account-agnostic so the administrator can create fewer resources per shared account.
-- You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
-- You also cannot have more than one @LAUNCH@ constraint on a product and portfolio.
--
--
--     * NOTIFICATION
--
--     * Specify the @NotificationArns@ property as follows:
-- @{"NotificationArns" : ["arn:aws:sns:us-east-1:123456789012:Topic"]}@ 
--
--
--     * RESOURCE_UPDATE
--
--     * Specify the @TagUpdatesOnProvisionedProduct@ property as follows:
-- @{"Version":"2.0","Properties":{"TagUpdateOnProvisionedProduct":"String"}}@ 
-- The @TagUpdatesOnProvisionedProduct@ property accepts a string value of @ALLOWED@ or @NOT_ALLOWED@ .
--
--
--     * STACKSET
--
--     * Specify the @Parameters@ property as follows:
-- @{"Version": "String", "Properties": {"AccountList": [ "String" ], "RegionList": [ "String" ], "AdminRole": "String", "ExecutionRole": "String"}}@ 
-- You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
-- You also cannot have more than one @STACKSET@ constraint on a product and portfolio.
-- Products with a @STACKSET@ constraint will launch an AWS CloudFormation stack set.
--
--
--     * TEMPLATE
--
--     * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConstraint' value with any optional fields omitted.
mkUpdateConstraint
    :: Types.Id -- ^ 'id'
    -> UpdateConstraint
mkUpdateConstraint id
  = UpdateConstraint'{id, acceptLanguage = Core.Nothing,
                      description = Core.Nothing, parameters = Core.Nothing}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucId :: Lens.Lens' UpdateConstraint Types.Id
ucId = Lens.field @"id"
{-# INLINEABLE ucId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAcceptLanguage :: Lens.Lens' UpdateConstraint (Core.Maybe Types.AcceptLanguage)
ucAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE ucAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The updated description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateConstraint (Core.Maybe Types.ConstraintDescription)
ucDescription = Lens.field @"description"
{-# INLINEABLE ucDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:
--
--
--     * LAUNCH
--
--     * You are required to specify either the @RoleArn@ or the @LocalRoleName@ but can't use both.
-- Specify the @RoleArn@ property as follows:
-- @{"RoleArn" : "arn:aws:iam::123456789012:role/LaunchRole"}@ 
-- Specify the @LocalRoleName@ property as follows:
-- @{"LocalRoleName": "SCBasicLaunchRole"}@ 
-- If you specify the @LocalRoleName@ property, when an account uses the launch constraint, the IAM role with that name in the account will be used. This allows launch-role constraints to be account-agnostic so the administrator can create fewer resources per shared account.
-- You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
-- You also cannot have more than one @LAUNCH@ constraint on a product and portfolio.
--
--
--     * NOTIFICATION
--
--     * Specify the @NotificationArns@ property as follows:
-- @{"NotificationArns" : ["arn:aws:sns:us-east-1:123456789012:Topic"]}@ 
--
--
--     * RESOURCE_UPDATE
--
--     * Specify the @TagUpdatesOnProvisionedProduct@ property as follows:
-- @{"Version":"2.0","Properties":{"TagUpdateOnProvisionedProduct":"String"}}@ 
-- The @TagUpdatesOnProvisionedProduct@ property accepts a string value of @ALLOWED@ or @NOT_ALLOWED@ .
--
--
--     * STACKSET
--
--     * Specify the @Parameters@ property as follows:
-- @{"Version": "String", "Properties": {"AccountList": [ "String" ], "RegionList": [ "String" ], "AdminRole": "String", "ExecutionRole": "String"}}@ 
-- You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
-- You also cannot have more than one @STACKSET@ constraint on a product and portfolio.
-- Products with a @STACKSET@ constraint will launch an AWS CloudFormation stack set.
--
--
--     * TEMPLATE
--
--     * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
--
--
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucParameters :: Lens.Lens' UpdateConstraint (Core.Maybe Types.ConstraintParameters)
ucParameters = Lens.field @"parameters"
{-# INLINEABLE ucParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery UpdateConstraint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateConstraint where
        toHeaders UpdateConstraint{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.UpdateConstraint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateConstraint where
        toJSON UpdateConstraint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("Description" Core..=) Core.<$> description,
                  ("Parameters" Core..=) Core.<$> parameters])

instance Core.AWSRequest UpdateConstraint where
        type Rs UpdateConstraint = UpdateConstraintResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateConstraintResponse' Core.<$>
                   (x Core..:? "ConstraintDetail") Core.<*>
                     x Core..:? "ConstraintParameters"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
  { constraintDetail :: Core.Maybe Types.ConstraintDetail
    -- ^ Information about the constraint.
  , constraintParameters :: Core.Maybe Types.ConstraintParameters
    -- ^ The constraint parameters.
  , status :: Core.Maybe Types.RequestStatus
    -- ^ The status of the current request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConstraintResponse' value with any optional fields omitted.
mkUpdateConstraintResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConstraintResponse
mkUpdateConstraintResponse responseStatus
  = UpdateConstraintResponse'{constraintDetail = Core.Nothing,
                              constraintParameters = Core.Nothing, status = Core.Nothing,
                              responseStatus}

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsConstraintDetail :: Lens.Lens' UpdateConstraintResponse (Core.Maybe Types.ConstraintDetail)
ucrrsConstraintDetail = Lens.field @"constraintDetail"
{-# INLINEABLE ucrrsConstraintDetail #-}
{-# DEPRECATED constraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead"  #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsConstraintParameters :: Lens.Lens' UpdateConstraintResponse (Core.Maybe Types.ConstraintParameters)
ucrrsConstraintParameters = Lens.field @"constraintParameters"
{-# INLINEABLE ucrrsConstraintParameters #-}
{-# DEPRECATED constraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead"  #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsStatus :: Lens.Lens' UpdateConstraintResponse (Core.Maybe Types.RequestStatus)
ucrrsStatus = Lens.field @"status"
{-# INLINEABLE ucrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateConstraintResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
