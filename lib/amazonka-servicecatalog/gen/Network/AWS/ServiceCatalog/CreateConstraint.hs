{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a constraint.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreateConstraint
  ( -- * Creating a request
    CreateConstraint (..),
    mkCreateConstraint,

    -- ** Request lenses
    ccPortfolioId,
    ccProductId,
    ccParameters,
    ccType,
    ccIdempotencyToken,
    ccAcceptLanguage,
    ccDescription,

    -- * Destructuring the response
    CreateConstraintResponse (..),
    mkCreateConstraintResponse,

    -- ** Response lenses
    ccrrsConstraintDetail,
    ccrrsConstraintParameters,
    ccrrsStatus,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateConstraint' smart constructor.
data CreateConstraint = CreateConstraint'
  { -- | The portfolio identifier.
    portfolioId :: Types.PortfolioId,
    -- | The product identifier.
    productId :: Types.ProductId,
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
    parameters :: Types.ConstraintParameters,
    -- | The type of constraint.
    --
    --
    --     * @LAUNCH@
    --
    --
    --     * @NOTIFICATION@
    --
    --
    --     * @RESOURCE_UPDATE@
    --
    --
    --     * @STACKSET@
    --
    --
    --     * @TEMPLATE@
    type' :: Types.Type,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Types.IdempotencyToken,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The description of the constraint.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConstraint' value with any optional fields omitted.
mkCreateConstraint ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  -- | 'productId'
  Types.ProductId ->
  -- | 'parameters'
  Types.ConstraintParameters ->
  -- | 'type\''
  Types.Type ->
  -- | 'idempotencyToken'
  Types.IdempotencyToken ->
  CreateConstraint
mkCreateConstraint
  portfolioId
  productId
  parameters
  type'
  idempotencyToken =
    CreateConstraint'
      { portfolioId,
        productId,
        parameters,
        type',
        idempotencyToken,
        acceptLanguage = Core.Nothing,
        description = Core.Nothing
      }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPortfolioId :: Lens.Lens' CreateConstraint Types.PortfolioId
ccPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED ccPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccProductId :: Lens.Lens' CreateConstraint Types.ProductId
ccProductId = Lens.field @"productId"
{-# DEPRECATED ccProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

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
ccParameters :: Lens.Lens' CreateConstraint Types.ConstraintParameters
ccParameters = Lens.field @"parameters"
{-# DEPRECATED ccParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of constraint.
--
--
--     * @LAUNCH@
--
--
--     * @NOTIFICATION@
--
--
--     * @RESOURCE_UPDATE@
--
--
--     * @STACKSET@
--
--
--     * @TEMPLATE@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccType :: Lens.Lens' CreateConstraint Types.Type
ccType = Lens.field @"type'"
{-# DEPRECATED ccType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIdempotencyToken :: Lens.Lens' CreateConstraint Types.IdempotencyToken
ccIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED ccIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
ccAcceptLanguage :: Lens.Lens' CreateConstraint (Core.Maybe Types.AcceptLanguage)
ccAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED ccAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateConstraint (Core.Maybe Types.Description)
ccDescription = Lens.field @"description"
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateConstraint where
  toJSON CreateConstraint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("ProductId" Core..= productId),
            Core.Just ("Parameters" Core..= parameters),
            Core.Just ("Type" Core..= type'),
            Core.Just ("IdempotencyToken" Core..= idempotencyToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateConstraint where
  type Rs CreateConstraint = CreateConstraintResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.CreateConstraint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConstraintResponse'
            Core.<$> (x Core..:? "ConstraintDetail")
            Core.<*> (x Core..:? "ConstraintParameters")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateConstraintResponse' smart constructor.
data CreateConstraintResponse = CreateConstraintResponse'
  { -- | Information about the constraint.
    constraintDetail :: Core.Maybe Types.ConstraintDetail,
    -- | The constraint parameters.
    constraintParameters :: Core.Maybe Types.ConstraintParameters,
    -- | The status of the current request.
    status :: Core.Maybe Types.RequestStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConstraintResponse' value with any optional fields omitted.
mkCreateConstraintResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateConstraintResponse
mkCreateConstraintResponse responseStatus =
  CreateConstraintResponse'
    { constraintDetail = Core.Nothing,
      constraintParameters = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsConstraintDetail :: Lens.Lens' CreateConstraintResponse (Core.Maybe Types.ConstraintDetail)
ccrrsConstraintDetail = Lens.field @"constraintDetail"
{-# DEPRECATED ccrrsConstraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead." #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsConstraintParameters :: Lens.Lens' CreateConstraintResponse (Core.Maybe Types.ConstraintParameters)
ccrrsConstraintParameters = Lens.field @"constraintParameters"
{-# DEPRECATED ccrrsConstraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead." #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsStatus :: Lens.Lens' CreateConstraintResponse (Core.Maybe Types.RequestStatus)
ccrrsStatus = Lens.field @"status"
{-# DEPRECATED ccrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateConstraintResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
