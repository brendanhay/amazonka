{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccAcceptLanguage,
    ccDescription,
    ccPortfolioId,
    ccProductId,
    ccParameters,
    ccType,
    ccIdempotencyToken,

    -- * Destructuring the response
    CreateConstraintResponse (..),
    mkCreateConstraintResponse,

    -- ** Response lenses
    ccrsStatus,
    ccrsConstraintDetail,
    ccrsConstraintParameters,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreateConstraint' smart constructor.
data CreateConstraint = CreateConstraint'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    portfolioId :: Lude.Text,
    productId :: Lude.Text,
    parameters :: Lude.Text,
    type' :: Lude.Text,
    idempotencyToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConstraint' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
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
-- * 'description' - The description of the constraint.
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'parameters' - The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:
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
-- * 'portfolioId' - The portfolio identifier.
-- * 'productId' - The product identifier.
-- * 'type'' - The type of constraint.
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
mkCreateConstraint ::
  -- | 'portfolioId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  -- | 'parameters'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'idempotencyToken'
  Lude.Text ->
  CreateConstraint
mkCreateConstraint
  pPortfolioId_
  pProductId_
  pParameters_
  pType_
  pIdempotencyToken_ =
    CreateConstraint'
      { acceptLanguage = Lude.Nothing,
        description = Lude.Nothing,
        portfolioId = pPortfolioId_,
        productId = pProductId_,
        parameters = pParameters_,
        type' = pType_,
        idempotencyToken = pIdempotencyToken_
      }

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
ccAcceptLanguage :: Lens.Lens' CreateConstraint (Lude.Maybe Lude.Text)
ccAcceptLanguage = Lens.lens (acceptLanguage :: CreateConstraint -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreateConstraint)
{-# DEPRECATED ccAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateConstraint (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: CreateConstraint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateConstraint)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPortfolioId :: Lens.Lens' CreateConstraint Lude.Text
ccPortfolioId = Lens.lens (portfolioId :: CreateConstraint -> Lude.Text) (\s a -> s {portfolioId = a} :: CreateConstraint)
{-# DEPRECATED ccPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccProductId :: Lens.Lens' CreateConstraint Lude.Text
ccProductId = Lens.lens (productId :: CreateConstraint -> Lude.Text) (\s a -> s {productId = a} :: CreateConstraint)
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
ccParameters :: Lens.Lens' CreateConstraint Lude.Text
ccParameters = Lens.lens (parameters :: CreateConstraint -> Lude.Text) (\s a -> s {parameters = a} :: CreateConstraint)
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
ccType :: Lens.Lens' CreateConstraint Lude.Text
ccType = Lens.lens (type' :: CreateConstraint -> Lude.Text) (\s a -> s {type' = a} :: CreateConstraint)
{-# DEPRECATED ccType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIdempotencyToken :: Lens.Lens' CreateConstraint Lude.Text
ccIdempotencyToken = Lens.lens (idempotencyToken :: CreateConstraint -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateConstraint)
{-# DEPRECATED ccIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Lude.AWSRequest CreateConstraint where
  type Rs CreateConstraint = CreateConstraintResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConstraintResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ConstraintDetail")
            Lude.<*> (x Lude..?> "ConstraintParameters")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConstraint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.CreateConstraint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConstraint where
  toJSON CreateConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("PortfolioId" Lude..= portfolioId),
            Lude.Just ("ProductId" Lude..= productId),
            Lude.Just ("Parameters" Lude..= parameters),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("IdempotencyToken" Lude..= idempotencyToken)
          ]
      )

instance Lude.ToPath CreateConstraint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConstraint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConstraintResponse' smart constructor.
data CreateConstraintResponse = CreateConstraintResponse'
  { status ::
      Lude.Maybe RequestStatus,
    constraintDetail ::
      Lude.Maybe ConstraintDetail,
    constraintParameters ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConstraintResponse' with the minimum fields required to make a request.
--
-- * 'constraintDetail' - Information about the constraint.
-- * 'constraintParameters' - The constraint parameters.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the current request.
mkCreateConstraintResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConstraintResponse
mkCreateConstraintResponse pResponseStatus_ =
  CreateConstraintResponse'
    { status = Lude.Nothing,
      constraintDetail = Lude.Nothing,
      constraintParameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsStatus :: Lens.Lens' CreateConstraintResponse (Lude.Maybe RequestStatus)
ccrsStatus = Lens.lens (status :: CreateConstraintResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: CreateConstraintResponse)
{-# DEPRECATED ccrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsConstraintDetail :: Lens.Lens' CreateConstraintResponse (Lude.Maybe ConstraintDetail)
ccrsConstraintDetail = Lens.lens (constraintDetail :: CreateConstraintResponse -> Lude.Maybe ConstraintDetail) (\s a -> s {constraintDetail = a} :: CreateConstraintResponse)
{-# DEPRECATED ccrsConstraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead." #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsConstraintParameters :: Lens.Lens' CreateConstraintResponse (Lude.Maybe Lude.Text)
ccrsConstraintParameters = Lens.lens (constraintParameters :: CreateConstraintResponse -> Lude.Maybe Lude.Text) (\s a -> s {constraintParameters = a} :: CreateConstraintResponse)
{-# DEPRECATED ccrsConstraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateConstraintResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateConstraintResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConstraintResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
