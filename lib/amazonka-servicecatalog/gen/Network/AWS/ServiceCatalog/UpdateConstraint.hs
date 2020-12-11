{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateConstraint (..),
    mkUpdateConstraint,

    -- ** Request lenses
    ucAcceptLanguage,
    ucParameters,
    ucDescription,
    ucId,

    -- * Destructuring the response
    UpdateConstraintResponse (..),
    mkUpdateConstraintResponse,

    -- ** Response lenses
    ucrsStatus,
    ucrsConstraintDetail,
    ucrsConstraintParameters,
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConstraint' with the minimum fields required to make a request.
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
-- * 'description' - The updated description of the constraint.
-- * 'id' - The identifier of the constraint.
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
mkUpdateConstraint ::
  -- | 'id'
  Lude.Text ->
  UpdateConstraint
mkUpdateConstraint pId_ =
  UpdateConstraint'
    { acceptLanguage = Lude.Nothing,
      parameters = Lude.Nothing,
      description = Lude.Nothing,
      id = pId_
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
ucAcceptLanguage :: Lens.Lens' UpdateConstraint (Lude.Maybe Lude.Text)
ucAcceptLanguage = Lens.lens (acceptLanguage :: UpdateConstraint -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateConstraint)
{-# DEPRECATED ucAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

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
ucParameters :: Lens.Lens' UpdateConstraint (Lude.Maybe Lude.Text)
ucParameters = Lens.lens (parameters :: UpdateConstraint -> Lude.Maybe Lude.Text) (\s a -> s {parameters = a} :: UpdateConstraint)
{-# DEPRECATED ucParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The updated description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateConstraint (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateConstraint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateConstraint)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucId :: Lens.Lens' UpdateConstraint Lude.Text
ucId = Lens.lens (id :: UpdateConstraint -> Lude.Text) (\s a -> s {id = a} :: UpdateConstraint)
{-# DEPRECATED ucId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateConstraint where
  type Rs UpdateConstraint = UpdateConstraintResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateConstraintResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ConstraintDetail")
            Lude.<*> (x Lude..?> "ConstraintParameters")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConstraint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.UpdateConstraint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConstraint where
  toJSON UpdateConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdateConstraint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConstraint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
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

-- | Creates a value of 'UpdateConstraintResponse' with the minimum fields required to make a request.
--
-- * 'constraintDetail' - Information about the constraint.
-- * 'constraintParameters' - The constraint parameters.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the current request.
mkUpdateConstraintResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConstraintResponse
mkUpdateConstraintResponse pResponseStatus_ =
  UpdateConstraintResponse'
    { status = Lude.Nothing,
      constraintDetail = Lude.Nothing,
      constraintParameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsStatus :: Lens.Lens' UpdateConstraintResponse (Lude.Maybe RequestStatus)
ucrsStatus = Lens.lens (status :: UpdateConstraintResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: UpdateConstraintResponse)
{-# DEPRECATED ucrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsConstraintDetail :: Lens.Lens' UpdateConstraintResponse (Lude.Maybe ConstraintDetail)
ucrsConstraintDetail = Lens.lens (constraintDetail :: UpdateConstraintResponse -> Lude.Maybe ConstraintDetail) (\s a -> s {constraintDetail = a} :: UpdateConstraintResponse)
{-# DEPRECATED ucrsConstraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead." #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsConstraintParameters :: Lens.Lens' UpdateConstraintResponse (Lude.Maybe Lude.Text)
ucrsConstraintParameters = Lens.lens (constraintParameters :: UpdateConstraintResponse -> Lude.Maybe Lude.Text) (\s a -> s {constraintParameters = a} :: UpdateConstraintResponse)
{-# DEPRECATED ucrsConstraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateConstraintResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateConstraintResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConstraintResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
