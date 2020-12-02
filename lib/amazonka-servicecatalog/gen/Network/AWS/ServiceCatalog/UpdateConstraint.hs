{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    updateConstraint,
    UpdateConstraint,

    -- * Request Lenses
    ucAcceptLanguage,
    ucParameters,
    ucDescription,
    ucId,

    -- * Destructuring the Response
    updateConstraintResponse,
    UpdateConstraintResponse,

    -- * Response Lenses
    ucrsStatus,
    ucrsConstraintDetail,
    ucrsConstraintParameters,
    ucrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'updateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { _ucAcceptLanguage ::
      !(Maybe Text),
    _ucParameters :: !(Maybe Text),
    _ucDescription :: !(Maybe Text),
    _ucId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'ucParameters' - The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:     * LAUNCH    * You are required to specify either the @RoleArn@ or the @LocalRoleName@ but can't use both. Specify the @RoleArn@ property as follows: @{"RoleArn" : "arn:aws:iam::123456789012:role/LaunchRole"}@  Specify the @LocalRoleName@ property as follows: @{"LocalRoleName": "SCBasicLaunchRole"}@  If you specify the @LocalRoleName@ property, when an account uses the launch constraint, the IAM role with that name in the account will be used. This allows launch-role constraints to be account-agnostic so the administrator can create fewer resources per shared account. You cannot have both a @LAUNCH@ and a @STACKSET@ constraint. You also cannot have more than one @LAUNCH@ constraint on a product and portfolio.     * NOTIFICATION    * Specify the @NotificationArns@ property as follows: @{"NotificationArns" : ["arn:aws:sns:us-east-1:123456789012:Topic"]}@      * RESOURCE_UPDATE    * Specify the @TagUpdatesOnProvisionedProduct@ property as follows: @{"Version":"2.0","Properties":{"TagUpdateOnProvisionedProduct":"String"}}@  The @TagUpdatesOnProvisionedProduct@ property accepts a string value of @ALLOWED@ or @NOT_ALLOWED@ .     * STACKSET    * Specify the @Parameters@ property as follows: @{"Version": "String", "Properties": {"AccountList": [ "String" ], "RegionList": [ "String" ], "AdminRole": "String", "ExecutionRole": "String"}}@  You cannot have both a @LAUNCH@ and a @STACKSET@ constraint. You also cannot have more than one @STACKSET@ constraint on a product and portfolio. Products with a @STACKSET@ constraint will launch an AWS CloudFormation stack set.     * TEMPLATE    * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
--
-- * 'ucDescription' - The updated description of the constraint.
--
-- * 'ucId' - The identifier of the constraint.
updateConstraint ::
  -- | 'ucId'
  Text ->
  UpdateConstraint
updateConstraint pId_ =
  UpdateConstraint'
    { _ucAcceptLanguage = Nothing,
      _ucParameters = Nothing,
      _ucDescription = Nothing,
      _ucId = pId_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
ucAcceptLanguage :: Lens' UpdateConstraint (Maybe Text)
ucAcceptLanguage = lens _ucAcceptLanguage (\s a -> s {_ucAcceptLanguage = a})

-- | The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:     * LAUNCH    * You are required to specify either the @RoleArn@ or the @LocalRoleName@ but can't use both. Specify the @RoleArn@ property as follows: @{"RoleArn" : "arn:aws:iam::123456789012:role/LaunchRole"}@  Specify the @LocalRoleName@ property as follows: @{"LocalRoleName": "SCBasicLaunchRole"}@  If you specify the @LocalRoleName@ property, when an account uses the launch constraint, the IAM role with that name in the account will be used. This allows launch-role constraints to be account-agnostic so the administrator can create fewer resources per shared account. You cannot have both a @LAUNCH@ and a @STACKSET@ constraint. You also cannot have more than one @LAUNCH@ constraint on a product and portfolio.     * NOTIFICATION    * Specify the @NotificationArns@ property as follows: @{"NotificationArns" : ["arn:aws:sns:us-east-1:123456789012:Topic"]}@      * RESOURCE_UPDATE    * Specify the @TagUpdatesOnProvisionedProduct@ property as follows: @{"Version":"2.0","Properties":{"TagUpdateOnProvisionedProduct":"String"}}@  The @TagUpdatesOnProvisionedProduct@ property accepts a string value of @ALLOWED@ or @NOT_ALLOWED@ .     * STACKSET    * Specify the @Parameters@ property as follows: @{"Version": "String", "Properties": {"AccountList": [ "String" ], "RegionList": [ "String" ], "AdminRole": "String", "ExecutionRole": "String"}}@  You cannot have both a @LAUNCH@ and a @STACKSET@ constraint. You also cannot have more than one @STACKSET@ constraint on a product and portfolio. Products with a @STACKSET@ constraint will launch an AWS CloudFormation stack set.     * TEMPLATE    * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
ucParameters :: Lens' UpdateConstraint (Maybe Text)
ucParameters = lens _ucParameters (\s a -> s {_ucParameters = a})

-- | The updated description of the constraint.
ucDescription :: Lens' UpdateConstraint (Maybe Text)
ucDescription = lens _ucDescription (\s a -> s {_ucDescription = a})

-- | The identifier of the constraint.
ucId :: Lens' UpdateConstraint Text
ucId = lens _ucId (\s a -> s {_ucId = a})

instance AWSRequest UpdateConstraint where
  type Rs UpdateConstraint = UpdateConstraintResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          UpdateConstraintResponse'
            <$> (x .?> "Status")
            <*> (x .?> "ConstraintDetail")
            <*> (x .?> "ConstraintParameters")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateConstraint

instance NFData UpdateConstraint

instance ToHeaders UpdateConstraint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.UpdateConstraint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateConstraint where
  toJSON UpdateConstraint' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _ucAcceptLanguage,
            ("Parameters" .=) <$> _ucParameters,
            ("Description" .=) <$> _ucDescription,
            Just ("Id" .= _ucId)
          ]
      )

instance ToPath UpdateConstraint where
  toPath = const "/"

instance ToQuery UpdateConstraint where
  toQuery = const mempty

-- | /See:/ 'updateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
  { _ucrsStatus ::
      !(Maybe RequestStatus),
    _ucrsConstraintDetail ::
      !(Maybe ConstraintDetail),
    _ucrsConstraintParameters ::
      !(Maybe Text),
    _ucrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateConstraintResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsStatus' - The status of the current request.
--
-- * 'ucrsConstraintDetail' - Information about the constraint.
--
-- * 'ucrsConstraintParameters' - The constraint parameters.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateConstraintResponse ::
  -- | 'ucrsResponseStatus'
  Int ->
  UpdateConstraintResponse
updateConstraintResponse pResponseStatus_ =
  UpdateConstraintResponse'
    { _ucrsStatus = Nothing,
      _ucrsConstraintDetail = Nothing,
      _ucrsConstraintParameters = Nothing,
      _ucrsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
ucrsStatus :: Lens' UpdateConstraintResponse (Maybe RequestStatus)
ucrsStatus = lens _ucrsStatus (\s a -> s {_ucrsStatus = a})

-- | Information about the constraint.
ucrsConstraintDetail :: Lens' UpdateConstraintResponse (Maybe ConstraintDetail)
ucrsConstraintDetail = lens _ucrsConstraintDetail (\s a -> s {_ucrsConstraintDetail = a})

-- | The constraint parameters.
ucrsConstraintParameters :: Lens' UpdateConstraintResponse (Maybe Text)
ucrsConstraintParameters = lens _ucrsConstraintParameters (\s a -> s {_ucrsConstraintParameters = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateConstraintResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\s a -> s {_ucrsResponseStatus = a})

instance NFData UpdateConstraintResponse
