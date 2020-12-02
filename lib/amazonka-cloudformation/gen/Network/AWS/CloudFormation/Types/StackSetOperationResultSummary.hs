{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationResultSummary where

import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure that contains information about a specified operation's results for a given account in a given Region.
--
--
--
-- /See:/ 'stackSetOperationResultSummary' smart constructor.
data StackSetOperationResultSummary = StackSetOperationResultSummary'
  { _ssorsStatus ::
      !( Maybe
           StackSetOperationResultStatus
       ),
    _ssorsAccount ::
      !(Maybe Text),
    _ssorsAccountGateResult ::
      !(Maybe AccountGateResult),
    _ssorsOrganizationalUnitId ::
      !(Maybe Text),
    _ssorsRegion :: !(Maybe Text),
    _ssorsStatusReason ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetOperationResultSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssorsStatus' - The result status of the stack set operation for the given account in the given Region.     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and Region failed.  If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.      * @RUNNING@ : The operation in the specified account and Region is currently in progress.     * @PENDING@ : The operation in the specified account and Region has yet to start.      * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
-- * 'ssorsAccount' - [@Self-managed@ permissions] The name of the AWS account for this operation result.
--
-- * 'ssorsAccountGateResult' - The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
--
-- * 'ssorsOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- * 'ssorsRegion' - The name of the AWS Region for this operation result.
--
-- * 'ssorsStatusReason' - The reason for the assigned result status.
stackSetOperationResultSummary ::
  StackSetOperationResultSummary
stackSetOperationResultSummary =
  StackSetOperationResultSummary'
    { _ssorsStatus = Nothing,
      _ssorsAccount = Nothing,
      _ssorsAccountGateResult = Nothing,
      _ssorsOrganizationalUnitId = Nothing,
      _ssorsRegion = Nothing,
      _ssorsStatusReason = Nothing
    }

-- | The result status of the stack set operation for the given account in the given Region.     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and Region failed.  If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.      * @RUNNING@ : The operation in the specified account and Region is currently in progress.     * @PENDING@ : The operation in the specified account and Region has yet to start.      * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
ssorsStatus :: Lens' StackSetOperationResultSummary (Maybe StackSetOperationResultStatus)
ssorsStatus = lens _ssorsStatus (\s a -> s {_ssorsStatus = a})

-- | [@Self-managed@ permissions] The name of the AWS account for this operation result.
ssorsAccount :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsAccount = lens _ssorsAccount (\s a -> s {_ssorsAccount = a})

-- | The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
ssorsAccountGateResult :: Lens' StackSetOperationResultSummary (Maybe AccountGateResult)
ssorsAccountGateResult = lens _ssorsAccountGateResult (\s a -> s {_ssorsAccountGateResult = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
ssorsOrganizationalUnitId :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsOrganizationalUnitId = lens _ssorsOrganizationalUnitId (\s a -> s {_ssorsOrganizationalUnitId = a})

-- | The name of the AWS Region for this operation result.
ssorsRegion :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsRegion = lens _ssorsRegion (\s a -> s {_ssorsRegion = a})

-- | The reason for the assigned result status.
ssorsStatusReason :: Lens' StackSetOperationResultSummary (Maybe Text)
ssorsStatusReason = lens _ssorsStatusReason (\s a -> s {_ssorsStatusReason = a})

instance FromXML StackSetOperationResultSummary where
  parseXML x =
    StackSetOperationResultSummary'
      <$> (x .@? "Status")
      <*> (x .@? "Account")
      <*> (x .@? "AccountGateResult")
      <*> (x .@? "OrganizationalUnitId")
      <*> (x .@? "Region")
      <*> (x .@? "StatusReason")

instance Hashable StackSetOperationResultSummary

instance NFData StackSetOperationResultSummary
