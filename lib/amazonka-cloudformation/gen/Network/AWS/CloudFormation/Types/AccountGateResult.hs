{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountGateResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AccountGateResult where

import Network.AWS.CloudFormation.Types.AccountGateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and Region.
--
--
-- For each account and Region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and Region. CloudFormation invokes the function each time a stack set operation is requested for that account and Region; if the function returns @FAILED@ , CloudFormation cancels the operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html Configuring a target account gate> .
--
--
-- /See:/ 'accountGateResult' smart constructor.
data AccountGateResult = AccountGateResult'
  { _agrStatus ::
      !(Maybe AccountGateStatus),
    _agrStatusReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountGateResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agrStatus' - The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and Region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and Region.      * @FAILED@ : The account gate function has determined that the account and Region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and Region, for one of the following reasons:     * An account gate function has not been specified for the account and Region. AWS CloudFormation proceeds with the stack set operation in this account and Region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and Region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and Region.
--
-- * 'agrStatusReason' - The reason for the account gate status assigned to this account and Region for the stack set operation.
accountGateResult ::
  AccountGateResult
accountGateResult =
  AccountGateResult'
    { _agrStatus = Nothing,
      _agrStatusReason = Nothing
    }

-- | The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and Region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and Region.      * @FAILED@ : The account gate function has determined that the account and Region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and Region, for one of the following reasons:     * An account gate function has not been specified for the account and Region. AWS CloudFormation proceeds with the stack set operation in this account and Region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and Region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and Region.
agrStatus :: Lens' AccountGateResult (Maybe AccountGateStatus)
agrStatus = lens _agrStatus (\s a -> s {_agrStatus = a})

-- | The reason for the account gate status assigned to this account and Region for the stack set operation.
agrStatusReason :: Lens' AccountGateResult (Maybe Text)
agrStatusReason = lens _agrStatusReason (\s a -> s {_agrStatusReason = a})

instance FromXML AccountGateResult where
  parseXML x =
    AccountGateResult'
      <$> (x .@? "Status") <*> (x .@? "StatusReason")

instance Hashable AccountGateResult

instance NFData AccountGateResult
