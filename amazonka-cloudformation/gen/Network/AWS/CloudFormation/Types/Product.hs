{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Product where

import Network.AWS.CloudFormation.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and region.
--
--
-- For each account and region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and region. CloudFormation invokes the function each time a stack set operation is requested for that account and region; if the function returns @FAILED@ , CloudFormation cancels the operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html Configuring a target account gate> .
--
--
-- /See:/ 'accountGateResult' smart constructor.
data AccountGateResult = AccountGateResult'
  { _agrStatus       :: !(Maybe AccountGateStatus)
  , _agrStatusReason :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountGateResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agrStatus' - The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and region.      * @FAILED@ : The account gate function has determined that the account and region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and region, for one of the following reasons:     * An account gate function has not been specified for the account and region. AWS CloudFormation proceeds with the stack set operation in this account and region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and region.
--
-- * 'agrStatusReason' - The reason for the account gate status assigned to this account and region for the stack set operation.
accountGateResult
    :: AccountGateResult
accountGateResult =
  AccountGateResult' {_agrStatus = Nothing, _agrStatusReason = Nothing}


-- | The status of the account gate function.     * @SUCCEEDED@ : The account gate function has determined that the account and region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and region.      * @FAILED@ : The account gate function has determined that the account and region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and region, and sets the stack set operation result status for that account and region to @FAILED@ .      * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and region, for one of the following reasons:     * An account gate function has not been specified for the account and region. AWS CloudFormation proceeds with the stack set operation in this account and region.     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and region.     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and region.
agrStatus :: Lens' AccountGateResult (Maybe AccountGateStatus)
agrStatus = lens _agrStatus (\ s a -> s{_agrStatus = a})

-- | The reason for the account gate status assigned to this account and region for the stack set operation.
agrStatusReason :: Lens' AccountGateResult (Maybe Text)
agrStatusReason = lens _agrStatusReason (\ s a -> s{_agrStatusReason = a})

instance FromXML AccountGateResult where
        parseXML x
          = AccountGateResult' <$>
              (x .@? "Status") <*> (x .@? "StatusReason")

instance Hashable AccountGateResult where

instance NFData AccountGateResult where

-- | The AccountLimit data type.
--
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { _alValue :: !(Maybe Int)
  , _alName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alValue' - The value that is associated with the account limit name.
--
-- * 'alName' - The name of the account limit. Currently, the only account limit is @StackLimit@ .
accountLimit
    :: AccountLimit
accountLimit = AccountLimit' {_alValue = Nothing, _alName = Nothing}


-- | The value that is associated with the account limit name.
alValue :: Lens' AccountLimit (Maybe Int)
alValue = lens _alValue (\ s a -> s{_alValue = a})

-- | The name of the account limit. Currently, the only account limit is @StackLimit@ .
alName :: Lens' AccountLimit (Maybe Text)
alName = lens _alName (\ s a -> s{_alName = a})

instance FromXML AccountLimit where
        parseXML x
          = AccountLimit' <$>
              (x .@? "Value") <*> (x .@? "Name")

instance Hashable AccountLimit where

instance NFData AccountLimit where

-- | The @Change@ structure describes the changes AWS CloudFormation will perform if you execute the change set.
--
--
--
-- /See:/ 'change' smart constructor.
data Change = Change'
  { _cResourceChange :: !(Maybe ResourceChange)
  , _cType           :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResourceChange' - A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
--
-- * 'cType' - The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
change
    :: Change
change = Change' {_cResourceChange = Nothing, _cType = Nothing}


-- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
cResourceChange :: Lens' Change (Maybe ResourceChange)
cResourceChange = lens _cResourceChange (\ s a -> s{_cResourceChange = a})

-- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
cType :: Lens' Change (Maybe ChangeType)
cType = lens _cType (\ s a -> s{_cType = a})

instance FromXML Change where
        parseXML x
          = Change' <$>
              (x .@? "ResourceChange") <*> (x .@? "Type")

instance Hashable Change where

instance NFData Change where

-- | The @ChangeSetSummary@ structure describes a change set, its status, and the stack with which it's associated.
--
--
--
-- /See:/ 'changeSetSummary' smart constructor.
data ChangeSetSummary = ChangeSetSummary'
  { _cCreationTime    :: !(Maybe ISO8601)
  , _cStatus          :: !(Maybe ChangeSetStatus)
  , _cChangeSetName   :: !(Maybe Text)
  , _cExecutionStatus :: !(Maybe ExecutionStatus)
  , _cChangeSetId     :: !(Maybe Text)
  , _cStatusReason    :: !(Maybe Text)
  , _cStackId         :: !(Maybe Text)
  , _cDescription     :: !(Maybe Text)
  , _cStackName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'cStatus' - The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- * 'cChangeSetName' - The name of the change set.
--
-- * 'cExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can
