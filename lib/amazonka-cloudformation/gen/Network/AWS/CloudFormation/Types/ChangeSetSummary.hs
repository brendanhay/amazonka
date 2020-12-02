{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetSummary where

import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @ChangeSetSummary@ structure describes a change set, its status, and the stack with which it's associated.
--
--
--
-- /See:/ 'changeSetSummary' smart constructor.
data ChangeSetSummary = ChangeSetSummary'
  { _cCreationTime ::
      !(Maybe ISO8601),
    _cStatus :: !(Maybe ChangeSetStatus),
    _cParentChangeSetId :: !(Maybe Text),
    _cChangeSetName :: !(Maybe Text),
    _cExecutionStatus :: !(Maybe ExecutionStatus),
    _cChangeSetId :: !(Maybe Text),
    _cIncludeNestedStacks :: !(Maybe Bool),
    _cRootChangeSetId :: !(Maybe Text),
    _cStatusReason :: !(Maybe Text),
    _cStackId :: !(Maybe Text),
    _cDescription :: !(Maybe Text),
    _cStackName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'cStatus' - The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
--
-- * 'cParentChangeSetId' - The parent change set ID.
--
-- * 'cChangeSetName' - The name of the change set.
--
-- * 'cExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- * 'cChangeSetId' - The ID of the change set.
--
-- * 'cIncludeNestedStacks' - Specifies the current setting of @IncludeNestedStacks@ for the change set.
--
-- * 'cRootChangeSetId' - The root change set ID.
--
-- * 'cStatusReason' - A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
--
-- * 'cStackId' - The ID of the stack with which the change set is associated.
--
-- * 'cDescription' - Descriptive information about the change set.
--
-- * 'cStackName' - The name of the stack with which the change set is associated.
changeSetSummary ::
  ChangeSetSummary
changeSetSummary =
  ChangeSetSummary'
    { _cCreationTime = Nothing,
      _cStatus = Nothing,
      _cParentChangeSetId = Nothing,
      _cChangeSetName = Nothing,
      _cExecutionStatus = Nothing,
      _cChangeSetId = Nothing,
      _cIncludeNestedStacks = Nothing,
      _cRootChangeSetId = Nothing,
      _cStatusReason = Nothing,
      _cStackId = Nothing,
      _cDescription = Nothing,
      _cStackName = Nothing
    }

-- | The start time when the change set was created, in UTC.
cCreationTime :: Lens' ChangeSetSummary (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\s a -> s {_cCreationTime = a}) . mapping _Time

-- | The state of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
cStatus :: Lens' ChangeSetSummary (Maybe ChangeSetStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The parent change set ID.
cParentChangeSetId :: Lens' ChangeSetSummary (Maybe Text)
cParentChangeSetId = lens _cParentChangeSetId (\s a -> s {_cParentChangeSetId = a})

-- | The name of the change set.
cChangeSetName :: Lens' ChangeSetSummary (Maybe Text)
cChangeSetName = lens _cChangeSetName (\s a -> s {_cChangeSetName = a})

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
cExecutionStatus :: Lens' ChangeSetSummary (Maybe ExecutionStatus)
cExecutionStatus = lens _cExecutionStatus (\s a -> s {_cExecutionStatus = a})

-- | The ID of the change set.
cChangeSetId :: Lens' ChangeSetSummary (Maybe Text)
cChangeSetId = lens _cChangeSetId (\s a -> s {_cChangeSetId = a})

-- | Specifies the current setting of @IncludeNestedStacks@ for the change set.
cIncludeNestedStacks :: Lens' ChangeSetSummary (Maybe Bool)
cIncludeNestedStacks = lens _cIncludeNestedStacks (\s a -> s {_cIncludeNestedStacks = a})

-- | The root change set ID.
cRootChangeSetId :: Lens' ChangeSetSummary (Maybe Text)
cRootChangeSetId = lens _cRootChangeSetId (\s a -> s {_cRootChangeSetId = a})

-- | A description of the change set's status. For example, if your change set is in the @FAILED@ state, AWS CloudFormation shows the error message.
cStatusReason :: Lens' ChangeSetSummary (Maybe Text)
cStatusReason = lens _cStatusReason (\s a -> s {_cStatusReason = a})

-- | The ID of the stack with which the change set is associated.
cStackId :: Lens' ChangeSetSummary (Maybe Text)
cStackId = lens _cStackId (\s a -> s {_cStackId = a})

-- | Descriptive information about the change set.
cDescription :: Lens' ChangeSetSummary (Maybe Text)
cDescription = lens _cDescription (\s a -> s {_cDescription = a})

-- | The name of the stack with which the change set is associated.
cStackName :: Lens' ChangeSetSummary (Maybe Text)
cStackName = lens _cStackName (\s a -> s {_cStackName = a})

instance FromXML ChangeSetSummary where
  parseXML x =
    ChangeSetSummary'
      <$> (x .@? "CreationTime")
      <*> (x .@? "Status")
      <*> (x .@? "ParentChangeSetId")
      <*> (x .@? "ChangeSetName")
      <*> (x .@? "ExecutionStatus")
      <*> (x .@? "ChangeSetId")
      <*> (x .@? "IncludeNestedStacks")
      <*> (x .@? "RootChangeSetId")
      <*> (x .@? "StatusReason")
      <*> (x .@? "StackId")
      <*> (x .@? "Description")
      <*> (x .@? "StackName")

instance Hashable ChangeSetSummary

instance NFData ChangeSetSummary
