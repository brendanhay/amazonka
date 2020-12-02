{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSummary where

import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The StackSummary Data Type
--
--
--
-- /See:/ 'stackSummary' smart constructor.
data StackSummary = StackSummary'
  { _ssLastUpdatedTime ::
      !(Maybe ISO8601),
    _ssRootId :: !(Maybe Text),
    _ssStackStatusReason :: !(Maybe Text),
    _ssTemplateDescription :: !(Maybe Text),
    _ssDriftInformation :: !(Maybe StackDriftInformationSummary),
    _ssDeletionTime :: !(Maybe ISO8601),
    _ssStackId :: !(Maybe Text),
    _ssParentId :: !(Maybe Text),
    _ssStackName :: !Text,
    _ssCreationTime :: !ISO8601,
    _ssStackStatus :: !StackStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- * 'ssRootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssStackStatusReason' - Success/Failure message associated with the stack status.
--
-- * 'ssTemplateDescription' - The template description of the template used to create the stack.
--
-- * 'ssDriftInformation' - Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'ssDeletionTime' - The time the stack was deleted.
--
-- * 'ssStackId' - Unique stack identifier.
--
-- * 'ssParentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssStackName' - The name associated with the stack.
--
-- * 'ssCreationTime' - The time the stack was created.
--
-- * 'ssStackStatus' - The current status of the stack.
stackSummary ::
  -- | 'ssStackName'
  Text ->
  -- | 'ssCreationTime'
  UTCTime ->
  -- | 'ssStackStatus'
  StackStatus ->
  StackSummary
stackSummary pStackName_ pCreationTime_ pStackStatus_ =
  StackSummary'
    { _ssLastUpdatedTime = Nothing,
      _ssRootId = Nothing,
      _ssStackStatusReason = Nothing,
      _ssTemplateDescription = Nothing,
      _ssDriftInformation = Nothing,
      _ssDeletionTime = Nothing,
      _ssStackId = Nothing,
      _ssParentId = Nothing,
      _ssStackName = pStackName_,
      _ssCreationTime = _Time # pCreationTime_,
      _ssStackStatus = pStackStatus_
    }

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe UTCTime)
ssLastUpdatedTime = lens _ssLastUpdatedTime (\s a -> s {_ssLastUpdatedTime = a}) . mapping _Time

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
ssRootId :: Lens' StackSummary (Maybe Text)
ssRootId = lens _ssRootId (\s a -> s {_ssRootId = a})

-- | Success/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason = lens _ssStackStatusReason (\s a -> s {_ssStackStatusReason = a})

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription = lens _ssTemplateDescription (\s a -> s {_ssTemplateDescription = a})

-- | Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
ssDriftInformation :: Lens' StackSummary (Maybe StackDriftInformationSummary)
ssDriftInformation = lens _ssDriftInformation (\s a -> s {_ssDriftInformation = a})

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe UTCTime)
ssDeletionTime = lens _ssDeletionTime (\s a -> s {_ssDeletionTime = a}) . mapping _Time

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s {_ssStackId = a})

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
ssParentId :: Lens' StackSummary (Maybe Text)
ssParentId = lens _ssParentId (\s a -> s {_ssParentId = a})

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\s a -> s {_ssStackName = a})

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary UTCTime
ssCreationTime = lens _ssCreationTime (\s a -> s {_ssCreationTime = a}) . _Time

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\s a -> s {_ssStackStatus = a})

instance FromXML StackSummary where
  parseXML x =
    StackSummary'
      <$> (x .@? "LastUpdatedTime")
      <*> (x .@? "RootId")
      <*> (x .@? "StackStatusReason")
      <*> (x .@? "TemplateDescription")
      <*> (x .@? "DriftInformation")
      <*> (x .@? "DeletionTime")
      <*> (x .@? "StackId")
      <*> (x .@? "ParentId")
      <*> (x .@ "StackName")
      <*> (x .@ "CreationTime")
      <*> (x .@ "StackStatus")

instance Hashable StackSummary

instance NFData StackSummary
