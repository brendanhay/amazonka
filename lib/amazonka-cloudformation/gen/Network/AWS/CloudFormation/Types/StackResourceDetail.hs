{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDetail where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains detailed information about the specified stack resource.
--
--
--
-- /See:/ 'stackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
  { _sPhysicalResourceId ::
      !(Maybe Text),
    _sResourceStatusReason :: !(Maybe Text),
    _sDriftInformation ::
      !(Maybe StackResourceDriftInformation),
    _sModuleInfo :: !(Maybe ModuleInfo),
    _sMetadata :: !(Maybe Text),
    _sStackId :: !(Maybe Text),
    _sDescription :: !(Maybe Text),
    _sStackName :: !(Maybe Text),
    _sLogicalResourceId :: !Text,
    _sResourceType :: !Text,
    _sLastUpdatedTimestamp :: !ISO8601,
    _sResourceStatus :: !ResourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'sResourceStatusReason' - Success/failure message associated with the resource.
--
-- * 'sDriftInformation' - Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- * 'sModuleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- * 'sMetadata' - The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
--
-- * 'sStackId' - Unique identifier of the stack.
--
-- * 'sDescription' - User defined description associated with the resource.
--
-- * 'sStackName' - The name associated with the stack.
--
-- * 'sLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'sResourceType' - Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- * 'sLastUpdatedTimestamp' - Time the status was updated.
--
-- * 'sResourceStatus' - Current status of the resource.
stackResourceDetail ::
  -- | 'sLogicalResourceId'
  Text ->
  -- | 'sResourceType'
  Text ->
  -- | 'sLastUpdatedTimestamp'
  UTCTime ->
  -- | 'sResourceStatus'
  ResourceStatus ->
  StackResourceDetail
stackResourceDetail
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceDetail'
      { _sPhysicalResourceId = Nothing,
        _sResourceStatusReason = Nothing,
        _sDriftInformation = Nothing,
        _sModuleInfo = Nothing,
        _sMetadata = Nothing,
        _sStackId = Nothing,
        _sDescription = Nothing,
        _sStackName = Nothing,
        _sLogicalResourceId = pLogicalResourceId_,
        _sResourceType = pResourceType_,
        _sLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_,
        _sResourceStatus = pResourceStatus_
      }

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
sPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
sPhysicalResourceId = lens _sPhysicalResourceId (\s a -> s {_sPhysicalResourceId = a})

-- | Success/failure message associated with the resource.
sResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
sResourceStatusReason = lens _sResourceStatusReason (\s a -> s {_sResourceStatusReason = a})

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
sDriftInformation :: Lens' StackResourceDetail (Maybe StackResourceDriftInformation)
sDriftInformation = lens _sDriftInformation (\s a -> s {_sDriftInformation = a})

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
sModuleInfo :: Lens' StackResourceDetail (Maybe ModuleInfo)
sModuleInfo = lens _sModuleInfo (\s a -> s {_sModuleInfo = a})

-- | The content of the @Metadata@ attribute declared for the resource. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute> in the AWS CloudFormation User Guide.
sMetadata :: Lens' StackResourceDetail (Maybe Text)
sMetadata = lens _sMetadata (\s a -> s {_sMetadata = a})

-- | Unique identifier of the stack.
sStackId :: Lens' StackResourceDetail (Maybe Text)
sStackId = lens _sStackId (\s a -> s {_sStackId = a})

-- | User defined description associated with the resource.
sDescription :: Lens' StackResourceDetail (Maybe Text)
sDescription = lens _sDescription (\s a -> s {_sDescription = a})

-- | The name associated with the stack.
sStackName :: Lens' StackResourceDetail (Maybe Text)
sStackName = lens _sStackName (\s a -> s {_sStackName = a})

-- | The logical name of the resource specified in the template.
sLogicalResourceId :: Lens' StackResourceDetail Text
sLogicalResourceId = lens _sLogicalResourceId (\s a -> s {_sLogicalResourceId = a})

-- | Type of resource. ((For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
sResourceType :: Lens' StackResourceDetail Text
sResourceType = lens _sResourceType (\s a -> s {_sResourceType = a})

-- | Time the status was updated.
sLastUpdatedTimestamp :: Lens' StackResourceDetail UTCTime
sLastUpdatedTimestamp = lens _sLastUpdatedTimestamp (\s a -> s {_sLastUpdatedTimestamp = a}) . _Time

-- | Current status of the resource.
sResourceStatus :: Lens' StackResourceDetail ResourceStatus
sResourceStatus = lens _sResourceStatus (\s a -> s {_sResourceStatus = a})

instance FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      <$> (x .@? "PhysicalResourceId")
      <*> (x .@? "ResourceStatusReason")
      <*> (x .@? "DriftInformation")
      <*> (x .@? "ModuleInfo")
      <*> (x .@? "Metadata")
      <*> (x .@? "StackId")
      <*> (x .@? "Description")
      <*> (x .@? "StackName")
      <*> (x .@ "LogicalResourceId")
      <*> (x .@ "ResourceType")
      <*> (x .@ "LastUpdatedTimestamp")
      <*> (x .@ "ResourceStatus")

instance Hashable StackResourceDetail

instance NFData StackResourceDetail
