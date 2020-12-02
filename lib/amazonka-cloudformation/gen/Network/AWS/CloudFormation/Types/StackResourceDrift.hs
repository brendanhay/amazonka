{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDrift where

import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the drift information for a resource that has been checked for drift. This includes actual and expected property values for resources in which AWS CloudFormation has detected drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
--
--
-- /See:/ 'stackResourceDrift' smart constructor.
data StackResourceDrift = StackResourceDrift'
  { _srdActualProperties ::
      !(Maybe Text),
    _srdPhysicalResourceId :: !(Maybe Text),
    _srdPhysicalResourceIdContext ::
      !(Maybe [PhysicalResourceIdContextKeyValuePair]),
    _srdPropertyDifferences ::
      !(Maybe [PropertyDifference]),
    _srdModuleInfo :: !(Maybe ModuleInfo),
    _srdExpectedProperties :: !(Maybe Text),
    _srdStackId :: !Text,
    _srdLogicalResourceId :: !Text,
    _srdResourceType :: !Text,
    _srdStackResourceDriftStatus ::
      !StackResourceDriftStatus,
    _srdTimestamp :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResourceDrift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdActualProperties' - A JSON structure containing the actual property values of the stack resource. For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- * 'srdPhysicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- * 'srdPhysicalResourceIdContext' - Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
--
-- * 'srdPropertyDifferences' - A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
--
-- * 'srdModuleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- * 'srdExpectedProperties' - A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.  For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
--
-- * 'srdStackId' - The ID of the stack.
--
-- * 'srdLogicalResourceId' - The logical name of the resource specified in the template.
--
-- * 'srdResourceType' - The type of the resource.
--
-- * 'srdStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
-- * 'srdTimestamp' - Time at which AWS CloudFormation performed drift detection on the stack resource.
stackResourceDrift ::
  -- | 'srdStackId'
  Text ->
  -- | 'srdLogicalResourceId'
  Text ->
  -- | 'srdResourceType'
  Text ->
  -- | 'srdStackResourceDriftStatus'
  StackResourceDriftStatus ->
  -- | 'srdTimestamp'
  UTCTime ->
  StackResourceDrift
stackResourceDrift
  pStackId_
  pLogicalResourceId_
  pResourceType_
  pStackResourceDriftStatus_
  pTimestamp_ =
    StackResourceDrift'
      { _srdActualProperties = Nothing,
        _srdPhysicalResourceId = Nothing,
        _srdPhysicalResourceIdContext = Nothing,
        _srdPropertyDifferences = Nothing,
        _srdModuleInfo = Nothing,
        _srdExpectedProperties = Nothing,
        _srdStackId = pStackId_,
        _srdLogicalResourceId = pLogicalResourceId_,
        _srdResourceType = pResourceType_,
        _srdStackResourceDriftStatus = pStackResourceDriftStatus_,
        _srdTimestamp = _Time # pTimestamp_
      }

-- | A JSON structure containing the actual property values of the stack resource. For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
srdActualProperties :: Lens' StackResourceDrift (Maybe Text)
srdActualProperties = lens _srdActualProperties (\s a -> s {_srdActualProperties = a})

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDrift (Maybe Text)
srdPhysicalResourceId = lens _srdPhysicalResourceId (\s a -> s {_srdPhysicalResourceId = a})

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a unique resource that contains the targeted resource.
srdPhysicalResourceIdContext :: Lens' StackResourceDrift [PhysicalResourceIdContextKeyValuePair]
srdPhysicalResourceIdContext = lens _srdPhysicalResourceIdContext (\s a -> s {_srdPhysicalResourceIdContext = a}) . _Default . _Coerce

-- | A collection of the resource properties whose actual values differ from their expected values. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ .
srdPropertyDifferences :: Lens' StackResourceDrift [PropertyDifference]
srdPropertyDifferences = lens _srdPropertyDifferences (\s a -> s {_srdPropertyDifferences = a}) . _Default . _Coerce

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
srdModuleInfo :: Lens' StackResourceDrift (Maybe ModuleInfo)
srdModuleInfo = lens _srdModuleInfo (\s a -> s {_srdModuleInfo = a})

-- | A JSON structure containing the expected property values of the stack resource, as defined in the stack template and any values specified as template parameters.  For resources whose @StackResourceDriftStatus@ is @DELETED@ , this structure will not be present.
srdExpectedProperties :: Lens' StackResourceDrift (Maybe Text)
srdExpectedProperties = lens _srdExpectedProperties (\s a -> s {_srdExpectedProperties = a})

-- | The ID of the stack.
srdStackId :: Lens' StackResourceDrift Text
srdStackId = lens _srdStackId (\s a -> s {_srdStackId = a})

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDrift Text
srdLogicalResourceId = lens _srdLogicalResourceId (\s a -> s {_srdLogicalResourceId = a})

-- | The type of the resource.
srdResourceType :: Lens' StackResourceDrift Text
srdResourceType = lens _srdResourceType (\s a -> s {_srdResourceType = a})

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected template configuration because the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected values (as defined in the stack template and any values specified as template parameters).     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
srdStackResourceDriftStatus :: Lens' StackResourceDrift StackResourceDriftStatus
srdStackResourceDriftStatus = lens _srdStackResourceDriftStatus (\s a -> s {_srdStackResourceDriftStatus = a})

-- | Time at which AWS CloudFormation performed drift detection on the stack resource.
srdTimestamp :: Lens' StackResourceDrift UTCTime
srdTimestamp = lens _srdTimestamp (\s a -> s {_srdTimestamp = a}) . _Time

instance FromXML StackResourceDrift where
  parseXML x =
    StackResourceDrift'
      <$> (x .@? "ActualProperties")
      <*> (x .@? "PhysicalResourceId")
      <*> ( x .@? "PhysicalResourceIdContext" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "PropertyDifferences" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "ModuleInfo")
      <*> (x .@? "ExpectedProperties")
      <*> (x .@ "StackId")
      <*> (x .@ "LogicalResourceId")
      <*> (x .@ "ResourceType")
      <*> (x .@ "StackResourceDriftStatus")
      <*> (x .@ "Timestamp")

instance Hashable StackResourceDrift

instance NFData StackResourceDrift
