{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetSummary where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structures that contain summary information about the specified stack set.
--
--
--
-- /See:/ 'stackSetSummary' smart constructor.
data StackSetSummary = StackSetSummary'
  { _sssStatus ::
      !(Maybe StackSetStatus),
    _sssLastDriftCheckTimestamp :: !(Maybe ISO8601),
    _sssAutoDeployment :: !(Maybe AutoDeployment),
    _sssDriftStatus :: !(Maybe StackDriftStatus),
    _sssPermissionModel :: !(Maybe PermissionModels),
    _sssStackSetName :: !(Maybe Text),
    _sssDescription :: !(Maybe Text),
    _sssStackSetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssStatus' - The status of the stack set.
--
-- * 'sssLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- * 'sssAutoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
--
-- * 'sssDriftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'sssPermissionModel' - Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
-- * 'sssStackSetName' - The name of the stack set.
--
-- * 'sssDescription' - A description of the stack set that you specify when the stack set is created or updated.
--
-- * 'sssStackSetId' - The ID of the stack set.
stackSetSummary ::
  StackSetSummary
stackSetSummary =
  StackSetSummary'
    { _sssStatus = Nothing,
      _sssLastDriftCheckTimestamp = Nothing,
      _sssAutoDeployment = Nothing,
      _sssDriftStatus = Nothing,
      _sssPermissionModel = Nothing,
      _sssStackSetName = Nothing,
      _sssDescription = Nothing,
      _sssStackSetId = Nothing
    }

-- | The status of the stack set.
sssStatus :: Lens' StackSetSummary (Maybe StackSetStatus)
sssStatus = lens _sssStatus (\s a -> s {_sssStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
sssLastDriftCheckTimestamp :: Lens' StackSetSummary (Maybe UTCTime)
sssLastDriftCheckTimestamp = lens _sssLastDriftCheckTimestamp (\s a -> s {_sssLastDriftCheckTimestamp = a}) . mapping _Time

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
sssAutoDeployment :: Lens' StackSetSummary (Maybe AutoDeployment)
sssAutoDeployment = lens _sssAutoDeployment (\s a -> s {_sssAutoDeployment = a})

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.     * @UNKNOWN@ : This value is reserved for future use.
sssDriftStatus :: Lens' StackSetSummary (Maybe StackDriftStatus)
sssDriftStatus = lens _sssDriftStatus (\s a -> s {_sssDriftStatus = a})

-- | Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
sssPermissionModel :: Lens' StackSetSummary (Maybe PermissionModels)
sssPermissionModel = lens _sssPermissionModel (\s a -> s {_sssPermissionModel = a})

-- | The name of the stack set.
sssStackSetName :: Lens' StackSetSummary (Maybe Text)
sssStackSetName = lens _sssStackSetName (\s a -> s {_sssStackSetName = a})

-- | A description of the stack set that you specify when the stack set is created or updated.
sssDescription :: Lens' StackSetSummary (Maybe Text)
sssDescription = lens _sssDescription (\s a -> s {_sssDescription = a})

-- | The ID of the stack set.
sssStackSetId :: Lens' StackSetSummary (Maybe Text)
sssStackSetId = lens _sssStackSetId (\s a -> s {_sssStackSetId = a})

instance FromXML StackSetSummary where
  parseXML x =
    StackSetSummary'
      <$> (x .@? "Status")
      <*> (x .@? "LastDriftCheckTimestamp")
      <*> (x .@? "AutoDeployment")
      <*> (x .@? "DriftStatus")
      <*> (x .@? "PermissionModel")
      <*> (x .@? "StackSetName")
      <*> (x .@? "Description")
      <*> (x .@? "StackSetId")

instance Hashable StackSetSummary

instance NFData StackSetSummary
