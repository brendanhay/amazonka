{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceSummary where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure that contains summary information about a stack instance.
--
--
--
-- /See:/ 'stackInstanceSummary' smart constructor.
data StackInstanceSummary = StackInstanceSummary'
  { _sisStatus ::
      !(Maybe StackInstanceStatus),
    _sisLastDriftCheckTimestamp :: !(Maybe ISO8601),
    _sisAccount :: !(Maybe Text),
    _sisDriftStatus :: !(Maybe StackDriftStatus),
    _sisOrganizationalUnitId :: !(Maybe Text),
    _sisRegion :: !(Maybe Text),
    _sisStatusReason :: !(Maybe Text),
    _sisStackId :: !(Maybe Text),
    _sisStackInstanceStatus ::
      !(Maybe StackInstanceComprehensiveStatus),
    _sisStackSetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
--
-- * 'sisLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- * 'sisAccount' - [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- * 'sisDriftStatus' - Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'sisOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- * 'sisRegion' - The name of the AWS Region that the stack instance is associated with.
--
-- * 'sisStatusReason' - The explanation for the specific status code assigned to this stack instance.
--
-- * 'sisStackId' - The ID of the stack instance.
--
-- * 'sisStackInstanceStatus' - The detailed status of the stack instance.
--
-- * 'sisStackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
stackInstanceSummary ::
  StackInstanceSummary
stackInstanceSummary =
  StackInstanceSummary'
    { _sisStatus = Nothing,
      _sisLastDriftCheckTimestamp = Nothing,
      _sisAccount = Nothing,
      _sisDriftStatus = Nothing,
      _sisOrganizationalUnitId = Nothing,
      _sisRegion = Nothing,
      _sisStatusReason = Nothing,
      _sisStackId = Nothing,
      _sisStackInstanceStatus = Nothing,
      _sisStackSetId = Nothing
    }

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
sisStatus :: Lens' StackInstanceSummary (Maybe StackInstanceStatus)
sisStatus = lens _sisStatus (\s a -> s {_sisStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
sisLastDriftCheckTimestamp :: Lens' StackInstanceSummary (Maybe UTCTime)
sisLastDriftCheckTimestamp = lens _sisLastDriftCheckTimestamp (\s a -> s {_sisLastDriftCheckTimestamp = a}) . mapping _Time

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
sisAccount :: Lens' StackInstanceSummary (Maybe Text)
sisAccount = lens _sisAccount (\s a -> s {_sisAccount = a})

-- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
sisDriftStatus :: Lens' StackInstanceSummary (Maybe StackDriftStatus)
sisDriftStatus = lens _sisDriftStatus (\s a -> s {_sisDriftStatus = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
sisOrganizationalUnitId :: Lens' StackInstanceSummary (Maybe Text)
sisOrganizationalUnitId = lens _sisOrganizationalUnitId (\s a -> s {_sisOrganizationalUnitId = a})

-- | The name of the AWS Region that the stack instance is associated with.
sisRegion :: Lens' StackInstanceSummary (Maybe Text)
sisRegion = lens _sisRegion (\s a -> s {_sisRegion = a})

-- | The explanation for the specific status code assigned to this stack instance.
sisStatusReason :: Lens' StackInstanceSummary (Maybe Text)
sisStatusReason = lens _sisStatusReason (\s a -> s {_sisStatusReason = a})

-- | The ID of the stack instance.
sisStackId :: Lens' StackInstanceSummary (Maybe Text)
sisStackId = lens _sisStackId (\s a -> s {_sisStackId = a})

-- | The detailed status of the stack instance.
sisStackInstanceStatus :: Lens' StackInstanceSummary (Maybe StackInstanceComprehensiveStatus)
sisStackInstanceStatus = lens _sisStackInstanceStatus (\s a -> s {_sisStackInstanceStatus = a})

-- | The name or unique ID of the stack set that the stack instance is associated with.
sisStackSetId :: Lens' StackInstanceSummary (Maybe Text)
sisStackSetId = lens _sisStackSetId (\s a -> s {_sisStackSetId = a})

instance FromXML StackInstanceSummary where
  parseXML x =
    StackInstanceSummary'
      <$> (x .@? "Status")
      <*> (x .@? "LastDriftCheckTimestamp")
      <*> (x .@? "Account")
      <*> (x .@? "DriftStatus")
      <*> (x .@? "OrganizationalUnitId")
      <*> (x .@? "Region")
      <*> (x .@? "StatusReason")
      <*> (x .@? "StackId")
      <*> (x .@? "StackInstanceStatus")
      <*> (x .@? "StackSetId")

instance Hashable StackInstanceSummary

instance NFData StackInstanceSummary
