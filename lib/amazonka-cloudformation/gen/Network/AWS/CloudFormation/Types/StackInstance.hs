{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstance where

import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An AWS CloudFormation stack, in a specific account and Region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given Region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
--
--
-- /See:/ 'stackInstance' smart constructor.
data StackInstance = StackInstance'
  { _siStatus ::
      !(Maybe StackInstanceStatus),
    _siLastDriftCheckTimestamp :: !(Maybe ISO8601),
    _siAccount :: !(Maybe Text),
    _siDriftStatus :: !(Maybe StackDriftStatus),
    _siOrganizationalUnitId :: !(Maybe Text),
    _siRegion :: !(Maybe Text),
    _siStatusReason :: !(Maybe Text),
    _siStackId :: !(Maybe Text),
    _siStackInstanceStatus ::
      !(Maybe StackInstanceComprehensiveStatus),
    _siParameterOverrides :: !(Maybe [Parameter]),
    _siStackSetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
--
-- * 'siLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- * 'siAccount' - [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- * 'siDriftStatus' - Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'siOrganizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- * 'siRegion' - The name of the AWS Region that the stack instance is associated with.
--
-- * 'siStatusReason' - The explanation for the specific status code that is assigned to this stack instance.
--
-- * 'siStackId' - The ID of the stack instance.
--
-- * 'siStackInstanceStatus' - The detailed status of the stack instance.
--
-- * 'siParameterOverrides' - A list of parameters from the stack set template whose values have been overridden in this stack instance.
--
-- * 'siStackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
stackInstance ::
  StackInstance
stackInstance =
  StackInstance'
    { _siStatus = Nothing,
      _siLastDriftCheckTimestamp = Nothing,
      _siAccount = Nothing,
      _siDriftStatus = Nothing,
      _siOrganizationalUnitId = Nothing,
      _siRegion = Nothing,
      _siStatusReason = Nothing,
      _siStackId = Nothing,
      _siStackInstanceStatus = Nothing,
      _siParameterOverrides = Nothing,
      _siStackSetId = Nothing
    }

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
siStatus :: Lens' StackInstance (Maybe StackInstanceStatus)
siStatus = lens _siStatus (\s a -> s {_siStatus = a})

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
siLastDriftCheckTimestamp :: Lens' StackInstance (Maybe UTCTime)
siLastDriftCheckTimestamp = lens _siLastDriftCheckTimestamp (\s a -> s {_siLastDriftCheckTimestamp = a}) . mapping _Time

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
siAccount :: Lens' StackInstance (Maybe Text)
siAccount = lens _siAccount (\s a -> s {_siAccount = a})

-- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.      * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.     * @UNKNOWN@ : This value is reserved for future use.
siDriftStatus :: Lens' StackInstance (Maybe StackDriftStatus)
siDriftStatus = lens _siDriftStatus (\s a -> s {_siDriftStatus = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
siOrganizationalUnitId :: Lens' StackInstance (Maybe Text)
siOrganizationalUnitId = lens _siOrganizationalUnitId (\s a -> s {_siOrganizationalUnitId = a})

-- | The name of the AWS Region that the stack instance is associated with.
siRegion :: Lens' StackInstance (Maybe Text)
siRegion = lens _siRegion (\s a -> s {_siRegion = a})

-- | The explanation for the specific status code that is assigned to this stack instance.
siStatusReason :: Lens' StackInstance (Maybe Text)
siStatusReason = lens _siStatusReason (\s a -> s {_siStatusReason = a})

-- | The ID of the stack instance.
siStackId :: Lens' StackInstance (Maybe Text)
siStackId = lens _siStackId (\s a -> s {_siStackId = a})

-- | The detailed status of the stack instance.
siStackInstanceStatus :: Lens' StackInstance (Maybe StackInstanceComprehensiveStatus)
siStackInstanceStatus = lens _siStackInstanceStatus (\s a -> s {_siStackInstanceStatus = a})

-- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
siParameterOverrides :: Lens' StackInstance [Parameter]
siParameterOverrides = lens _siParameterOverrides (\s a -> s {_siParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set that the stack instance is associated with.
siStackSetId :: Lens' StackInstance (Maybe Text)
siStackSetId = lens _siStackSetId (\s a -> s {_siStackSetId = a})

instance FromXML StackInstance where
  parseXML x =
    StackInstance'
      <$> (x .@? "Status")
      <*> (x .@? "LastDriftCheckTimestamp")
      <*> (x .@? "Account")
      <*> (x .@? "DriftStatus")
      <*> (x .@? "OrganizationalUnitId")
      <*> (x .@? "Region")
      <*> (x .@? "StatusReason")
      <*> (x .@? "StackId")
      <*> (x .@? "StackInstanceStatus")
      <*> ( x .@? "ParameterOverrides" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "StackSetId")

instance Hashable StackInstance

instance NFData StackInstance
