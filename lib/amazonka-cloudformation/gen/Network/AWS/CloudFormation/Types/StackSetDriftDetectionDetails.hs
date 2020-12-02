{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails where

import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about the drift status of the stack set.
--
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations in-progress is not included.
--
-- For stack set operations, includes information about drift operations currently being performed on the stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the /AWS CloudFormation User Guide/ .
--
--
-- /See:/ 'stackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { _ssdddLastDriftCheckTimestamp ::
      !(Maybe ISO8601),
    _ssdddTotalStackInstancesCount ::
      !(Maybe Nat),
    _ssdddInProgressStackInstancesCount ::
      !(Maybe Nat),
    _ssdddDriftedStackInstancesCount ::
      !(Maybe Nat),
    _ssdddDriftDetectionStatus ::
      !( Maybe
           StackSetDriftDetectionStatus
       ),
    _ssdddDriftStatus ::
      !(Maybe StackSetDriftStatus),
    _ssdddFailedStackInstancesCount ::
      !(Maybe Nat),
    _ssdddInSyncStackInstancesCount ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetDriftDetectionDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdddLastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- * 'ssdddTotalStackInstancesCount' - The total number of stack instances belonging to this stack set.  The total number of stack instances is equal to the total of:     * Stack instances that match the stack set configuration.      * Stack instances that have drifted from the stack set configuration.      * Stack instances where the drift detection operation has failed.     * Stack instances currently being checked for drift.
--
-- * 'ssdddInProgressStackInstancesCount' - The number of stack instances that are currently being checked for drift.
--
-- * 'ssdddDriftedStackInstancesCount' - The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
--
-- * 'ssdddDriftDetectionStatus' - The status of the stack set drift detection operation.     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.      * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.     * @IN_PROGRESS@ : The drift detection operation is currently being performed.     * @STOPPED@ : The user has cancelled the drift detection operation.
--
-- * 'ssdddDriftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
--
-- * 'ssdddFailedStackInstancesCount' - The number of stack instances for which the drift detection operation failed.
--
-- * 'ssdddInSyncStackInstancesCount' - The number of stack instances which match the expected template and parameter configuration of the stack set.
stackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
stackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { _ssdddLastDriftCheckTimestamp =
        Nothing,
      _ssdddTotalStackInstancesCount = Nothing,
      _ssdddInProgressStackInstancesCount = Nothing,
      _ssdddDriftedStackInstancesCount = Nothing,
      _ssdddDriftDetectionStatus = Nothing,
      _ssdddDriftStatus = Nothing,
      _ssdddFailedStackInstancesCount = Nothing,
      _ssdddInSyncStackInstancesCount = Nothing
    }

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
ssdddLastDriftCheckTimestamp :: Lens' StackSetDriftDetectionDetails (Maybe UTCTime)
ssdddLastDriftCheckTimestamp = lens _ssdddLastDriftCheckTimestamp (\s a -> s {_ssdddLastDriftCheckTimestamp = a}) . mapping _Time

-- | The total number of stack instances belonging to this stack set.  The total number of stack instances is equal to the total of:     * Stack instances that match the stack set configuration.      * Stack instances that have drifted from the stack set configuration.      * Stack instances where the drift detection operation has failed.     * Stack instances currently being checked for drift.
ssdddTotalStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddTotalStackInstancesCount = lens _ssdddTotalStackInstancesCount (\s a -> s {_ssdddTotalStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances that are currently being checked for drift.
ssdddInProgressStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddInProgressStackInstancesCount = lens _ssdddInProgressStackInstancesCount (\s a -> s {_ssdddInProgressStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
ssdddDriftedStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddDriftedStackInstancesCount = lens _ssdddDriftedStackInstancesCount (\s a -> s {_ssdddDriftedStackInstancesCount = a}) . mapping _Nat

-- | The status of the stack set drift detection operation.     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.      * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.     * @IN_PROGRESS@ : The drift detection operation is currently being performed.     * @STOPPED@ : The user has cancelled the drift detection operation.
ssdddDriftDetectionStatus :: Lens' StackSetDriftDetectionDetails (Maybe StackSetDriftDetectionStatus)
ssdddDriftDetectionStatus = lens _ssdddDriftDetectionStatus (\s a -> s {_ssdddDriftDetectionStatus = a})

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
ssdddDriftStatus :: Lens' StackSetDriftDetectionDetails (Maybe StackSetDriftStatus)
ssdddDriftStatus = lens _ssdddDriftStatus (\s a -> s {_ssdddDriftStatus = a})

-- | The number of stack instances for which the drift detection operation failed.
ssdddFailedStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddFailedStackInstancesCount = lens _ssdddFailedStackInstancesCount (\s a -> s {_ssdddFailedStackInstancesCount = a}) . mapping _Nat

-- | The number of stack instances which match the expected template and parameter configuration of the stack set.
ssdddInSyncStackInstancesCount :: Lens' StackSetDriftDetectionDetails (Maybe Natural)
ssdddInSyncStackInstancesCount = lens _ssdddInSyncStackInstancesCount (\s a -> s {_ssdddInSyncStackInstancesCount = a}) . mapping _Nat

instance FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      <$> (x .@? "LastDriftCheckTimestamp")
      <*> (x .@? "TotalStackInstancesCount")
      <*> (x .@? "InProgressStackInstancesCount")
      <*> (x .@? "DriftedStackInstancesCount")
      <*> (x .@? "DriftDetectionStatus")
      <*> (x .@? "DriftStatus")
      <*> (x .@? "FailedStackInstancesCount")
      <*> (x .@? "InSyncStackInstancesCount")

instance Hashable StackSetDriftDetectionDetails

instance NFData StackSetDriftDetectionDetails
