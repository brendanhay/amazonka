{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stack drift detection operation. A stack drift detection operation detects whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted. For more information on stack and resource drift, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
-- Use 'DetectStackDrift' to initiate a stack drift detection operation. @DetectStackDrift@ returns a @StackDriftDetectionId@ you can use to monitor the progress of the operation using @DescribeStackDriftDetectionStatus@ . Once the drift detection operation has completed, use 'DescribeStackResourceDrifts' to return drift information about the stack and its resources.
module Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
  ( -- * Creating a Request
    describeStackDriftDetectionStatus,
    DescribeStackDriftDetectionStatus,

    -- * Request Lenses
    dsddsStackDriftDetectionId,

    -- * Destructuring the Response
    describeStackDriftDetectionStatusResponse,
    DescribeStackDriftDetectionStatusResponse,

    -- * Response Lenses
    dsddsrsStackDriftStatus,
    dsddsrsDriftedStackResourceCount,
    dsddsrsDetectionStatusReason,
    dsddsrsResponseStatus,
    dsddsrsStackId,
    dsddsrsStackDriftDetectionId,
    dsddsrsDetectionStatus,
    dsddsrsTimestamp,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackDriftDetectionStatus' smart constructor.
newtype DescribeStackDriftDetectionStatus = DescribeStackDriftDetectionStatus'
  { _dsddsStackDriftDetectionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStackDriftDetectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsddsStackDriftDetectionId' - The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
describeStackDriftDetectionStatus ::
  -- | 'dsddsStackDriftDetectionId'
  Text ->
  DescribeStackDriftDetectionStatus
describeStackDriftDetectionStatus pStackDriftDetectionId_ =
  DescribeStackDriftDetectionStatus'
    { _dsddsStackDriftDetectionId =
        pStackDriftDetectionId_
    }

-- | The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
dsddsStackDriftDetectionId :: Lens' DescribeStackDriftDetectionStatus Text
dsddsStackDriftDetectionId = lens _dsddsStackDriftDetectionId (\s a -> s {_dsddsStackDriftDetectionId = a})

instance AWSRequest DescribeStackDriftDetectionStatus where
  type
    Rs DescribeStackDriftDetectionStatus =
      DescribeStackDriftDetectionStatusResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DescribeStackDriftDetectionStatusResult"
      ( \s h x ->
          DescribeStackDriftDetectionStatusResponse'
            <$> (x .@? "StackDriftStatus")
            <*> (x .@? "DriftedStackResourceCount")
            <*> (x .@? "DetectionStatusReason")
            <*> (pure (fromEnum s))
            <*> (x .@ "StackId")
            <*> (x .@ "StackDriftDetectionId")
            <*> (x .@ "DetectionStatus")
            <*> (x .@ "Timestamp")
      )

instance Hashable DescribeStackDriftDetectionStatus

instance NFData DescribeStackDriftDetectionStatus

instance ToHeaders DescribeStackDriftDetectionStatus where
  toHeaders = const mempty

instance ToPath DescribeStackDriftDetectionStatus where
  toPath = const "/"

instance ToQuery DescribeStackDriftDetectionStatus where
  toQuery DescribeStackDriftDetectionStatus' {..} =
    mconcat
      [ "Action" =: ("DescribeStackDriftDetectionStatus" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "StackDriftDetectionId" =: _dsddsStackDriftDetectionId
      ]

-- | /See:/ 'describeStackDriftDetectionStatusResponse' smart constructor.
data DescribeStackDriftDetectionStatusResponse = DescribeStackDriftDetectionStatusResponse'
  { _dsddsrsStackDriftStatus ::
      !( Maybe
           StackDriftStatus
       ),
    _dsddsrsDriftedStackResourceCount ::
      !( Maybe
           Int
       ),
    _dsddsrsDetectionStatusReason ::
      !( Maybe
           Text
       ),
    _dsddsrsResponseStatus ::
      !Int,
    _dsddsrsStackId ::
      !Text,
    _dsddsrsStackDriftDetectionId ::
      !Text,
    _dsddsrsDetectionStatus ::
      !StackDriftDetectionStatus,
    _dsddsrsTimestamp ::
      !ISO8601
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeStackDriftDetectionStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsddsrsStackDriftStatus' - Status of the stack's actual configuration compared to its expected configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
--
-- * 'dsddsrsDriftedStackResourceCount' - Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
--
-- * 'dsddsrsDetectionStatusReason' - The reason the stack drift detection operation has its current status.
--
-- * 'dsddsrsResponseStatus' - -- | The response status code.
--
-- * 'dsddsrsStackId' - The ID of the stack.
--
-- * 'dsddsrsStackDriftDetectionId' - The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
--
-- * 'dsddsrsDetectionStatus' - The status of the stack drift detection operation.     * @DETECTION_COMPLETE@ : The stack drift detection operation has successfully completed for all resources in the stack that support drift detection. (Resources that do not currently support stack detection remain unchecked.) If you specified logical resource IDs for AWS CloudFormation to use as a filter for the stack drift detection operation, only the resources with those logical IDs are checked for drift.     * @DETECTION_FAILED@ : The stack drift detection operation has failed for at least one resource in the stack. Results will be available for resources on which AWS CloudFormation successfully completed drift detection.     * @DETECTION_IN_PROGRESS@ : The stack drift detection operation is currently in progress.
--
-- * 'dsddsrsTimestamp' - Time at which the stack drift detection operation was initiated.
describeStackDriftDetectionStatusResponse ::
  -- | 'dsddsrsResponseStatus'
  Int ->
  -- | 'dsddsrsStackId'
  Text ->
  -- | 'dsddsrsStackDriftDetectionId'
  Text ->
  -- | 'dsddsrsDetectionStatus'
  StackDriftDetectionStatus ->
  -- | 'dsddsrsTimestamp'
  UTCTime ->
  DescribeStackDriftDetectionStatusResponse
describeStackDriftDetectionStatusResponse
  pResponseStatus_
  pStackId_
  pStackDriftDetectionId_
  pDetectionStatus_
  pTimestamp_ =
    DescribeStackDriftDetectionStatusResponse'
      { _dsddsrsStackDriftStatus =
          Nothing,
        _dsddsrsDriftedStackResourceCount = Nothing,
        _dsddsrsDetectionStatusReason = Nothing,
        _dsddsrsResponseStatus = pResponseStatus_,
        _dsddsrsStackId = pStackId_,
        _dsddsrsStackDriftDetectionId =
          pStackDriftDetectionId_,
        _dsddsrsDetectionStatus = pDetectionStatus_,
        _dsddsrsTimestamp = _Time # pTimestamp_
      }

-- | Status of the stack's actual configuration compared to its expected configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
dsddsrsStackDriftStatus :: Lens' DescribeStackDriftDetectionStatusResponse (Maybe StackDriftStatus)
dsddsrsStackDriftStatus = lens _dsddsrsStackDriftStatus (\s a -> s {_dsddsrsStackDriftStatus = a})

-- | Total number of stack resources that have drifted. This is NULL until the drift detection operation reaches a status of @DETECTION_COMPLETE@ . This value will be 0 for stacks whose drift status is @IN_SYNC@ .
dsddsrsDriftedStackResourceCount :: Lens' DescribeStackDriftDetectionStatusResponse (Maybe Int)
dsddsrsDriftedStackResourceCount = lens _dsddsrsDriftedStackResourceCount (\s a -> s {_dsddsrsDriftedStackResourceCount = a})

-- | The reason the stack drift detection operation has its current status.
dsddsrsDetectionStatusReason :: Lens' DescribeStackDriftDetectionStatusResponse (Maybe Text)
dsddsrsDetectionStatusReason = lens _dsddsrsDetectionStatusReason (\s a -> s {_dsddsrsDetectionStatusReason = a})

-- | -- | The response status code.
dsddsrsResponseStatus :: Lens' DescribeStackDriftDetectionStatusResponse Int
dsddsrsResponseStatus = lens _dsddsrsResponseStatus (\s a -> s {_dsddsrsResponseStatus = a})

-- | The ID of the stack.
dsddsrsStackId :: Lens' DescribeStackDriftDetectionStatusResponse Text
dsddsrsStackId = lens _dsddsrsStackId (\s a -> s {_dsddsrsStackId = a})

-- | The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of reports AWS CloudFormation retains for any given stack, and for how long, may vary.
dsddsrsStackDriftDetectionId :: Lens' DescribeStackDriftDetectionStatusResponse Text
dsddsrsStackDriftDetectionId = lens _dsddsrsStackDriftDetectionId (\s a -> s {_dsddsrsStackDriftDetectionId = a})

-- | The status of the stack drift detection operation.     * @DETECTION_COMPLETE@ : The stack drift detection operation has successfully completed for all resources in the stack that support drift detection. (Resources that do not currently support stack detection remain unchecked.) If you specified logical resource IDs for AWS CloudFormation to use as a filter for the stack drift detection operation, only the resources with those logical IDs are checked for drift.     * @DETECTION_FAILED@ : The stack drift detection operation has failed for at least one resource in the stack. Results will be available for resources on which AWS CloudFormation successfully completed drift detection.     * @DETECTION_IN_PROGRESS@ : The stack drift detection operation is currently in progress.
dsddsrsDetectionStatus :: Lens' DescribeStackDriftDetectionStatusResponse StackDriftDetectionStatus
dsddsrsDetectionStatus = lens _dsddsrsDetectionStatus (\s a -> s {_dsddsrsDetectionStatus = a})

-- | Time at which the stack drift detection operation was initiated.
dsddsrsTimestamp :: Lens' DescribeStackDriftDetectionStatusResponse UTCTime
dsddsrsTimestamp = lens _dsddsrsTimestamp (\s a -> s {_dsddsrsTimestamp = a}) . _Time

instance NFData DescribeStackDriftDetectionStatusResponse
