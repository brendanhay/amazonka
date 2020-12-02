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
-- Module      : Network.AWS.CloudFormation.DetectStackSetDrift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detect drift on a stack set. When CloudFormation performs drift detection on a stack set, it performs drift detection on the stack associated with each stack instance in the stack set. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html How CloudFormation Performs Drift Detection on a Stack Set> .
--
--
-- @DetectStackSetDrift@ returns the @OperationId@ of the stack set drift detection operation. Use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation. The drift detection operation may take some time, depending on the number of stack instances included in the stack set, as well as the number of resources included in each stack.
--
-- Once the operation has completed, use the following actions to return drift information:
--
--     * Use @'DescribeStackSet' @ to return detailed informaiton about the stack set, including detailed information about the last /completed/ drift operation performed on the stack set. (Information about drift operations that are in progress is not included.)
--
--     * Use @'ListStackInstances' @ to return a list of stack instances belonging to the stack set, including the drift status and last drift time checked of each instance.
--
--     * Use @'DescribeStackInstance' @ to return detailed information about a specific stack instance, including its drift status and last drift time checked.
--
--
--
-- For more information on performing a drift detection operation on a stack set, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> .
--
-- You can only run a single drift detection operation on a given stack set at one time.
--
-- To stop a drift detection stack set operation, use @'StopStackSetOperation' @ .
module Network.AWS.CloudFormation.DetectStackSetDrift
  ( -- * Creating a Request
    detectStackSetDrift,
    DetectStackSetDrift,

    -- * Request Lenses
    dssdOperationPreferences,
    dssdOperationId,
    dssdStackSetName,

    -- * Destructuring the Response
    detectStackSetDriftResponse,
    DetectStackSetDriftResponse,

    -- * Response Lenses
    dssdrsOperationId,
    dssdrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectStackSetDrift' smart constructor.
data DetectStackSetDrift = DetectStackSetDrift'
  { _dssdOperationPreferences ::
      !(Maybe StackSetOperationPreferences),
    _dssdOperationId :: !(Maybe Text),
    _dssdStackSetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectStackSetDrift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssdOperationPreferences' - Undocumented member.
--
-- * 'dssdOperationId' - /The ID of the stack set operation./
--
-- * 'dssdStackSetName' - The name of the stack set on which to perform the drift detection operation.
detectStackSetDrift ::
  -- | 'dssdStackSetName'
  Text ->
  DetectStackSetDrift
detectStackSetDrift pStackSetName_ =
  DetectStackSetDrift'
    { _dssdOperationPreferences = Nothing,
      _dssdOperationId = Nothing,
      _dssdStackSetName = pStackSetName_
    }

-- | Undocumented member.
dssdOperationPreferences :: Lens' DetectStackSetDrift (Maybe StackSetOperationPreferences)
dssdOperationPreferences = lens _dssdOperationPreferences (\s a -> s {_dssdOperationPreferences = a})

-- | /The ID of the stack set operation./
dssdOperationId :: Lens' DetectStackSetDrift (Maybe Text)
dssdOperationId = lens _dssdOperationId (\s a -> s {_dssdOperationId = a})

-- | The name of the stack set on which to perform the drift detection operation.
dssdStackSetName :: Lens' DetectStackSetDrift Text
dssdStackSetName = lens _dssdStackSetName (\s a -> s {_dssdStackSetName = a})

instance AWSRequest DetectStackSetDrift where
  type Rs DetectStackSetDrift = DetectStackSetDriftResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DetectStackSetDriftResult"
      ( \s h x ->
          DetectStackSetDriftResponse'
            <$> (x .@? "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable DetectStackSetDrift

instance NFData DetectStackSetDrift

instance ToHeaders DetectStackSetDrift where
  toHeaders = const mempty

instance ToPath DetectStackSetDrift where
  toPath = const "/"

instance ToQuery DetectStackSetDrift where
  toQuery DetectStackSetDrift' {..} =
    mconcat
      [ "Action" =: ("DetectStackSetDrift" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "OperationPreferences" =: _dssdOperationPreferences,
        "OperationId" =: _dssdOperationId,
        "StackSetName" =: _dssdStackSetName
      ]

-- | /See:/ 'detectStackSetDriftResponse' smart constructor.
data DetectStackSetDriftResponse = DetectStackSetDriftResponse'
  { _dssdrsOperationId ::
      !(Maybe Text),
    _dssdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectStackSetDriftResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssdrsOperationId' - The ID of the drift detection stack set operation.  you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
--
-- * 'dssdrsResponseStatus' - -- | The response status code.
detectStackSetDriftResponse ::
  -- | 'dssdrsResponseStatus'
  Int ->
  DetectStackSetDriftResponse
detectStackSetDriftResponse pResponseStatus_ =
  DetectStackSetDriftResponse'
    { _dssdrsOperationId = Nothing,
      _dssdrsResponseStatus = pResponseStatus_
    }

-- | The ID of the drift detection stack set operation.  you can use this operation id with @'DescribeStackSetOperation' @ to monitor the progress of the drift detection operation.
dssdrsOperationId :: Lens' DetectStackSetDriftResponse (Maybe Text)
dssdrsOperationId = lens _dssdrsOperationId (\s a -> s {_dssdrsOperationId = a})

-- | -- | The response status code.
dssdrsResponseStatus :: Lens' DetectStackSetDriftResponse Int
dssdrsResponseStatus = lens _dssdrsResponseStatus (\s a -> s {_dssdrsResponseStatus = a})

instance NFData DetectStackSetDriftResponse
