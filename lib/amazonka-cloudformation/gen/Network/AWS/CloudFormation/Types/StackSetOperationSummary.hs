{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationSummary where

import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structures that contain summary information about the specified operation.
--
--
--
-- /See:/ 'stackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { _ssosStatus ::
      !(Maybe StackSetOperationStatus),
    _ssosAction ::
      !(Maybe StackSetOperationAction),
    _ssosEndTimestamp :: !(Maybe ISO8601),
    _ssosCreationTimestamp ::
      !(Maybe ISO8601),
    _ssosOperationId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetOperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssosStatus' - The overall status of the operation.     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
-- * 'ssosAction' - The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
--
-- * 'ssosEndTimestamp' - The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- * 'ssosCreationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- * 'ssosOperationId' - The unique ID of the stack set operation.
stackSetOperationSummary ::
  StackSetOperationSummary
stackSetOperationSummary =
  StackSetOperationSummary'
    { _ssosStatus = Nothing,
      _ssosAction = Nothing,
      _ssosEndTimestamp = Nothing,
      _ssosCreationTimestamp = Nothing,
      _ssosOperationId = Nothing
    }

-- | The overall status of the operation.     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.     * @RUNNING@ : The operation is currently being performed.     * @STOPPED@ : The user has cancelled the operation.     * @STOPPING@ : The operation is in the process of stopping, at user request.      * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
ssosStatus :: Lens' StackSetOperationSummary (Maybe StackSetOperationStatus)
ssosStatus = lens _ssosStatus (\s a -> s {_ssosStatus = a})

-- | The type of operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack instances that are associated with the specified stack set. Update operations affect both the stack set itself as well as /all/ associated stack set instances.
ssosAction :: Lens' StackSetOperationSummary (Maybe StackSetOperationAction)
ssosAction = lens _ssosAction (\s a -> s {_ssosAction = a})

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
ssosEndTimestamp :: Lens' StackSetOperationSummary (Maybe UTCTime)
ssosEndTimestamp = lens _ssosEndTimestamp (\s a -> s {_ssosEndTimestamp = a}) . mapping _Time

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
ssosCreationTimestamp :: Lens' StackSetOperationSummary (Maybe UTCTime)
ssosCreationTimestamp = lens _ssosCreationTimestamp (\s a -> s {_ssosCreationTimestamp = a}) . mapping _Time

-- | The unique ID of the stack set operation.
ssosOperationId :: Lens' StackSetOperationSummary (Maybe Text)
ssosOperationId = lens _ssosOperationId (\s a -> s {_ssosOperationId = a})

instance FromXML StackSetOperationSummary where
  parseXML x =
    StackSetOperationSummary'
      <$> (x .@? "Status")
      <*> (x .@? "Action")
      <*> (x .@? "EndTimestamp")
      <*> (x .@? "CreationTimestamp")
      <*> (x .@? "OperationId")

instance Hashable StackSetOperationSummary

instance NFData StackSetOperationSummary
