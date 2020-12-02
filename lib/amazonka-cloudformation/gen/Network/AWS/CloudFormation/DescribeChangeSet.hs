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
-- Module      : Network.AWS.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets> in the AWS CloudFormation User Guide.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeChangeSet
  ( -- * Creating a Request
    describeChangeSet,
    DescribeChangeSet,

    -- * Request Lenses
    desNextToken,
    desStackName,
    desChangeSetName,

    -- * Destructuring the Response
    describeChangeSetResponse,
    DescribeChangeSetResponse,

    -- * Response Lenses
    dcscrsCreationTime,
    dcscrsParentChangeSetId,
    dcscrsChanges,
    dcscrsNotificationARNs,
    dcscrsChangeSetName,
    dcscrsExecutionStatus,
    dcscrsChangeSetId,
    dcscrsIncludeNestedStacks,
    dcscrsNextToken,
    dcscrsRootChangeSetId,
    dcscrsParameters,
    dcscrsStatusReason,
    dcscrsStackId,
    dcscrsDescription,
    dcscrsCapabilities,
    dcscrsRollbackConfiguration,
    dcscrsTags,
    dcscrsStackName,
    dcscrsResponseStatus,
    dcscrsStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { _desNextToken ::
      !(Maybe Text),
    _desStackName :: !(Maybe Text),
    _desChangeSetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken' - A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- * 'desStackName' - If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- * 'desChangeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to describe.
describeChangeSet ::
  -- | 'desChangeSetName'
  Text ->
  DescribeChangeSet
describeChangeSet pChangeSetName_ =
  DescribeChangeSet'
    { _desNextToken = Nothing,
      _desStackName = Nothing,
      _desChangeSetName = pChangeSetName_
    }

-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
desNextToken :: Lens' DescribeChangeSet (Maybe Text)
desNextToken = lens _desNextToken (\s a -> s {_desNextToken = a})

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
desStackName :: Lens' DescribeChangeSet (Maybe Text)
desStackName = lens _desStackName (\s a -> s {_desStackName = a})

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
desChangeSetName :: Lens' DescribeChangeSet Text
desChangeSetName = lens _desChangeSetName (\s a -> s {_desChangeSetName = a})

instance AWSPager DescribeChangeSet where
  page rq rs
    | stop (rs ^. dcscrsNextToken) = Nothing
    | stop (rs ^. dcscrsChanges) = Nothing
    | otherwise = Just $ rq & desNextToken .~ rs ^. dcscrsNextToken

instance AWSRequest DescribeChangeSet where
  type Rs DescribeChangeSet = DescribeChangeSetResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DescribeChangeSetResult"
      ( \s h x ->
          DescribeChangeSetResponse'
            <$> (x .@? "CreationTime")
            <*> (x .@? "ParentChangeSetId")
            <*> (x .@? "Changes" .!@ mempty >>= may (parseXMLList "member"))
            <*> ( x .@? "NotificationARNs" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (x .@? "ChangeSetName")
            <*> (x .@? "ExecutionStatus")
            <*> (x .@? "ChangeSetId")
            <*> (x .@? "IncludeNestedStacks")
            <*> (x .@? "NextToken")
            <*> (x .@? "RootChangeSetId")
            <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "StatusReason")
            <*> (x .@? "StackId")
            <*> (x .@? "Description")
            <*> (x .@? "Capabilities" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "RollbackConfiguration")
            <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "StackName")
            <*> (pure (fromEnum s))
            <*> (x .@ "Status")
      )

instance Hashable DescribeChangeSet

instance NFData DescribeChangeSet

instance ToHeaders DescribeChangeSet where
  toHeaders = const mempty

instance ToPath DescribeChangeSet where
  toPath = const "/"

instance ToQuery DescribeChangeSet where
  toQuery DescribeChangeSet' {..} =
    mconcat
      [ "Action" =: ("DescribeChangeSet" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "NextToken" =: _desNextToken,
        "StackName" =: _desStackName,
        "ChangeSetName" =: _desChangeSetName
      ]

-- | The output for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { _dcscrsCreationTime ::
      !(Maybe ISO8601),
    _dcscrsParentChangeSetId ::
      !(Maybe Text),
    _dcscrsChanges :: !(Maybe [Change]),
    _dcscrsNotificationARNs ::
      !(Maybe [Text]),
    _dcscrsChangeSetName :: !(Maybe Text),
    _dcscrsExecutionStatus ::
      !(Maybe ExecutionStatus),
    _dcscrsChangeSetId :: !(Maybe Text),
    _dcscrsIncludeNestedStacks ::
      !(Maybe Bool),
    _dcscrsNextToken :: !(Maybe Text),
    _dcscrsRootChangeSetId :: !(Maybe Text),
    _dcscrsParameters ::
      !(Maybe [Parameter]),
    _dcscrsStatusReason :: !(Maybe Text),
    _dcscrsStackId :: !(Maybe Text),
    _dcscrsDescription :: !(Maybe Text),
    _dcscrsCapabilities ::
      !(Maybe [Capability]),
    _dcscrsRollbackConfiguration ::
      !(Maybe RollbackConfiguration),
    _dcscrsTags :: !(Maybe [Tag]),
    _dcscrsStackName :: !(Maybe Text),
    _dcscrsResponseStatus :: !Int,
    _dcscrsStatus :: !ChangeSetStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscrsCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'dcscrsParentChangeSetId' - Specifies the change set ID of the parent change set in the current nested change set hierarchy.
--
-- * 'dcscrsChanges' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- * 'dcscrsNotificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- * 'dcscrsChangeSetName' - The name of the change set.
--
-- * 'dcscrsExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- * 'dcscrsChangeSetId' - The ARN of the change set.
--
-- * 'dcscrsIncludeNestedStacks' - Verifies if @IncludeNestedStacks@ is set to @True@ .
--
-- * 'dcscrsNextToken' - If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- * 'dcscrsRootChangeSetId' - Specifies the change set ID of the root change set in the current nested change set hierarchy.
--
-- * 'dcscrsParameters' - A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- * 'dcscrsStatusReason' - A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- * 'dcscrsStackId' - The ARN of the stack that is associated with the change set.
--
-- * 'dcscrsDescription' - Information about the change set.
--
-- * 'dcscrsCapabilities' - If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- * 'dcscrsRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'dcscrsTags' - If you execute the change set, the tags that will be associated with the stack.
--
-- * 'dcscrsStackName' - The name of the stack that is associated with the change set.
--
-- * 'dcscrsResponseStatus' - -- | The response status code.
--
-- * 'dcscrsStatus' - The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
describeChangeSetResponse ::
  -- | 'dcscrsResponseStatus'
  Int ->
  -- | 'dcscrsStatus'
  ChangeSetStatus ->
  DescribeChangeSetResponse
describeChangeSetResponse pResponseStatus_ pStatus_ =
  DescribeChangeSetResponse'
    { _dcscrsCreationTime = Nothing,
      _dcscrsParentChangeSetId = Nothing,
      _dcscrsChanges = Nothing,
      _dcscrsNotificationARNs = Nothing,
      _dcscrsChangeSetName = Nothing,
      _dcscrsExecutionStatus = Nothing,
      _dcscrsChangeSetId = Nothing,
      _dcscrsIncludeNestedStacks = Nothing,
      _dcscrsNextToken = Nothing,
      _dcscrsRootChangeSetId = Nothing,
      _dcscrsParameters = Nothing,
      _dcscrsStatusReason = Nothing,
      _dcscrsStackId = Nothing,
      _dcscrsDescription = Nothing,
      _dcscrsCapabilities = Nothing,
      _dcscrsRollbackConfiguration = Nothing,
      _dcscrsTags = Nothing,
      _dcscrsStackName = Nothing,
      _dcscrsResponseStatus = pResponseStatus_,
      _dcscrsStatus = pStatus_
    }

-- | The start time when the change set was created, in UTC.
dcscrsCreationTime :: Lens' DescribeChangeSetResponse (Maybe UTCTime)
dcscrsCreationTime = lens _dcscrsCreationTime (\s a -> s {_dcscrsCreationTime = a}) . mapping _Time

-- | Specifies the change set ID of the parent change set in the current nested change set hierarchy.
dcscrsParentChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsParentChangeSetId = lens _dcscrsParentChangeSetId (\s a -> s {_dcscrsParentChangeSetId = a})

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
dcscrsChanges :: Lens' DescribeChangeSetResponse [Change]
dcscrsChanges = lens _dcscrsChanges (\s a -> s {_dcscrsChanges = a}) . _Default . _Coerce

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
dcscrsNotificationARNs :: Lens' DescribeChangeSetResponse [Text]
dcscrsNotificationARNs = lens _dcscrsNotificationARNs (\s a -> s {_dcscrsNotificationARNs = a}) . _Default . _Coerce

-- | The name of the change set.
dcscrsChangeSetName :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsChangeSetName = lens _dcscrsChangeSetName (\s a -> s {_dcscrsChangeSetName = a})

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
dcscrsExecutionStatus :: Lens' DescribeChangeSetResponse (Maybe ExecutionStatus)
dcscrsExecutionStatus = lens _dcscrsExecutionStatus (\s a -> s {_dcscrsExecutionStatus = a})

-- | The ARN of the change set.
dcscrsChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsChangeSetId = lens _dcscrsChangeSetId (\s a -> s {_dcscrsChangeSetId = a})

-- | Verifies if @IncludeNestedStacks@ is set to @True@ .
dcscrsIncludeNestedStacks :: Lens' DescribeChangeSetResponse (Maybe Bool)
dcscrsIncludeNestedStacks = lens _dcscrsIncludeNestedStacks (\s a -> s {_dcscrsIncludeNestedStacks = a})

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
dcscrsNextToken :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsNextToken = lens _dcscrsNextToken (\s a -> s {_dcscrsNextToken = a})

-- | Specifies the change set ID of the root change set in the current nested change set hierarchy.
dcscrsRootChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsRootChangeSetId = lens _dcscrsRootChangeSetId (\s a -> s {_dcscrsRootChangeSetId = a})

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
dcscrsParameters :: Lens' DescribeChangeSetResponse [Parameter]
dcscrsParameters = lens _dcscrsParameters (\s a -> s {_dcscrsParameters = a}) . _Default . _Coerce

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
dcscrsStatusReason :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsStatusReason = lens _dcscrsStatusReason (\s a -> s {_dcscrsStatusReason = a})

-- | The ARN of the stack that is associated with the change set.
dcscrsStackId :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsStackId = lens _dcscrsStackId (\s a -> s {_dcscrsStackId = a})

-- | Information about the change set.
dcscrsDescription :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsDescription = lens _dcscrsDescription (\s a -> s {_dcscrsDescription = a})

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
dcscrsCapabilities :: Lens' DescribeChangeSetResponse [Capability]
dcscrsCapabilities = lens _dcscrsCapabilities (\s a -> s {_dcscrsCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
dcscrsRollbackConfiguration :: Lens' DescribeChangeSetResponse (Maybe RollbackConfiguration)
dcscrsRollbackConfiguration = lens _dcscrsRollbackConfiguration (\s a -> s {_dcscrsRollbackConfiguration = a})

-- | If you execute the change set, the tags that will be associated with the stack.
dcscrsTags :: Lens' DescribeChangeSetResponse [Tag]
dcscrsTags = lens _dcscrsTags (\s a -> s {_dcscrsTags = a}) . _Default . _Coerce

-- | The name of the stack that is associated with the change set.
dcscrsStackName :: Lens' DescribeChangeSetResponse (Maybe Text)
dcscrsStackName = lens _dcscrsStackName (\s a -> s {_dcscrsStackName = a})

-- | -- | The response status code.
dcscrsResponseStatus :: Lens' DescribeChangeSetResponse Int
dcscrsResponseStatus = lens _dcscrsResponseStatus (\s a -> s {_dcscrsResponseStatus = a})

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
dcscrsStatus :: Lens' DescribeChangeSetResponse ChangeSetStatus
dcscrsStatus = lens _dcscrsStatus (\s a -> s {_dcscrsStatus = a})

instance NFData DescribeChangeSetResponse
