{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeChangeSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets> in the AWS CloudFormation User Guide.
--
--
module Network.AWS.CloudFormation.DescribeChangeSet
    (
    -- * Creating a Request
      describeChangeSet
    , DescribeChangeSet
    -- * Request Lenses
    , desNextToken
    , desStackName
    , desChangeSetName

    -- * Destructuring the Response
    , describeChangeSetResponse
    , DescribeChangeSetResponse
    -- * Response Lenses
    , desrsCreationTime
    , desrsChanges
    , desrsNotificationARNs
    , desrsChangeSetName
    , desrsExecutionStatus
    , desrsChangeSetId
    , desrsNextToken
    , desrsParameters
    , desrsStatusReason
    , desrsStackId
    , desrsDescription
    , desrsCapabilities
    , desrsRollbackConfiguration
    , desrsTags
    , desrsStackName
    , desrsResponseStatus
    , desrsStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { _desNextToken     :: !(Maybe Text)
  , _desStackName     :: !(Maybe Text)
  , _desChangeSetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken' - A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
--
-- * 'desStackName' - If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
--
-- * 'desChangeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to describe.
describeChangeSet
    :: Text -- ^ 'desChangeSetName'
    -> DescribeChangeSet
describeChangeSet pChangeSetName_ =
  DescribeChangeSet'
    { _desNextToken = Nothing
    , _desStackName = Nothing
    , _desChangeSetName = pChangeSetName_
    }


-- | A string (provided by the 'DescribeChangeSet' response output) that identifies the next page of information that you want to retrieve.
desNextToken :: Lens' DescribeChangeSet (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | If you specified the name of a change set, specify the stack name or ID (ARN) of the change set you want to describe.
desStackName :: Lens' DescribeChangeSet (Maybe Text)
desStackName = lens _desStackName (\ s a -> s{_desStackName = a})

-- | The name or Amazon Resource Name (ARN) of the change set that you want to describe.
desChangeSetName :: Lens' DescribeChangeSet Text
desChangeSetName = lens _desChangeSetName (\ s a -> s{_desChangeSetName = a})

instance AWSRequest DescribeChangeSet where
        type Rs DescribeChangeSet = DescribeChangeSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeChangeSetResult"
              (\ s h x ->
                 DescribeChangeSetResponse' <$>
                   (x .@? "CreationTime") <*>
                     (x .@? "Changes" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "NotificationARNs" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "ChangeSetName")
                     <*> (x .@? "ExecutionStatus")
                     <*> (x .@? "ChangeSetId")
                     <*> (x .@? "NextToken")
                     <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "StatusReason")
                     <*> (x .@? "StackId")
                     <*> (x .@? "Description")
                     <*>
                     (x .@? "Capabilities" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "RollbackConfiguration")
                     <*>
                     (x .@? "Tags" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "StackName")
                     <*> (pure (fromEnum s))
                     <*> (x .@ "Status"))

instance Hashable DescribeChangeSet where

instance NFData DescribeChangeSet where

instance ToHeaders DescribeChangeSet where
        toHeaders = const mempty

instance ToPath DescribeChangeSet where
        toPath = const "/"

instance ToQuery DescribeChangeSet where
        toQuery DescribeChangeSet'{..}
          = mconcat
              ["Action" =: ("DescribeChangeSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _desNextToken,
               "StackName" =: _desStackName,
               "ChangeSetName" =: _desChangeSetName]

-- | The output for the 'DescribeChangeSet' action.
--
--
--
-- /See:/ 'describeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { _desrsCreationTime          :: !(Maybe ISO8601)
  , _desrsChanges               :: !(Maybe [Change])
  , _desrsNotificationARNs      :: !(Maybe [Text])
  , _desrsChangeSetName         :: !(Maybe Text)
  , _desrsExecutionStatus       :: !(Maybe ExecutionStatus)
  , _desrsChangeSetId           :: !(Maybe Text)
  , _desrsNextToken             :: !(Maybe Text)
  , _desrsParameters            :: !(Maybe [Parameter])
  , _desrsStatusReason          :: !(Maybe Text)
  , _desrsStackId               :: !(Maybe Text)
  , _desrsDescription           :: !(Maybe Text)
  , _desrsCapabilities          :: !(Maybe [Capability])
  , _desrsRollbackConfiguration :: !(Maybe RollbackConfiguration)
  , _desrsTags                  :: !(Maybe [Tag])
  , _desrsStackName             :: !(Maybe Text)
  , _desrsResponseStatus        :: !Int
  , _desrsStatus                :: !ChangeSetStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'desrsChanges' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- * 'desrsNotificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- * 'desrsChangeSetName' - The name of the change set.
--
-- * 'desrsExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
--
-- * 'desrsChangeSetId' - The ARN of the change set.
--
-- * 'desrsNextToken' - If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
--
-- * 'desrsParameters' - A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- * 'desrsStatusReason' - A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
--
-- * 'desrsStackId' - The ARN of the stack that is associated with the change set.
--
-- * 'desrsDescription' - Information about the change set.
--
-- * 'desrsCapabilities' - If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
--
-- * 'desrsRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'desrsTags' - If you execute the change set, the tags that will be associated with the stack.
--
-- * 'desrsStackName' - The name of the stack that is associated with the change set.
--
-- * 'desrsResponseStatus' - -- | The response status code.
--
-- * 'desrsStatus' - The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
describeChangeSetResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> ChangeSetStatus -- ^ 'desrsStatus'
    -> DescribeChangeSetResponse
describeChangeSetResponse pResponseStatus_ pStatus_ =
  DescribeChangeSetResponse'
    { _desrsCreationTime = Nothing
    , _desrsChanges = Nothing
    , _desrsNotificationARNs = Nothing
    , _desrsChangeSetName = Nothing
    , _desrsExecutionStatus = Nothing
    , _desrsChangeSetId = Nothing
    , _desrsNextToken = Nothing
    , _desrsParameters = Nothing
    , _desrsStatusReason = Nothing
    , _desrsStackId = Nothing
    , _desrsDescription = Nothing
    , _desrsCapabilities = Nothing
    , _desrsRollbackConfiguration = Nothing
    , _desrsTags = Nothing
    , _desrsStackName = Nothing
    , _desrsResponseStatus = pResponseStatus_
    , _desrsStatus = pStatus_
    }


-- | The start time when the change set was created, in UTC.
desrsCreationTime :: Lens' DescribeChangeSetResponse (Maybe UTCTime)
desrsCreationTime = lens _desrsCreationTime (\ s a -> s{_desrsCreationTime = a}) . mapping _Time

-- | A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
desrsChanges :: Lens' DescribeChangeSetResponse [Change]
desrsChanges = lens _desrsChanges (\ s a -> s{_desrsChanges = a}) . _Default . _Coerce

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
desrsNotificationARNs :: Lens' DescribeChangeSetResponse [Text]
desrsNotificationARNs = lens _desrsNotificationARNs (\ s a -> s{_desrsNotificationARNs = a}) . _Default . _Coerce

-- | The name of the change set.
desrsChangeSetName :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsChangeSetName = lens _desrsChangeSetName (\ s a -> s{_desrsChangeSetName = a})

-- | If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can’t execute the change set, the status indicates why. For example, a change set might be in an @UNAVAILABLE@ state because AWS CloudFormation is still creating it or in an @OBSOLETE@ state because the stack was already updated.
desrsExecutionStatus :: Lens' DescribeChangeSetResponse (Maybe ExecutionStatus)
desrsExecutionStatus = lens _desrsExecutionStatus (\ s a -> s{_desrsExecutionStatus = a})

-- | The ARN of the change set.
desrsChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsChangeSetId = lens _desrsChangeSetId (\ s a -> s{_desrsChangeSetId = a})

-- | If the output exceeds 1 MB, a string that identifies the next page of changes. If there is no additional page, this value is null.
desrsNextToken :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a})

-- | A list of @Parameter@ structures that describes the input parameters and their values used to create the change set. For more information, see the <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
desrsParameters :: Lens' DescribeChangeSetResponse [Parameter]
desrsParameters = lens _desrsParameters (\ s a -> s{_desrsParameters = a}) . _Default . _Coerce

-- | A description of the change set's status. For example, if your attempt to create a change set failed, AWS CloudFormation shows the error message.
desrsStatusReason :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsStatusReason = lens _desrsStatusReason (\ s a -> s{_desrsStatusReason = a})

-- | The ARN of the stack that is associated with the change set.
desrsStackId :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsStackId = lens _desrsStackId (\ s a -> s{_desrsStackId = a})

-- | Information about the change set.
desrsDescription :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsDescription = lens _desrsDescription (\ s a -> s{_desrsDescription = a})

-- | If you execute the change set, the list of capabilities that were explicitly acknowledged when the change set was created.
desrsCapabilities :: Lens' DescribeChangeSetResponse [Capability]
desrsCapabilities = lens _desrsCapabilities (\ s a -> s{_desrsCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
desrsRollbackConfiguration :: Lens' DescribeChangeSetResponse (Maybe RollbackConfiguration)
desrsRollbackConfiguration = lens _desrsRollbackConfiguration (\ s a -> s{_desrsRollbackConfiguration = a})

-- | If you execute the change set, the tags that will be associated with the stack.
desrsTags :: Lens' DescribeChangeSetResponse [Tag]
desrsTags = lens _desrsTags (\ s a -> s{_desrsTags = a}) . _Default . _Coerce

-- | The name of the stack that is associated with the change set.
desrsStackName :: Lens' DescribeChangeSetResponse (Maybe Text)
desrsStackName = lens _desrsStackName (\ s a -> s{_desrsStackName = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeChangeSetResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

-- | The current status of the change set, such as @CREATE_IN_PROGRESS@ , @CREATE_COMPLETE@ , or @FAILED@ .
desrsStatus :: Lens' DescribeChangeSetResponse ChangeSetStatus
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a})

instance NFData DescribeChangeSetResponse where
