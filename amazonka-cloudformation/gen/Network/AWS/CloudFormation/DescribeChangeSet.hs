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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the inputs for the change set and a list of changes that AWS
-- CloudFormation will make if you execute the change set. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html Updating Stacks Using Change Sets>
-- in the AWS CloudFormation User Guide.
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
    , drsCreationTime
    , drsStatus
    , drsChanges
    , drsNotificationARNs
    , drsChangeSetName
    , drsChangeSetId
    , drsNextToken
    , drsParameters
    , drsStatusReason
    , drsStackId
    , drsDescription
    , drsCapabilities
    , drsTags
    , drsStackName
    , drsResponseStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the < DescribeChangeSet> action.
--
-- /See:/ 'describeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
    { _desNextToken     :: !(Maybe Text)
    , _desStackName     :: !(Maybe Text)
    , _desChangeSetName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken'
--
-- * 'desStackName'
--
-- * 'desChangeSetName'
describeChangeSet
    :: Text -- ^ 'desChangeSetName'
    -> DescribeChangeSet
describeChangeSet pChangeSetName_ =
    DescribeChangeSet'
    { _desNextToken = Nothing
    , _desStackName = Nothing
    , _desChangeSetName = pChangeSetName_
    }

-- | A string (provided by the < DescribeChangeSet> response output) that
-- identifies the next page of information that you want to retrieve.
desNextToken :: Lens' DescribeChangeSet (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | If you specified the name of a change set, specify the stack name or ID
-- (ARN) of the change set you want to describe.
desStackName :: Lens' DescribeChangeSet (Maybe Text)
desStackName = lens _desStackName (\ s a -> s{_desStackName = a});

-- | The name or Amazon Resource Name (ARN) of the change set that you want
-- to describe.
desChangeSetName :: Lens' DescribeChangeSet Text
desChangeSetName = lens _desChangeSetName (\ s a -> s{_desChangeSetName = a});

instance AWSRequest DescribeChangeSet where
        type Rs DescribeChangeSet = DescribeChangeSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeChangeSetResult"
              (\ s h x ->
                 DescribeChangeSetResponse' <$>
                   (x .@? "CreationTime") <*> (x .@? "Status") <*>
                     (x .@? "Changes" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "NotificationARNs" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "ChangeSetName")
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
                     <*>
                     (x .@? "Tags" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "StackName")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeChangeSet

instance NFData DescribeChangeSet

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

-- | The output for the < DescribeChangeSet> action.
--
-- /See:/ 'describeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
    { _drsCreationTime     :: !(Maybe ISO8601)
    , _drsStatus           :: !(Maybe ChangeSetStatus)
    , _drsChanges          :: !(Maybe [Change])
    , _drsNotificationARNs :: !(Maybe [Text])
    , _drsChangeSetName    :: !(Maybe Text)
    , _drsChangeSetId      :: !(Maybe Text)
    , _drsNextToken        :: !(Maybe Text)
    , _drsParameters       :: !(Maybe [Parameter])
    , _drsStatusReason     :: !(Maybe Text)
    , _drsStackId          :: !(Maybe Text)
    , _drsDescription      :: !(Maybe Text)
    , _drsCapabilities     :: !(Maybe [Capability])
    , _drsTags             :: !(Maybe [Tag])
    , _drsStackName        :: !(Maybe Text)
    , _drsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCreationTime'
--
-- * 'drsStatus'
--
-- * 'drsChanges'
--
-- * 'drsNotificationARNs'
--
-- * 'drsChangeSetName'
--
-- * 'drsChangeSetId'
--
-- * 'drsNextToken'
--
-- * 'drsParameters'
--
-- * 'drsStatusReason'
--
-- * 'drsStackId'
--
-- * 'drsDescription'
--
-- * 'drsCapabilities'
--
-- * 'drsTags'
--
-- * 'drsStackName'
--
-- * 'drsResponseStatus'
describeChangeSetResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeChangeSetResponse
describeChangeSetResponse pResponseStatus_ =
    DescribeChangeSetResponse'
    { _drsCreationTime = Nothing
    , _drsStatus = Nothing
    , _drsChanges = Nothing
    , _drsNotificationARNs = Nothing
    , _drsChangeSetName = Nothing
    , _drsChangeSetId = Nothing
    , _drsNextToken = Nothing
    , _drsParameters = Nothing
    , _drsStatusReason = Nothing
    , _drsStackId = Nothing
    , _drsDescription = Nothing
    , _drsCapabilities = Nothing
    , _drsTags = Nothing
    , _drsStackName = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The start time when the change set was created, in UTC.
drsCreationTime :: Lens' DescribeChangeSetResponse (Maybe UTCTime)
drsCreationTime = lens _drsCreationTime (\ s a -> s{_drsCreationTime = a}) . mapping _Time;

-- | The current status of the change set, such as 'CREATE_IN_PROGRESS',
-- 'CREATE_COMPLETE', or 'FAILED'.
drsStatus :: Lens' DescribeChangeSetResponse (Maybe ChangeSetStatus)
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});

-- | A list of 'Change' structures that describes the resources AWS
-- CloudFormation changes if you execute the change set.
drsChanges :: Lens' DescribeChangeSetResponse [Change]
drsChanges = lens _drsChanges (\ s a -> s{_drsChanges = a}) . _Default . _Coerce;

-- | The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics
-- that will be associated with the stack if you execute the change set.
drsNotificationARNs :: Lens' DescribeChangeSetResponse [Text]
drsNotificationARNs = lens _drsNotificationARNs (\ s a -> s{_drsNotificationARNs = a}) . _Default . _Coerce;

-- | The name of the change set.
drsChangeSetName :: Lens' DescribeChangeSetResponse (Maybe Text)
drsChangeSetName = lens _drsChangeSetName (\ s a -> s{_drsChangeSetName = a});

-- | The ARN of the change set.
drsChangeSetId :: Lens' DescribeChangeSetResponse (Maybe Text)
drsChangeSetId = lens _drsChangeSetId (\ s a -> s{_drsChangeSetId = a});

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- changes. If there is no additional page, this value is null.
drsNextToken :: Lens' DescribeChangeSetResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a});

-- | A list of 'Parameter' structures that describes the input parameters and
-- their values used to create the change set. For more information, see
-- the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
drsParameters :: Lens' DescribeChangeSetResponse [Parameter]
drsParameters = lens _drsParameters (\ s a -> s{_drsParameters = a}) . _Default . _Coerce;

-- | A description of the change set\'s status. For example, if your attempt
-- to create a change set failed, AWS CloudFormation shows the error
-- message.
drsStatusReason :: Lens' DescribeChangeSetResponse (Maybe Text)
drsStatusReason = lens _drsStatusReason (\ s a -> s{_drsStatusReason = a});

-- | The ARN of the stack that is associated with the change set.
drsStackId :: Lens' DescribeChangeSetResponse (Maybe Text)
drsStackId = lens _drsStackId (\ s a -> s{_drsStackId = a});

-- | Information about the change set.
drsDescription :: Lens' DescribeChangeSetResponse (Maybe Text)
drsDescription = lens _drsDescription (\ s a -> s{_drsDescription = a});

-- | If you execute the change set, the list of capabilities that were
-- explicitly acknowledged when the change set was created.
drsCapabilities :: Lens' DescribeChangeSetResponse [Capability]
drsCapabilities = lens _drsCapabilities (\ s a -> s{_drsCapabilities = a}) . _Default . _Coerce;

-- | If you execute the change set, the tags that will be associated with the
-- stack.
drsTags :: Lens' DescribeChangeSetResponse [Tag]
drsTags = lens _drsTags (\ s a -> s{_drsTags = a}) . _Default . _Coerce;

-- | The name of the stack that is associated with the change set.
drsStackName :: Lens' DescribeChangeSetResponse (Maybe Text)
drsStackName = lens _drsStackName (\ s a -> s{_drsStackName = a});

-- | The response status code.
drsResponseStatus :: Lens' DescribeChangeSetResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DescribeChangeSetResponse
