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
--
-- This operation returns paginated results.
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
    , dcscrsCreationTime
    , dcscrsChanges
    , dcscrsNotificationARNs
    , dcscrsChangeSetName
    , dcscrsExecutionStatus
    , dcscrsChangeSetId
    , dcscrsNextToken
    , dcscrsParameters
    , dcscrsStatusReason
    , dcscrsStackId
    , dcscrsDescription
    , dcscrsCapabilities
    , dcscrsRollbackConfiguration
    , dcscrsTags
    , dcscrsStackName
    , dcscrsResponseStatus
    , dcscrsStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
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

instance AWSPager DescribeChangeSet where
        page rq rs
          | stop (rs ^. dcscrsNextToken) = Nothing
          | stop (rs ^. dcscrsChanges) = Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dcscrsNextToken

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
  { _dcscrsCreationTime          :: !(Maybe ISO8601)
  , _dcscrsChanges               :: !(Maybe [Change])
  , _dcscrsNotificationARNs      :: !(Maybe [Text])
  , _dcscrsChangeSetName         :: !(Maybe Text)
  , _dcscrsExecutionStatus       :: !(Maybe ExecutionStatus)
  , _dcscrsChangeSetId           :: !(Maybe Text)
  , _dcscrsNextToken             :: !(Maybe Text)
  , _dcscrsParameters            :: !(Maybe [Parameter])
  , _dcscrsStatusReason          :: !(Maybe Text)
  , _dcscrsStackId               :: !(Maybe Text)
  , _dcscrsDescription           :: !(Maybe Text)
  , _dcscrsCapabilities          :: !(Maybe [Capability])
  , _dcscrsRollbackConfiguration :: !(Maybe RollbackConfiguration)
  , _dcscrsTags                  :: !(Maybe [Tag])
  , _dcscrsStackName             :: !(Maybe Text)
  , _dcscrsResponseStatus        :: !Int
  , _dcscrsStatus                :: !ChangeSetStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscrsCreationTime' - The start time when the change set was created, in UTC.
--
-- * 'dcscrsChanges' - A list of @Change@ structures that describes the resources AWS CloudFormation changes if you execute the change set.
--
-- * 'dcscrsNotificationARNs' - The ARNs of the Amazon Simple Notification Service (Amazon SNS) topics that will be associated with the stack if you execute the change set.
--
-- * 'dcscrsChangeSetName' - The name of the change set.
--
-- * 'dcscrsExecutionStatus' - If the change set execution status is @AVAILABLE@ , you can execute the change set. If you can
