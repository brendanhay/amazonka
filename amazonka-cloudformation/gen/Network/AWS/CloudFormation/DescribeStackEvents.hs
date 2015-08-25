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
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack. For more
-- information about a stack\'s event history, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks>
-- in the AWS CloudFormation User Guide.
--
-- You can list events for stacks that have failed to create or have been
-- deleted by specifying the unique stack identifier (stack ID).
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackEvents.html AWS API Reference> for DescribeStackEvents.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStackEvents
    (
    -- * Creating a Request
      describeStackEvents
    , DescribeStackEvents
    -- * Request Lenses
    , dseNextToken
    , dseStackName

    -- * Destructuring the Response
    , describeStackEventsResponse
    , DescribeStackEventsResponse
    -- * Response Lenses
    , dsersNextToken
    , dsersStackEvents
    , dsersStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStackEvents action.
--
-- /See:/ 'describeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
    { _dseNextToken :: !(Maybe Text)
    , _dseStackName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStackEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseNextToken'
--
-- * 'dseStackName'
describeStackEvents
    :: DescribeStackEvents
describeStackEvents =
    DescribeStackEvents'
    { _dseNextToken = Nothing
    , _dseStackName = Nothing
    }

-- | String that identifies the start of the next list of events, if there is
-- one.
--
-- Default: There is no default value.
dseNextToken :: Lens' DescribeStackEvents (Maybe Text)
dseNextToken = lens _dseNextToken (\ s a -> s{_dseNextToken = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
dseStackName :: Lens' DescribeStackEvents (Maybe Text)
dseStackName = lens _dseStackName (\ s a -> s{_dseStackName = a});

instance AWSPager DescribeStackEvents where
        page rq rs
          | stop (rs ^. dsersNextToken) = Nothing
          | stop (rs ^. dsersStackEvents) = Nothing
          | otherwise =
            Just $ rq & dseNextToken .~ rs ^. dsersNextToken

instance AWSRequest DescribeStackEvents where
        type Rs DescribeStackEvents =
             DescribeStackEventsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeStackEventsResult"
              (\ s h x ->
                 DescribeStackEventsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "StackEvents" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeStackEvents where
        toHeaders = const mempty

instance ToPath DescribeStackEvents where
        toPath = const "/"

instance ToQuery DescribeStackEvents where
        toQuery DescribeStackEvents'{..}
          = mconcat
              ["Action" =: ("DescribeStackEvents" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _dseNextToken,
               "StackName" =: _dseStackName]

-- | The output for a DescribeStackEvents action.
--
-- /See:/ 'describeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
    { _dsersNextToken   :: !(Maybe Text)
    , _dsersStackEvents :: !(Maybe [StackEvent])
    , _dsersStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStackEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsersNextToken'
--
-- * 'dsersStackEvents'
--
-- * 'dsersStatus'
describeStackEventsResponse
    :: Int -- ^ 'dsersStatus'
    -> DescribeStackEventsResponse
describeStackEventsResponse pStatus_ =
    DescribeStackEventsResponse'
    { _dsersNextToken = Nothing
    , _dsersStackEvents = Nothing
    , _dsersStatus = pStatus_
    }

-- | String that identifies the start of the next list of events, if there is
-- one.
dsersNextToken :: Lens' DescribeStackEventsResponse (Maybe Text)
dsersNextToken = lens _dsersNextToken (\ s a -> s{_dsersNextToken = a});

-- | A list of 'StackEvents' structures.
dsersStackEvents :: Lens' DescribeStackEventsResponse [StackEvent]
dsersStackEvents = lens _dsersStackEvents (\ s a -> s{_dsersStackEvents = a}) . _Default . _Coerce;

-- | The response status code.
dsersStatus :: Lens' DescribeStackEventsResponse Int
dsersStatus = lens _dsersStatus (\ s a -> s{_dsersStatus = a});
