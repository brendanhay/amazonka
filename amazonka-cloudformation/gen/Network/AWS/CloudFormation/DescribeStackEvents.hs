{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackEvents.html>
module Network.AWS.CloudFormation.DescribeStackEvents
    (
    -- * Request
      DescribeStackEvents
    -- ** Request constructor
    , describeStackEvents
    -- ** Request lenses
    , dseNextToken
    , dseStackName

    -- * Response
    , DescribeStackEventsResponse
    -- ** Response constructor
    , describeStackEventsResponse
    -- ** Response lenses
    , dsersNextToken
    , dsersStackEvents
    , dsersStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for DescribeStackEvents action.
--
-- /See:/ 'describeStackEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dseNextToken'
--
-- * 'dseStackName'
data DescribeStackEvents = DescribeStackEvents'
    { _dseNextToken :: !(Maybe Text)
    , _dseStackName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackEvents' smart constructor.
describeStackEvents :: DescribeStackEvents
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
        type Sv DescribeStackEvents = CloudFormation
        type Rs DescribeStackEvents =
             DescribeStackEventsResponse
        request = post "DescribeStackEvents"
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsersNextToken'
--
-- * 'dsersStackEvents'
--
-- * 'dsersStatus'
data DescribeStackEventsResponse = DescribeStackEventsResponse'
    { _dsersNextToken   :: !(Maybe Text)
    , _dsersStackEvents :: !(Maybe [StackEvent])
    , _dsersStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStackEventsResponse' smart constructor.
describeStackEventsResponse :: Int -> DescribeStackEventsResponse
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

-- | A list of @StackEvents@ structures.
dsersStackEvents :: Lens' DescribeStackEventsResponse [StackEvent]
dsersStackEvents = lens _dsersStackEvents (\ s a -> s{_dsersStackEvents = a}) . _Default;

-- | FIXME: Undocumented member.
dsersStatus :: Lens' DescribeStackEventsResponse Int
dsersStatus = lens _dsersStatus (\ s a -> s{_dsersStatus = a});
