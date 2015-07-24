{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the actions scheduled for your Auto Scaling group that
-- haven\'t run. To describe the actions that have already run, use
-- DescribeScalingActivities.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScheduledActions.html>
module Network.AWS.AutoScaling.DescribeScheduledActions
    (
    -- * Request
      DescribeScheduledActions
    -- ** Request constructor
    , describeScheduledActions
    -- ** Request lenses
    , dsasStartTime
    , dsasNextToken
    , dsasMaxRecords
    , dsasEndTime
    , dsasAutoScalingGroupName
    , dsasScheduledActionNames

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response constructor
    , describeScheduledActionsResponse
    -- ** Response lenses
    , dsarsScheduledUpdateGroupActions
    , dsarsNextToken
    , dsarsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScheduledActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsasStartTime'
--
-- * 'dsasNextToken'
--
-- * 'dsasMaxRecords'
--
-- * 'dsasEndTime'
--
-- * 'dsasAutoScalingGroupName'
--
-- * 'dsasScheduledActionNames'
data DescribeScheduledActions = DescribeScheduledActions'
    { _dsasStartTime            :: !(Maybe ISO8601)
    , _dsasNextToken            :: !(Maybe Text)
    , _dsasMaxRecords           :: !(Maybe Int)
    , _dsasEndTime              :: !(Maybe ISO8601)
    , _dsasAutoScalingGroupName :: !(Maybe Text)
    , _dsasScheduledActionNames :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScheduledActions' smart constructor.
describeScheduledActions :: DescribeScheduledActions
describeScheduledActions =
    DescribeScheduledActions'
    { _dsasStartTime = Nothing
    , _dsasNextToken = Nothing
    , _dsasMaxRecords = Nothing
    , _dsasEndTime = Nothing
    , _dsasAutoScalingGroupName = Nothing
    , _dsasScheduledActionNames = Nothing
    }

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this parameter is ignored.
dsasStartTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasStartTime = lens _dsasStartTime (\ s a -> s{_dsasStartTime = a}) . mapping _Time;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dsasNextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsasNextToken = lens _dsasNextToken (\ s a -> s{_dsasNextToken = a});

-- | The maximum number of items to return with this call.
dsasMaxRecords :: Lens' DescribeScheduledActions (Maybe Int)
dsasMaxRecords = lens _dsasMaxRecords (\ s a -> s{_dsasMaxRecords = a});

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this parameter is ignored.
dsasEndTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasEndTime = lens _dsasEndTime (\ s a -> s{_dsasEndTime = a}) . mapping _Time;

-- | The name of the group.
dsasAutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
dsasAutoScalingGroupName = lens _dsasAutoScalingGroupName (\ s a -> s{_dsasAutoScalingGroupName = a});

-- | Describes one or more scheduled actions. If you omit this list, the call
-- describes all scheduled actions. If you specify an unknown scheduled
-- action it is ignored with no error.
--
-- You can describe up to a maximum of 50 instances with a single call. If
-- there are more items to return, the call returns a token. To get the
-- next set of items, repeat the call with the returned token in the
-- @NextToken@ parameter.
dsasScheduledActionNames :: Lens' DescribeScheduledActions [Text]
dsasScheduledActionNames = lens _dsasScheduledActionNames (\ s a -> s{_dsasScheduledActionNames = a}) . _Default;

instance AWSPager DescribeScheduledActions where
        page rq rs
          | stop (rs ^. dsarsNextToken) = Nothing
          | stop (rs ^. dsarsScheduledUpdateGroupActions) =
            Nothing
          | otherwise =
            Just $ rq & dsasNextToken .~ rs ^. dsarsNextToken

instance AWSRequest DescribeScheduledActions where
        type Sv DescribeScheduledActions = AutoScaling
        type Rs DescribeScheduledActions =
             DescribeScheduledActionsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeScheduledActionsResult"
              (\ s h x ->
                 DescribeScheduledActionsResponse' <$>
                   (x .@? "ScheduledUpdateGroupActions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeScheduledActions where
        toHeaders = const mempty

instance ToPath DescribeScheduledActions where
        toPath = const "/"

instance ToQuery DescribeScheduledActions where
        toQuery DescribeScheduledActions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeScheduledActions" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "StartTime" =: _dsasStartTime,
               "NextToken" =: _dsasNextToken,
               "MaxRecords" =: _dsasMaxRecords,
               "EndTime" =: _dsasEndTime,
               "AutoScalingGroupName" =: _dsasAutoScalingGroupName,
               "ScheduledActionNames" =:
                 toQuery
                   (toQueryList "member" <$> _dsasScheduledActionNames)]

-- | /See:/ 'describeScheduledActionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsarsScheduledUpdateGroupActions'
--
-- * 'dsarsNextToken'
--
-- * 'dsarsStatus'
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
    { _dsarsScheduledUpdateGroupActions :: !(Maybe [ScheduledUpdateGroupAction])
    , _dsarsNextToken                   :: !(Maybe Text)
    , _dsarsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScheduledActionsResponse' smart constructor.
describeScheduledActionsResponse :: Int -> DescribeScheduledActionsResponse
describeScheduledActionsResponse pStatus_ =
    DescribeScheduledActionsResponse'
    { _dsarsScheduledUpdateGroupActions = Nothing
    , _dsarsNextToken = Nothing
    , _dsarsStatus = pStatus_
    }

-- | The scheduled actions.
dsarsScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse [ScheduledUpdateGroupAction]
dsarsScheduledUpdateGroupActions = lens _dsarsScheduledUpdateGroupActions (\ s a -> s{_dsarsScheduledUpdateGroupActions = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dsarsNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\ s a -> s{_dsarsNextToken = a});

-- | FIXME: Undocumented member.
dsarsStatus :: Lens' DescribeScheduledActionsResponse Int
dsarsStatus = lens _dsarsStatus (\ s a -> s{_dsarsStatus = a});
