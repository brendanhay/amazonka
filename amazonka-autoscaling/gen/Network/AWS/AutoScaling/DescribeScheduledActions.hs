{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the actions scheduled for your Auto Scaling group that
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
    , desStartTime
    , desNextToken
    , desMaxRecords
    , desEndTime
    , desAutoScalingGroupName
    , desScheduledActionNames

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response constructor
    , describeScheduledActionsResponse
    -- ** Response lenses
    , dsarScheduledUpdateGroupActions
    , dsarNextToken
    , dsarStatusCode
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScheduledActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desStartTime'
--
-- * 'desNextToken'
--
-- * 'desMaxRecords'
--
-- * 'desEndTime'
--
-- * 'desAutoScalingGroupName'
--
-- * 'desScheduledActionNames'
data DescribeScheduledActions = DescribeScheduledActions'{_desStartTime :: Maybe ISO8601, _desNextToken :: Maybe Text, _desMaxRecords :: Maybe Int, _desEndTime :: Maybe ISO8601, _desAutoScalingGroupName :: Maybe Text, _desScheduledActionNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeScheduledActions' smart constructor.
describeScheduledActions :: DescribeScheduledActions
describeScheduledActions = DescribeScheduledActions'{_desStartTime = Nothing, _desNextToken = Nothing, _desMaxRecords = Nothing, _desEndTime = Nothing, _desAutoScalingGroupName = Nothing, _desScheduledActionNames = Nothing};

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this parameter is ignored.
desStartTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
desStartTime = lens _desStartTime (\ s a -> s{_desStartTime = a}) . mapping _Time;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
desNextToken :: Lens' DescribeScheduledActions (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | The maximum number of items to return with this call.
desMaxRecords :: Lens' DescribeScheduledActions (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a});

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this parameter is ignored.
desEndTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
desEndTime = lens _desEndTime (\ s a -> s{_desEndTime = a}) . mapping _Time;

-- | The name of the group.
desAutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
desAutoScalingGroupName = lens _desAutoScalingGroupName (\ s a -> s{_desAutoScalingGroupName = a});

-- | Describes one or more scheduled actions. If you omit this list, the call
-- describes all scheduled actions. If you specify an unknown scheduled
-- action it is ignored with no error.
--
-- You can describe up to a maximum of 50 instances with a single call. If
-- there are more items to return, the call returns a token. To get the
-- next set of items, repeat the call with the returned token in the
-- @NextToken@ parameter.
desScheduledActionNames :: Lens' DescribeScheduledActions [Text]
desScheduledActionNames = lens _desScheduledActionNames (\ s a -> s{_desScheduledActionNames = a}) . _Default;

instance AWSPager DescribeScheduledActions where
        page rq rs
          | stop (rs ^. dsarNextToken) = Nothing
          | stop (rs ^. dsarScheduledUpdateGroupActions) =
            Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dsarNextToken

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
               "StartTime" =: _desStartTime,
               "NextToken" =: _desNextToken,
               "MaxRecords" =: _desMaxRecords,
               "EndTime" =: _desEndTime,
               "AutoScalingGroupName" =: _desAutoScalingGroupName,
               "ScheduledActionNames" =:
                 toQuery
                   (toQueryList "member" <$> _desScheduledActionNames)]

-- | /See:/ 'describeScheduledActionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsarScheduledUpdateGroupActions'
--
-- * 'dsarNextToken'
--
-- * 'dsarStatusCode'
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'{_dsarScheduledUpdateGroupActions :: Maybe [ScheduledUpdateGroupAction], _dsarNextToken :: Maybe Text, _dsarStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeScheduledActionsResponse' smart constructor.
describeScheduledActionsResponse :: Int -> DescribeScheduledActionsResponse
describeScheduledActionsResponse pStatusCode = DescribeScheduledActionsResponse'{_dsarScheduledUpdateGroupActions = Nothing, _dsarNextToken = Nothing, _dsarStatusCode = pStatusCode};

-- | The scheduled actions.
dsarScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse [ScheduledUpdateGroupAction]
dsarScheduledUpdateGroupActions = lens _dsarScheduledUpdateGroupActions (\ s a -> s{_dsarScheduledUpdateGroupActions = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dsarNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarNextToken = lens _dsarNextToken (\ s a -> s{_dsarNextToken = a});

-- | FIXME: Undocumented member.
dsarStatusCode :: Lens' DescribeScheduledActionsResponse Int
dsarStatusCode = lens _dsarStatusCode (\ s a -> s{_dsarStatusCode = a});
