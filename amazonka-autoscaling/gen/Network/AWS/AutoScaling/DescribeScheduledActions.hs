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
-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the actions scheduled for your Auto Scaling group that haven't run. To describe the actions that have already run, use 'DescribeScalingActivities' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScheduledActions
    (
    -- * Creating a Request
      describeScheduledActions
    , DescribeScheduledActions
    -- * Request Lenses
    , dsasStartTime
    , dsasNextToken
    , dsasAutoScalingGroupName
    , dsasMaxRecords
    , dsasEndTime
    , dsasScheduledActionNames

    -- * Destructuring the Response
    , describeScheduledActionsResponse
    , DescribeScheduledActionsResponse
    -- * Response Lenses
    , dsarsScheduledUpdateGroupActions
    , dsarsNextToken
    , dsarsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { _dsasStartTime            :: !(Maybe ISO8601)
  , _dsasNextToken            :: !(Maybe Text)
  , _dsasAutoScalingGroupName :: !(Maybe Text)
  , _dsasMaxRecords           :: !(Maybe Int)
  , _dsasEndTime              :: !(Maybe ISO8601)
  , _dsasScheduledActionNames :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasStartTime' - The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- * 'dsasNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dsasAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dsasMaxRecords' - The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
--
-- * 'dsasEndTime' - The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- * 'dsasScheduledActionNames' - Describes one or more scheduled actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error. You can describe up to a maximum of 50 instances with a single call. If there are more items to return, the call returns a token. To get the next set of items, repeat the call with the returned token.
describeScheduledActions
    :: DescribeScheduledActions
describeScheduledActions =
  DescribeScheduledActions'
    { _dsasStartTime = Nothing
    , _dsasNextToken = Nothing
    , _dsasAutoScalingGroupName = Nothing
    , _dsasMaxRecords = Nothing
    , _dsasEndTime = Nothing
    , _dsasScheduledActionNames = Nothing
    }


-- | The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
dsasStartTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasStartTime = lens _dsasStartTime (\ s a -> s{_dsasStartTime = a}) . mapping _Time

-- | The token for the next set of items to return. (You received this token from a previous call.)
dsasNextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsasNextToken = lens _dsasNextToken (\ s a -> s{_dsasNextToken = a})

-- | The name of the Auto Scaling group.
dsasAutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
dsasAutoScalingGroupName = lens _dsasAutoScalingGroupName (\ s a -> s{_dsasAutoScalingGroupName = a})

-- | The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
dsasMaxRecords :: Lens' DescribeScheduledActions (Maybe Int)
dsasMaxRecords = lens _dsasMaxRecords (\ s a -> s{_dsasMaxRecords = a})

-- | The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
dsasEndTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasEndTime = lens _dsasEndTime (\ s a -> s{_dsasEndTime = a}) . mapping _Time

-- | Describes one or more scheduled actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error. You can describe up to a maximum of 50 instances with a single call. If there are more items to return, the call returns a token. To get the next set of items, repeat the call with the returned token.
dsasScheduledActionNames :: Lens' DescribeScheduledActions [Text]
dsasScheduledActionNames = lens _dsasScheduledActionNames (\ s a -> s{_dsasScheduledActionNames = a}) . _Default . _Coerce

instance AWSPager DescribeScheduledActions where
        page rq rs
          | stop (rs ^. dsarsNextToken) = Nothing
          | stop (rs ^. dsarsScheduledUpdateGroupActions) =
            Nothing
          | otherwise =
            Just $ rq & dsasNextToken .~ rs ^. dsarsNextToken

instance AWSRequest DescribeScheduledActions where
        type Rs DescribeScheduledActions =
             DescribeScheduledActionsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DescribeScheduledActionsResult"
              (\ s h x ->
                 DescribeScheduledActionsResponse' <$>
                   (x .@? "ScheduledUpdateGroupActions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScheduledActions where

instance NFData DescribeScheduledActions where

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
               "AutoScalingGroupName" =: _dsasAutoScalingGroupName,
               "MaxRecords" =: _dsasMaxRecords,
               "EndTime" =: _dsasEndTime,
               "ScheduledActionNames" =:
                 toQuery
                   (toQueryList "member" <$> _dsasScheduledActionNames)]

-- | /See:/ 'describeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { _dsarsScheduledUpdateGroupActions :: !(Maybe [ScheduledUpdateGroupAction])
  , _dsarsNextToken                   :: !(Maybe Text)
  , _dsarsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsScheduledUpdateGroupActions' - The scheduled actions.
--
-- * 'dsarsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeScheduledActionsResponse
    :: Int -- ^ 'dsarsResponseStatus'
    -> DescribeScheduledActionsResponse
describeScheduledActionsResponse pResponseStatus_ =
  DescribeScheduledActionsResponse'
    { _dsarsScheduledUpdateGroupActions = Nothing
    , _dsarsNextToken = Nothing
    , _dsarsResponseStatus = pResponseStatus_
    }


-- | The scheduled actions.
dsarsScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse [ScheduledUpdateGroupAction]
dsarsScheduledUpdateGroupActions = lens _dsarsScheduledUpdateGroupActions (\ s a -> s{_dsarsScheduledUpdateGroupActions = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dsarsNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\ s a -> s{_dsarsNextToken = a})

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeScheduledActionsResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\ s a -> s{_dsarsResponseStatus = a})

instance NFData DescribeScheduledActionsResponse
         where
