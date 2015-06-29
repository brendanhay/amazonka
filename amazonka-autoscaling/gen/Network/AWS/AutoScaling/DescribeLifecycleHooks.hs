{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
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

-- | Describes the lifecycle hooks for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHooks.html>
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Request
      DescribeLifecycleHooks
    -- ** Request constructor
    , describeLifecycleHooks
    -- ** Request lenses
    , dlhLifecycleHookNames
    , dlhAutoScalingGroupName

    -- * Response
    , DescribeLifecycleHooksResponse
    -- ** Response constructor
    , describeLifecycleHooksResponse
    -- ** Response lenses
    , dlhrLifecycleHooks
    , dlhrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLifecycleHooks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhLifecycleHookNames'
--
-- * 'dlhAutoScalingGroupName'
data DescribeLifecycleHooks = DescribeLifecycleHooks'
    { _dlhLifecycleHookNames   :: !(Maybe [Text])
    , _dlhAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeLifecycleHooks' smart constructor.
describeLifecycleHooks :: Text -> DescribeLifecycleHooks
describeLifecycleHooks pAutoScalingGroupName =
    DescribeLifecycleHooks'
    { _dlhLifecycleHookNames = Nothing
    , _dlhAutoScalingGroupName = pAutoScalingGroupName
    }

-- | The names of one or more lifecycle hooks.
dlhLifecycleHookNames :: Lens' DescribeLifecycleHooks [Text]
dlhLifecycleHookNames = lens _dlhLifecycleHookNames (\ s a -> s{_dlhLifecycleHookNames = a}) . _Default;

-- | The name of the group.
dlhAutoScalingGroupName :: Lens' DescribeLifecycleHooks Text
dlhAutoScalingGroupName = lens _dlhAutoScalingGroupName (\ s a -> s{_dlhAutoScalingGroupName = a});

instance AWSRequest DescribeLifecycleHooks where
        type Sv DescribeLifecycleHooks = AutoScaling
        type Rs DescribeLifecycleHooks =
             DescribeLifecycleHooksResponse
        request = post
        response
          = receiveXMLWrapper "DescribeLifecycleHooksResult"
              (\ s h x ->
                 DescribeLifecycleHooksResponse' <$>
                   (x .@? "LifecycleHooks" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeLifecycleHooks where
        toHeaders = const mempty

instance ToPath DescribeLifecycleHooks where
        toPath = const "/"

instance ToQuery DescribeLifecycleHooks where
        toQuery DescribeLifecycleHooks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLifecycleHooks" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LifecycleHookNames" =:
                 toQuery
                   (toQueryList "member" <$> _dlhLifecycleHookNames),
               "AutoScalingGroupName" =: _dlhAutoScalingGroupName]

-- | /See:/ 'describeLifecycleHooksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhrLifecycleHooks'
--
-- * 'dlhrStatus'
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
    { _dlhrLifecycleHooks :: !(Maybe [LifecycleHook])
    , _dlhrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeLifecycleHooksResponse' smart constructor.
describeLifecycleHooksResponse :: Int -> DescribeLifecycleHooksResponse
describeLifecycleHooksResponse pStatus =
    DescribeLifecycleHooksResponse'
    { _dlhrLifecycleHooks = Nothing
    , _dlhrStatus = pStatus
    }

-- | The lifecycle hooks for the specified group.
dlhrLifecycleHooks :: Lens' DescribeLifecycleHooksResponse [LifecycleHook]
dlhrLifecycleHooks = lens _dlhrLifecycleHooks (\ s a -> s{_dlhrLifecycleHooks = a}) . _Default;

-- | FIXME: Undocumented member.
dlhrStatus :: Lens' DescribeLifecycleHooksResponse Int
dlhrStatus = lens _dlhrStatus (\ s a -> s{_dlhrStatus = a});
