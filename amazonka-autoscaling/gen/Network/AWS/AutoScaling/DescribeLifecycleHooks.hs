{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the lifecycle hooks for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHooks.html>
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Request
      DescribeLifecycleHooks
    -- ** Request constructor
    , describeLifecycleHooks
    -- ** Request lenses
    , dlhrqLifecycleHookNames
    , dlhrqAutoScalingGroupName

    -- * Response
    , DescribeLifecycleHooksResponse
    -- ** Response constructor
    , describeLifecycleHooksResponse
    -- ** Response lenses
    , dlhrsLifecycleHooks
    , dlhrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLifecycleHooks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhrqLifecycleHookNames'
--
-- * 'dlhrqAutoScalingGroupName'
data DescribeLifecycleHooks = DescribeLifecycleHooks'
    { _dlhrqLifecycleHookNames   :: !(Maybe [Text])
    , _dlhrqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLifecycleHooks' smart constructor.
describeLifecycleHooks :: Text -> DescribeLifecycleHooks
describeLifecycleHooks pAutoScalingGroupName_ =
    DescribeLifecycleHooks'
    { _dlhrqLifecycleHookNames = Nothing
    , _dlhrqAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The names of one or more lifecycle hooks.
dlhrqLifecycleHookNames :: Lens' DescribeLifecycleHooks [Text]
dlhrqLifecycleHookNames = lens _dlhrqLifecycleHookNames (\ s a -> s{_dlhrqLifecycleHookNames = a}) . _Default;

-- | The name of the group.
dlhrqAutoScalingGroupName :: Lens' DescribeLifecycleHooks Text
dlhrqAutoScalingGroupName = lens _dlhrqAutoScalingGroupName (\ s a -> s{_dlhrqAutoScalingGroupName = a});

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
                   (toQueryList "member" <$> _dlhrqLifecycleHookNames),
               "AutoScalingGroupName" =: _dlhrqAutoScalingGroupName]

-- | /See:/ 'describeLifecycleHooksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhrsLifecycleHooks'
--
-- * 'dlhrsStatus'
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
    { _dlhrsLifecycleHooks :: !(Maybe [LifecycleHook])
    , _dlhrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLifecycleHooksResponse' smart constructor.
describeLifecycleHooksResponse :: Int -> DescribeLifecycleHooksResponse
describeLifecycleHooksResponse pStatus_ =
    DescribeLifecycleHooksResponse'
    { _dlhrsLifecycleHooks = Nothing
    , _dlhrsStatus = pStatus_
    }

-- | The lifecycle hooks for the specified group.
dlhrsLifecycleHooks :: Lens' DescribeLifecycleHooksResponse [LifecycleHook]
dlhrsLifecycleHooks = lens _dlhrsLifecycleHooks (\ s a -> s{_dlhrsLifecycleHooks = a}) . _Default;

-- | FIXME: Undocumented member.
dlhrsStatus :: Lens' DescribeLifecycleHooksResponse Int
dlhrsStatus = lens _dlhrsStatus (\ s a -> s{_dlhrsStatus = a});
