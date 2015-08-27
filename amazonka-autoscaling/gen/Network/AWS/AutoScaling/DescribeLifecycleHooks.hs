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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the lifecycle hooks for the specified Auto Scaling group.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHooks.html AWS API Reference> for DescribeLifecycleHooks.
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Creating a Request
      describeLifecycleHooks
    , DescribeLifecycleHooks
    -- * Request Lenses
    , dlhLifecycleHookNames
    , dlhAutoScalingGroupName

    -- * Destructuring the Response
    , describeLifecycleHooksResponse
    , DescribeLifecycleHooksResponse
    -- * Response Lenses
    , dlhrsLifecycleHooks
    , dlhrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLifecycleHooks' smart constructor.
data DescribeLifecycleHooks = DescribeLifecycleHooks'
    { _dlhLifecycleHookNames   :: !(Maybe [Text])
    , _dlhAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLifecycleHooks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlhLifecycleHookNames'
--
-- * 'dlhAutoScalingGroupName'
describeLifecycleHooks
    :: Text -- ^ 'dlhAutoScalingGroupName'
    -> DescribeLifecycleHooks
describeLifecycleHooks pAutoScalingGroupName_ =
    DescribeLifecycleHooks'
    { _dlhLifecycleHookNames = Nothing
    , _dlhAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The names of one or more lifecycle hooks.
dlhLifecycleHookNames :: Lens' DescribeLifecycleHooks [Text]
dlhLifecycleHookNames = lens _dlhLifecycleHookNames (\ s a -> s{_dlhLifecycleHookNames = a}) . _Default . _Coerce;

-- | The name of the group.
dlhAutoScalingGroupName :: Lens' DescribeLifecycleHooks Text
dlhAutoScalingGroupName = lens _dlhAutoScalingGroupName (\ s a -> s{_dlhAutoScalingGroupName = a});

instance AWSRequest DescribeLifecycleHooks where
        type Rs DescribeLifecycleHooks =
             DescribeLifecycleHooksResponse
        request = postQuery autoScaling
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
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
    { _dlhrsLifecycleHooks :: !(Maybe [LifecycleHook])
    , _dlhrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLifecycleHooksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlhrsLifecycleHooks'
--
-- * 'dlhrsStatus'
describeLifecycleHooksResponse
    :: Int -- ^ 'dlhrsStatus'
    -> DescribeLifecycleHooksResponse
describeLifecycleHooksResponse pStatus_ =
    DescribeLifecycleHooksResponse'
    { _dlhrsLifecycleHooks = Nothing
    , _dlhrsStatus = pStatus_
    }

-- | The lifecycle hooks for the specified group.
dlhrsLifecycleHooks :: Lens' DescribeLifecycleHooksResponse [LifecycleHook]
dlhrsLifecycleHooks = lens _dlhrsLifecycleHooks (\ s a -> s{_dlhrsLifecycleHooks = a}) . _Default . _Coerce;

-- | The response status code.
dlhrsStatus :: Lens' DescribeLifecycleHooksResponse Int
dlhrsStatus = lens _dlhrsStatus (\ s a -> s{_dlhrsStatus = a});
