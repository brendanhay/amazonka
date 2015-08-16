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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHookTypes.html AWS API Reference> for DescribeLifecycleHookTypes.
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Creating a Request
      describeLifecycleHookTypes
    , DescribeLifecycleHookTypes

    -- * Destructuring the Response
    , describeLifecycleHookTypesResponse
    , DescribeLifecycleHookTypesResponse
    -- * Response Lenses
    , dlhtrsLifecycleHookTypes
    , dlhtrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes =
    DescribeLifecycleHookTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLifecycleHookTypes' with the minimum fields required to make a request.
--
describeLifecycleHookTypes
    :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes'

instance AWSRequest DescribeLifecycleHookTypes where
        type Sv DescribeLifecycleHookTypes = AutoScaling
        type Rs DescribeLifecycleHookTypes =
             DescribeLifecycleHookTypesResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeLifecycleHookTypesResult"
              (\ s h x ->
                 DescribeLifecycleHookTypesResponse' <$>
                   (x .@? "LifecycleHookTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeLifecycleHookTypes where
        toHeaders = const mempty

instance ToPath DescribeLifecycleHookTypes where
        toPath = const "/"

instance ToQuery DescribeLifecycleHookTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeLifecycleHookTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
    { _dlhtrsLifecycleHookTypes :: !(Maybe [Text])
    , _dlhtrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLifecycleHookTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlhtrsLifecycleHookTypes'
--
-- * 'dlhtrsStatus'
describeLifecycleHookTypesResponse
    :: Int -- ^ 'dlhtrsStatus'
    -> DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse pStatus_ =
    DescribeLifecycleHookTypesResponse'
    { _dlhtrsLifecycleHookTypes = Nothing
    , _dlhtrsStatus = pStatus_
    }

-- | One or more of the following notification types:
--
-- -   'autoscaling:EC2_INSTANCE_LAUNCHING'
--
-- -   'autoscaling:EC2_INSTANCE_TERMINATING'
--
dlhtrsLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrsLifecycleHookTypes = lens _dlhtrsLifecycleHookTypes (\ s a -> s{_dlhtrsLifecycleHookTypes = a}) . _Default . _Coerce;

-- | The response status code.
dlhtrsStatus :: Lens' DescribeLifecycleHookTypesResponse Int
dlhtrsStatus = lens _dlhtrsStatus (\ s a -> s{_dlhtrsStatus = a});
