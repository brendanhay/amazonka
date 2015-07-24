{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHookTypes.html>
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , describeLifecycleHookTypes

    -- * Response
    , DescribeLifecycleHookTypesResponse
    -- ** Response constructor
    , describeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtrsLifecycleHookTypes
    , dlhtrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes =
    DescribeLifecycleHookTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLifecycleHookTypes' smart constructor.
describeLifecycleHookTypes :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes'

instance AWSRequest DescribeLifecycleHookTypes where
        type Sv DescribeLifecycleHookTypes = AutoScaling
        type Rs DescribeLifecycleHookTypes =
             DescribeLifecycleHookTypesResponse
        request = post "DescribeLifecycleHookTypes"
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhtrsLifecycleHookTypes'
--
-- * 'dlhtrsStatus'
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
    { _dlhtrsLifecycleHookTypes :: !(Maybe [Text])
    , _dlhtrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLifecycleHookTypesResponse' smart constructor.
describeLifecycleHookTypesResponse :: Int -> DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse pStatus_ =
    DescribeLifecycleHookTypesResponse'
    { _dlhtrsLifecycleHookTypes = Nothing
    , _dlhtrsStatus = pStatus_
    }

-- | One or more of the following notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCHING@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATING@
--
dlhtrsLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrsLifecycleHookTypes = lens _dlhtrsLifecycleHookTypes (\ s a -> s{_dlhtrsLifecycleHookTypes = a}) . _Default;

-- | FIXME: Undocumented member.
dlhtrsStatus :: Lens' DescribeLifecycleHookTypesResponse Int
dlhtrsStatus = lens _dlhtrsStatus (\ s a -> s{_dlhtrsStatus = a});
