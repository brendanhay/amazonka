{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes the available types of lifecycle hooks.
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
    , dlhtrLifecycleHookTypes
    , dlhtrStatus
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
        request = post
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
-- * 'dlhtrLifecycleHookTypes'
--
-- * 'dlhtrStatus'
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
    { _dlhtrLifecycleHookTypes :: !(Maybe [Text])
    , _dlhtrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLifecycleHookTypesResponse' smart constructor.
describeLifecycleHookTypesResponse :: Int -> DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse pStatus =
    DescribeLifecycleHookTypesResponse'
    { _dlhtrLifecycleHookTypes = Nothing
    , _dlhtrStatus = pStatus
    }

-- | One or more of the following notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCHING@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATING@
--
dlhtrLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrLifecycleHookTypes = lens _dlhtrLifecycleHookTypes (\ s a -> s{_dlhtrLifecycleHookTypes = a}) . _Default;

-- | FIXME: Undocumented member.
dlhtrStatus :: Lens' DescribeLifecycleHookTypesResponse Int
dlhtrStatus = lens _dlhtrStatus (\ s a -> s{_dlhtrStatus = a});
