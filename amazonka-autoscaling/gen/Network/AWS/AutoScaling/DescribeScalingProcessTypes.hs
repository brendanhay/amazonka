{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with ResumeProcesses and
-- SuspendProcesses.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypes.html>
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    (
    -- * Request
      DescribeScalingProcessTypes
    -- ** Request constructor
    , describeScalingProcessTypes

    -- * Response
    , DescribeScalingProcessTypesResponse
    -- ** Response constructor
    , describeScalingProcessTypesResponse
    -- ** Response lenses
    , dsptrsProcesses
    , dsptrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes =
    DescribeScalingProcessTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingProcessTypes' smart constructor.
describeScalingProcessTypes :: DescribeScalingProcessTypes
describeScalingProcessTypes = DescribeScalingProcessTypes'

instance AWSRequest DescribeScalingProcessTypes where
        type Sv DescribeScalingProcessTypes = AutoScaling
        type Rs DescribeScalingProcessTypes =
             DescribeScalingProcessTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeScalingProcessTypesResult"
              (\ s h x ->
                 DescribeScalingProcessTypesResponse' <$>
                   (x .@? "Processes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeScalingProcessTypes where
        toHeaders = const mempty

instance ToPath DescribeScalingProcessTypes where
        toPath = const "/"

instance ToQuery DescribeScalingProcessTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeScalingProcessTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeScalingProcessTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsptrsProcesses'
--
-- * 'dsptrsStatus'
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
    { _dsptrsProcesses :: !(Maybe [ProcessType])
    , _dsptrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingProcessTypesResponse' smart constructor.
describeScalingProcessTypesResponse :: Int -> DescribeScalingProcessTypesResponse
describeScalingProcessTypesResponse pStatus_ =
    DescribeScalingProcessTypesResponse'
    { _dsptrsProcesses = Nothing
    , _dsptrsStatus = pStatus_
    }

-- | The names of the process types.
dsptrsProcesses :: Lens' DescribeScalingProcessTypesResponse [ProcessType]
dsptrsProcesses = lens _dsptrsProcesses (\ s a -> s{_dsptrsProcesses = a}) . _Default;

-- | FIXME: Undocumented member.
dsptrsStatus :: Lens' DescribeScalingProcessTypesResponse Int
dsptrsStatus = lens _dsptrsStatus (\ s a -> s{_dsptrsStatus = a});
