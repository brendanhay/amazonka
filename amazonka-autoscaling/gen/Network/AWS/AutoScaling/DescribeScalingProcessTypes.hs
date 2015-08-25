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
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with ResumeProcesses and
-- SuspendProcesses.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypes.html AWS API Reference> for DescribeScalingProcessTypes.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    (
    -- * Creating a Request
      describeScalingProcessTypes
    , DescribeScalingProcessTypes

    -- * Destructuring the Response
    , describeScalingProcessTypesResponse
    , DescribeScalingProcessTypesResponse
    -- * Response Lenses
    , dsptrsProcesses
    , dsptrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes =
    DescribeScalingProcessTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingProcessTypes' with the minimum fields required to make a request.
--
describeScalingProcessTypes
    :: DescribeScalingProcessTypes
describeScalingProcessTypes = DescribeScalingProcessTypes'

instance AWSRequest DescribeScalingProcessTypes where
        type Rs DescribeScalingProcessTypes =
             DescribeScalingProcessTypesResponse
        request = postQuery autoScaling
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
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
    { _dsptrsProcesses :: !(Maybe [ProcessType])
    , _dsptrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingProcessTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsptrsProcesses'
--
-- * 'dsptrsStatus'
describeScalingProcessTypesResponse
    :: Int -- ^ 'dsptrsStatus'
    -> DescribeScalingProcessTypesResponse
describeScalingProcessTypesResponse pStatus_ =
    DescribeScalingProcessTypesResponse'
    { _dsptrsProcesses = Nothing
    , _dsptrsStatus = pStatus_
    }

-- | The names of the process types.
dsptrsProcesses :: Lens' DescribeScalingProcessTypesResponse [ProcessType]
dsptrsProcesses = lens _dsptrsProcesses (\ s a -> s{_dsptrsProcesses = a}) . _Default . _Coerce;

-- | The response status code.
dsptrsStatus :: Lens' DescribeScalingProcessTypesResponse Int
dsptrsStatus = lens _dsptrsStatus (\ s a -> s{_dsptrsStatus = a});
