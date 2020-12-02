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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with 'ResumeProcesses' and 'SuspendProcesses' .
--
--
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
    , dsptrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes =
  DescribeScalingProcessTypes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


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

instance Hashable DescribeScalingProcessTypes where

instance NFData DescribeScalingProcessTypes where

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
  { _dsptrsProcesses      :: !(Maybe [ProcessType])
  , _dsptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingProcessTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsptrsProcesses' - The names of the process types.
--
-- * 'dsptrsResponseStatus' - -- | The response status code.
describeScalingProcessTypesResponse
    :: Int -- ^ 'dsptrsResponseStatus'
    -> DescribeScalingProcessTypesResponse
describeScalingProcessTypesResponse pResponseStatus_ =
  DescribeScalingProcessTypesResponse'
    {_dsptrsProcesses = Nothing, _dsptrsResponseStatus = pResponseStatus_}


-- | The names of the process types.
dsptrsProcesses :: Lens' DescribeScalingProcessTypesResponse [ProcessType]
dsptrsProcesses = lens _dsptrsProcesses (\ s a -> s{_dsptrsProcesses = a}) . _Default . _Coerce

-- | -- | The response status code.
dsptrsResponseStatus :: Lens' DescribeScalingProcessTypesResponse Int
dsptrsResponseStatus = lens _dsptrsResponseStatus (\ s a -> s{_dsptrsResponseStatus = a})

instance NFData DescribeScalingProcessTypesResponse
         where
