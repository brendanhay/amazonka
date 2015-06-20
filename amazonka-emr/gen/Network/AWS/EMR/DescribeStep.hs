{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EMR.DescribeStep
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

-- | Provides more detail about the cluster step.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeStep.html>
module Network.AWS.EMR.DescribeStep
    (
    -- * Request
      DescribeStep
    -- ** Request constructor
    , describeStep
    -- ** Request lenses
    , dsClusterId
    , dsStepId

    -- * Response
    , DescribeStepResponse
    -- ** Response constructor
    , describeStepResponse
    -- ** Response lenses
    , dsrStep
    ) where

import Network.AWS.EMR.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStep' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsClusterId'
--
-- * 'dsStepId'
data DescribeStep = DescribeStep'{_dsClusterId :: Text, _dsStepId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeStep' smart constructor.
describeStep :: Text -> Text -> DescribeStep
describeStep pClusterId pStepId = DescribeStep'{_dsClusterId = pClusterId, _dsStepId = pStepId};

-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\ s a -> s{_dsClusterId = a});

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\ s a -> s{_dsStepId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeStep where
        type Sv DescribeStep = EMR
        type Rs DescribeStep = DescribeStepResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> DescribeStepResponse' <$> (x .?> "Step"))

instance ToHeaders DescribeStep where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.DescribeStep" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStep where
        toJSON DescribeStep'{..}
          = object
              ["ClusterId" .= _dsClusterId, "StepId" .= _dsStepId]

instance ToPath DescribeStep where
        toPath = const "/"

instance ToQuery DescribeStep where
        toQuery = const mempty

-- | /See:/ 'describeStepResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStep'
newtype DescribeStepResponse = DescribeStepResponse'{_dsrStep :: Maybe Step} deriving (Eq, Read, Show)

-- | 'DescribeStepResponse' smart constructor.
describeStepResponse :: DescribeStepResponse
describeStepResponse = DescribeStepResponse'{_dsrStep = Nothing};

-- | The step details for the requested step identifier.
dsrStep :: Lens' DescribeStepResponse (Maybe Step)
dsrStep = lens _dsrStep (\ s a -> s{_dsrStep = a});
