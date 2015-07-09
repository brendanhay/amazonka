{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
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
    , dsrStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which step to describe.
--
-- /See:/ 'describeStep' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsClusterId'
--
-- * 'dsStepId'
data DescribeStep = DescribeStep'
    { _dsClusterId :: !Text
    , _dsStepId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStep' smart constructor.
describeStep :: Text -> Text -> DescribeStep
describeStep pClusterId pStepId =
    DescribeStep'
    { _dsClusterId = pClusterId
    , _dsStepId = pStepId
    }

-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\ s a -> s{_dsClusterId = a});

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\ s a -> s{_dsStepId = a});

instance AWSRequest DescribeStep where
        type Sv DescribeStep = EMR
        type Rs DescribeStep = DescribeStepResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStepResponse' <$>
                   (x .?> "Step") <*> (pure (fromEnum s)))

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

-- | This output contains the description of the cluster step.
--
-- /See:/ 'describeStepResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStep'
--
-- * 'dsrStatus'
data DescribeStepResponse = DescribeStepResponse'
    { _dsrStep   :: !(Maybe Step)
    , _dsrStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStepResponse' smart constructor.
describeStepResponse :: Int -> DescribeStepResponse
describeStepResponse pStatus =
    DescribeStepResponse'
    { _dsrStep = Nothing
    , _dsrStatus = pStatus
    }

-- | The step details for the requested step identifier.
dsrStep :: Lens' DescribeStepResponse (Maybe Step)
dsrStep = lens _dsrStep (\ s a -> s{_dsrStep = a});

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DescribeStepResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});
