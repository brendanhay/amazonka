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
-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeStep.html AWS API Reference> for DescribeStep.
module Network.AWS.EMR.DescribeStep
    (
    -- * Creating a Request
      describeStep
    , DescribeStep
    -- * Request Lenses
    , dsClusterId
    , dsStepId

    -- * Destructuring the Response
    , describeStepResponse
    , DescribeStepResponse
    -- * Response Lenses
    , dsrsStep
    , dsrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.EMR.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which step to describe.
--
-- /See:/ 'describeStep' smart constructor.
data DescribeStep = DescribeStep'
    { _dsClusterId :: !Text
    , _dsStepId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStep' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsClusterId'
--
-- * 'dsStepId'
describeStep
    :: Text -- ^ 'dsClusterId'
    -> Text -- ^ 'dsStepId'
    -> DescribeStep
describeStep pClusterId_ pStepId_ =
    DescribeStep'
    { _dsClusterId = pClusterId_
    , _dsStepId = pStepId_
    }

-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\ s a -> s{_dsClusterId = a});

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\ s a -> s{_dsStepId = a});

instance AWSRequest DescribeStep where
        type Rs DescribeStep = DescribeStepResponse
        request = postJSON eMR
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
              (catMaybes
                 [Just ("ClusterId" .= _dsClusterId),
                  Just ("StepId" .= _dsStepId)])

instance ToPath DescribeStep where
        toPath = const "/"

instance ToQuery DescribeStep where
        toQuery = const mempty

-- | This output contains the description of the cluster step.
--
-- /See:/ 'describeStepResponse' smart constructor.
data DescribeStepResponse = DescribeStepResponse'
    { _dsrsStep   :: !(Maybe Step)
    , _dsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStepResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsStep'
--
-- * 'dsrsStatus'
describeStepResponse
    :: Int -- ^ 'dsrsStatus'
    -> DescribeStepResponse
describeStepResponse pStatus_ =
    DescribeStepResponse'
    { _dsrsStep = Nothing
    , _dsrsStatus = pStatus_
    }

-- | The step details for the requested step identifier.
dsrsStep :: Lens' DescribeStepResponse (Maybe Step)
dsrsStep = lens _dsrsStep (\ s a -> s{_dsrsStep = a});

-- | The response status code.
dsrsStatus :: Lens' DescribeStepResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
