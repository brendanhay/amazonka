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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
--
--
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
    , dsrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input determines which step to describe.
--
--
--
-- /See:/ 'describeStep' smart constructor.
data DescribeStep = DescribeStep'
  { _dsClusterId :: !Text
  , _dsStepId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStep' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsClusterId' - The identifier of the cluster with steps to describe.
--
-- * 'dsStepId' - The identifier of the step to describe.
describeStep
    :: Text -- ^ 'dsClusterId'
    -> Text -- ^ 'dsStepId'
    -> DescribeStep
describeStep pClusterId_ pStepId_ =
  DescribeStep' {_dsClusterId = pClusterId_, _dsStepId = pStepId_}


-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\ s a -> s{_dsClusterId = a})

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\ s a -> s{_dsStepId = a})

instance AWSRequest DescribeStep where
        type Rs DescribeStep = DescribeStepResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStepResponse' <$>
                   (x .?> "Step") <*> (pure (fromEnum s)))

instance Hashable DescribeStep where

instance NFData DescribeStep where

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
--
--
-- /See:/ 'describeStepResponse' smart constructor.
data DescribeStepResponse = DescribeStepResponse'
  { _dsrsStep           :: !(Maybe Step)
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStepResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsStep' - The step details for the requested step identifier.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeStepResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeStepResponse
describeStepResponse pResponseStatus_ =
  DescribeStepResponse'
    {_dsrsStep = Nothing, _dsrsResponseStatus = pResponseStatus_}


-- | The step details for the requested step identifier.
dsrsStep :: Lens' DescribeStepResponse (Maybe Step)
dsrsStep = lens _dsrsStep (\ s a -> s{_dsrsStep = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeStepResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeStepResponse where
