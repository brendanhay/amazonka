{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EMR.TerminateJobFlows
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | TerminateJobFlows shuts a list of job flows down. When a job flow is
-- shut down, any step not yet completed is canceled and the EC2 instances
-- on which the job flow is running are stopped. Any log files not already
-- saved are uploaded to Amazon S3 if a LogUri was specified when the job
-- flow was created.
--
-- The maximum number of JobFlows allowed is 10. The call to
-- TerminateJobFlows is asynchronous. Depending on the configuration of the
-- job flow, it may take up to 5-20 minutes for the job flow to completely
-- terminate and release allocated resources, such as Amazon EC2 instances.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_TerminateJobFlows.html>
module Network.AWS.EMR.TerminateJobFlows
    (
    -- * Request
      TerminateJobFlows
    -- ** Request constructor
    , terminateJobFlows
    -- ** Request lenses
    , tjfJobFlowIds

    -- * Response
    , TerminateJobFlowsResponse
    -- ** Response constructor
    , terminateJobFlowsResponse
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the TerminateJobFlows operation.
--
-- /See:/ 'terminateJobFlows' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tjfJobFlowIds'
newtype TerminateJobFlows = TerminateJobFlows'
    { _tjfJobFlowIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateJobFlows' smart constructor.
terminateJobFlows :: TerminateJobFlows
terminateJobFlows =
    TerminateJobFlows'
    { _tjfJobFlowIds = mempty
    }

-- | A list of job flows to be shutdown.
tjfJobFlowIds :: Lens' TerminateJobFlows [Text]
tjfJobFlowIds = lens _tjfJobFlowIds (\ s a -> s{_tjfJobFlowIds = a});

instance AWSRequest TerminateJobFlows where
        type Sv TerminateJobFlows = EMR
        type Rs TerminateJobFlows = TerminateJobFlowsResponse
        request = postJSON
        response = receiveNull TerminateJobFlowsResponse'

instance ToHeaders TerminateJobFlows where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.TerminateJobFlows" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TerminateJobFlows where
        toJSON TerminateJobFlows'{..}
          = object ["JobFlowIds" .= _tjfJobFlowIds]

instance ToPath TerminateJobFlows where
        toPath = const "/"

instance ToQuery TerminateJobFlows where
        toQuery = const mempty

-- | /See:/ 'terminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse =
    TerminateJobFlowsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateJobFlowsResponse' smart constructor.
terminateJobFlowsResponse :: TerminateJobFlowsResponse
terminateJobFlowsResponse = TerminateJobFlowsResponse'
