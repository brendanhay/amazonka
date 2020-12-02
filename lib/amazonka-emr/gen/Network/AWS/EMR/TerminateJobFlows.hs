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
-- Module      : Network.AWS.EMR.TerminateJobFlows
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- TerminateJobFlows shuts a list of clusters (job flows) down. When a job flow is shut down, any step not yet completed is canceled and the EC2 instances on which the cluster is running are stopped. Any log files not already saved are uploaded to Amazon S3 if a LogUri was specified when the cluster was created.
--
--
-- The maximum number of clusters allowed is 10. The call to @TerminateJobFlows@ is asynchronous. Depending on the configuration of the cluster, it may take up to 1-5 minutes for the cluster to completely terminate and release allocated resources, such as Amazon EC2 instances.
--
module Network.AWS.EMR.TerminateJobFlows
    (
    -- * Creating a Request
      terminateJobFlows
    , TerminateJobFlows
    -- * Request Lenses
    , tjfJobFlowIds

    -- * Destructuring the Response
    , terminateJobFlowsResponse
    , TerminateJobFlowsResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the 'TerminateJobFlows' operation.
--
--
--
-- /See:/ 'terminateJobFlows' smart constructor.
newtype TerminateJobFlows = TerminateJobFlows'
  { _tjfJobFlowIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateJobFlows' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjfJobFlowIds' - A list of job flows to be shutdown.
terminateJobFlows
    :: TerminateJobFlows
terminateJobFlows = TerminateJobFlows' {_tjfJobFlowIds = mempty}


-- | A list of job flows to be shutdown.
tjfJobFlowIds :: Lens' TerminateJobFlows [Text]
tjfJobFlowIds = lens _tjfJobFlowIds (\ s a -> s{_tjfJobFlowIds = a}) . _Coerce

instance AWSRequest TerminateJobFlows where
        type Rs TerminateJobFlows = TerminateJobFlowsResponse
        request = postJSON emr
        response = receiveNull TerminateJobFlowsResponse'

instance Hashable TerminateJobFlows where

instance NFData TerminateJobFlows where

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
          = object
              (catMaybes [Just ("JobFlowIds" .= _tjfJobFlowIds)])

instance ToPath TerminateJobFlows where
        toPath = const "/"

instance ToQuery TerminateJobFlows where
        toQuery = const mempty

-- | /See:/ 'terminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse =
  TerminateJobFlowsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateJobFlowsResponse' with the minimum fields required to make a request.
--
terminateJobFlowsResponse
    :: TerminateJobFlowsResponse
terminateJobFlowsResponse = TerminateJobFlowsResponse'


instance NFData TerminateJobFlowsResponse where
