{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | TerminateJobFlows shuts a list of job flows down. When a job flow is shut
-- down, any step not yet completed is canceled and the EC2 instances on which
-- the job flow is running are stopped. Any log files not already saved are
-- uploaded to Amazon S3 if a LogUri was specified when the job flow was
-- created. The call to TerminateJobFlows is asynchronous. Depending on the
-- configuration of the job flow, it may take up to 5-20 minutes for the job
-- flow to completely terminate and release allocated resources, such as
-- Amazon EC2 instances. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: ElasticMapReduce.TerminateJobFlows
-- Content-Length: 33 User-Agent: aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32
-- Host: us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T211858Z
-- X-Amz-Content-Sha256:
-- ab64713f61e066e80a6083844b9249b6c6362d34a7ae7393047aa46d38b9e315
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=9791416eaf09f36aa753a324b0de27ff5cc7084b8548cc748487a2bcb3439d58
-- Accept: */* {"JobFlowIds": ["j-3TS0OIYO4NFN"]} HTTP/1.1 200 OK
-- x-amzn-RequestId: 5551a7c9-ee5d-11e2-9542-25296c300ff0 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 21:18:59 GMT.
module Network.AWS.EMR
    (
    -- * Request
      TerminateJobFlows
    -- ** Request constructor
    , mkTerminateJobFlows
    -- ** Request lenses
    , tjfJobFlowIds

    -- * Response
    , TerminateJobFlowsResponse
    -- ** Response constructor
    , mkTerminateJobFlowsResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the TerminateJobFlows operation.
newtype TerminateJobFlows = TerminateJobFlows
    { _tjfJobFlowIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateJobFlows' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobFlowIds ::@ @[Text]@
--
mkTerminateJobFlows :: [Text] -- ^ 'tjfJobFlowIds'
                    -> TerminateJobFlows
mkTerminateJobFlows p1 = TerminateJobFlows
    { _tjfJobFlowIds = p1
    }

-- | A list of job flows to be shutdown.
tjfJobFlowIds :: Lens' TerminateJobFlows [Text]
tjfJobFlowIds = lens _tjfJobFlowIds (\s a -> s { _tjfJobFlowIds = a })

instance ToPath TerminateJobFlows

instance ToQuery TerminateJobFlows

instance ToHeaders TerminateJobFlows

instance ToJSON TerminateJobFlows

data TerminateJobFlowsResponse = TerminateJobFlowsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateJobFlowsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkTerminateJobFlowsResponse :: TerminateJobFlowsResponse
mkTerminateJobFlowsResponse = TerminateJobFlowsResponse

instance AWSRequest TerminateJobFlows where
    type Sv TerminateJobFlows = EMR
    type Rs TerminateJobFlows = TerminateJobFlowsResponse

    request = get
    response _ = nullaryResponse TerminateJobFlowsResponse
