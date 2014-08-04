{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.V2009_03_31.AddJobFlowSteps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AddJobFlowSteps adds new steps to a running job flow. A maximum of 256
-- steps are allowed in each job flow. If your job flow is long-running (such
-- as a Hive data warehouse) or complex, you may require more than 256 steps
-- to process your data. You can bypass the 256-step limitation in various
-- ways, including using the SSH shell to connect to the master node and
-- submitting queries directly to the software running on the master node,
-- such as Hive and Hadoop. For more information on how to do this, go to Add
-- More than 256 Steps to a Job Flow in the Amazon Elastic MapReduce
-- Developer's Guide. A step specifies the location of a JAR file stored
-- either on the master node of the job flow or in Amazon S3. Each step is
-- performed by the main function of the main class of the JAR file. The main
-- class can be specified either in the manifest of the JAR or by using the
-- MainFunction parameter of the step. Elastic MapReduce executes each step in
-- the order listed. For a step to be considered complete, the main function
-- must exit with a zero exit code and all Hadoop jobs started while the step
-- was running must have completed and run successfully. You can only add
-- steps to a job flow that is in one of the following states: STARTING,
-- BOOTSTRAPPING, RUNNING, or WAITING. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: ElasticMapReduce.AddJobFlowSteps
-- Content-Length: 426 User-Agent: aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32
-- Host: us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T210948Z
-- X-Amz-Content-Sha256:
-- 9e5ad0a93c22224947ce98eea94f766103d91b28fa82eb60d0cb8b6f9555a6b2
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=2a2393390760ae85eb74ee3a539e1d758bfdd8815a1a6d6f14d4a2fbcfdcd5b7
-- Accept: */* { "JobFlowId": "j-3TS0OIYO4NFN", "Steps": [{ "Name": "Example
-- Jar Step", "ActionOnFailure": "CANCEL_AND_WAIT", "HadoopJarStep": { "Jar":
-- "s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/cloudburst.jar",
-- "Args": [
-- "s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/s_suis.br",
-- "s3n:\\/\\/elasticmapreduce\\/samples\\/cloudburst\\/input\\/100k.br",
-- "s3n:\\/\\/examples-bucket\\/cloudburst\\/output", "36", "3", "0", "1",
-- "240", "48", "24", "24", "128", "16" ] } }] } HTTP/1.1 200 OK
-- x-amzn-RequestId: 6514261f-ee5b-11e2-9345-5332e9ab2e6d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 21:05:07 GMT.
module Network.AWS.EMR.V2009_03_31.AddJobFlowSteps where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude

data AddJobFlowSteps = AddJobFlowSteps
    { _ajfsiSteps :: [StepConfig]
      -- ^ A list of StepConfig to be executed by the job flow.
    , _ajfsiJobFlowId :: Text
      -- ^ A string that uniquely identifies the job flow. This identifier
      -- is returned by RunJobFlow and can also be obtained from
      -- ListClusters.
    } deriving (Generic)

makeLenses ''AddJobFlowSteps

instance ToPath AddJobFlowSteps

instance ToQuery AddJobFlowSteps

instance ToHeaders AddJobFlowSteps

instance ToJSON AddJobFlowSteps

data AddJobFlowStepsResponse = AddJobFlowStepsResponse
    { _ajfsoStepIds :: [Text]
      -- ^ The identifiers of the list of steps added to the job flow.
    } deriving (Generic)

makeLenses ''AddJobFlowStepsResponse

instance FromJSON AddJobFlowStepsResponse

instance AWSRequest AddJobFlowSteps where
    type Sv AddJobFlowSteps = EMR
    type Rs AddJobFlowSteps = AddJobFlowStepsResponse

    request = get
    response _ = jsonResponse
