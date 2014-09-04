{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.RunJobFlow
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | RunJobFlow creates and starts running a new job flow. The job flow will run
-- the steps specified. Once the job flow completes, the cluster is stopped
-- and the HDFS partition is lost. To prevent loss of data, configure the last
-- step of the job flow to store results in Amazon S3. If the
-- JobFlowInstancesConfig KeepJobFlowAliveWhenNoSteps parameter is set to
-- TRUE, the job flow will transition to the WAITING state rather than
-- shutting down once the steps have completed. For additional protection, you
-- can set the JobFlowInstancesConfig TerminationProtected parameter to TRUE
-- to lock the job flow and prevent it from being terminated by API call, user
-- intervention, or in the event of a job flow error. A maximum of 256 steps
-- are allowed in each job flow. If your job flow is long-running (such as a
-- Hive data warehouse) or complex, you may require more than 256 steps to
-- process your data. You can bypass the 256-step limitation in various ways,
-- including using the SSH shell to connect to the master node and submitting
-- queries directly to the software running on the master node, such as Hive
-- and Hadoop. For more information on how to do this, go to Add More than 256
-- Steps to a Job Flow in the Amazon Elastic MapReduce Developer's Guide. For
-- long running job flows, we recommend that you periodically store your
-- results. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.RunJobFlow Content-Length: 734 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T210803Z
-- X-Amz-Content-Sha256:
-- 8676d21986e4628a89fb1232a1344063778d4ffc23d10be02b437e0d53a24db3
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=71f79725c4dbe77c0e842718485f0b37fe6df69e1153c80f7748ebd9617ca2f3
-- Accept: */* { "Name": "Development Job Flow", "Instances": {
-- "KeepJobFlowAliveWhenNoSteps": "false", "TerminationProtected": "false",
-- "InstanceGroups": [{ "Name": "Master Instance Group", "InstanceRole":
-- "MASTER", "InstanceCount": 1, "InstanceType": "m1.small", "Market":
-- "ON_DEMAND" }] }, "Steps": [{ "Name": "Example Streaming Step",
-- "ActionOnFailure": "CANCEL_AND_WAIT", "HadoopJarStep": { "Jar":
-- "/home/hadoop/contrib/streaming/hadoop-streaming.jar", "Args": [ "-input",
-- "s3://elasticmapreduce/samples/wordcount/input", "-output",
-- "s3://examples-bucket/example-output", "-mapper",
-- "s3://elasticmapreduce/samples/wordcount/wordSplitter.py", "-reducer",
-- "aggregate" ] } }], "BootstrapActions": [], "VisibleToAllUsers": "false",
-- "NewSupportedProducts": [], "AmiVersion": "latest" } HTTP/1.1 200 OK
-- x-amzn-RequestId: a4406d6b-ed92-11e2-9787-192218ecb460 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 31 Date: Mon, 15 Jul 2013
-- 21:08:05 GMT {"JobFlowId": "j-ZKIY4CKQRX72"}.
module Network.AWS.EMR.V2009_03_31.RunJobFlow
    (
    -- * Request
      RunJobFlow
    -- ** Request constructor
    , mkRunJobFlowInput
    -- ** Request lenses
    , rjfiName
    , rjfiLogUri
    , rjfiAdditionalInfo
    , rjfiAmiVersion
    , rjfiInstances
    , rjfiSteps
    , rjfiBootstrapActions
    , rjfiSupportedProducts
    , rjfiNewSupportedProducts
    , rjfiVisibleToAllUsers
    , rjfiJobFlowRole
    , rjfiServiceRole
    , rjfiTags

    -- * Response
    , RunJobFlowResponse
    -- ** Response lenses
    , rjfoJobFlowId
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RunJobFlow' request.
mkRunJobFlowInput :: Text -- ^ 'rjfiName'
                  -> JobFlowInstancesConfig -- ^ 'rjfiInstances'
                  -> RunJobFlow
mkRunJobFlowInput p1 p2 = RunJobFlow
    { _rjfiName = p1
    , _rjfiLogUri = Nothing
    , _rjfiAdditionalInfo = Nothing
    , _rjfiAmiVersion = Nothing
    , _rjfiInstances = p5
    , _rjfiSteps = mempty
    , _rjfiBootstrapActions = mempty
    , _rjfiSupportedProducts = mempty
    , _rjfiNewSupportedProducts = mempty
    , _rjfiVisibleToAllUsers = Nothing
    , _rjfiJobFlowRole = Nothing
    , _rjfiServiceRole = Nothing
    , _rjfiTags = mempty
    }
{-# INLINE mkRunJobFlowInput #-}

data RunJobFlow = RunJobFlow
    { _rjfiName :: Text
      -- ^ The name of the job flow.
    , _rjfiLogUri :: Maybe Text
      -- ^ The location in Amazon S3 to write the log files of the job flow.
      -- If a value is not provided, logs are not created.
    , _rjfiAdditionalInfo :: Maybe Text
      -- ^ A JSON string for selecting additional features.
    , _rjfiAmiVersion :: Maybe Text
      -- ^ The version of the Amazon Machine Image (AMI) to use when
      -- launching Amazon EC2 instances in the job flow. The following
      -- values are valid: "latest" (uses the latest AMI) The version
      -- number of the AMI to use, for example, "2.0" If the AMI supports
      -- multiple versions of Hadoop (for example, AMI 1.0 supports both
      -- Hadoop 0.18 and 0.20) you can use the JobFlowInstancesConfig
      -- HadoopVersion parameter to modify the version of Hadoop from the
      -- defaults shown above. For details about the AMI versions
      -- currently supported by Amazon Elastic MapReduce, go to AMI
      -- Versions Supported in Elastic MapReduce in the Amazon Elastic
      -- MapReduce Developer's Guide.
    , _rjfiInstances :: JobFlowInstancesConfig
      -- ^ A specification of the number and type of Amazon EC2 instances on
      -- which to run the job flow.
    , _rjfiSteps :: [StepConfig]
      -- ^ A list of steps to be executed by the job flow.
    , _rjfiBootstrapActions :: [BootstrapActionConfig]
      -- ^ A list of bootstrap actions that will be run before Hadoop is
      -- started on the cluster nodes.
    , _rjfiSupportedProducts :: [Text]
      -- ^ A list of strings that indicates third-party software to use with
      -- the job flow. For more information, go to Use Third Party
      -- Applications with Amazon EMR. Currently supported values are:
      -- "mapr-m3" - launch the job flow using MapR M3 Edition. "mapr-m5"
      -- - launch the job flow using MapR M5 Edition.
    , _rjfiNewSupportedProducts :: [SupportedProductConfig]
      -- ^ A list of strings that indicates third-party software to use with
      -- the job flow that accepts a user argument list. EMR accepts and
      -- forwards the argument list to the corresponding installation
      -- script as bootstrap action arguments. For more information, see
      -- Launch a Job Flow on the MapR Distribution for Hadoop. Currently
      -- supported values are: "mapr-m3" - launch the job flow using MapR
      -- M3 Edition. "mapr-m5" - launch the job flow using MapR M5
      -- Edition. "mapr" with the user arguments specifying "--edition,m3"
      -- or "--edition,m5" - launch the job flow using MapR M3 or M5
      -- Edition respectively.
    , _rjfiVisibleToAllUsers :: Maybe Bool
      -- ^ Whether the job flow is visible to all IAM users of the AWS
      -- account associated with the job flow. If this value is set to
      -- true, all IAM users of that AWS account can view and (if they
      -- have the proper policy permissions set) manage the job flow. If
      -- it is set to false, only the IAM user that created the job flow
      -- can view and manage it.
    , _rjfiJobFlowRole :: Maybe Text
      -- ^ An IAM role for the job flow. The EC2 instances of the job flow
      -- assume this role. The default role is EMRJobflowDefault. In order
      -- to use the default role, you must have already created it using
      -- the CLI.
    , _rjfiServiceRole :: Maybe Text
      -- ^ The IAM role that will be assumed by the Amazon EMR service to
      -- access AWS resources on your behalf.
    , _rjfiTags :: [Tag]
      -- ^ A list of tags to associate with a cluster and propagate to
      -- Amazon EC2 instances.
    } deriving (Show, Generic)

-- | The name of the job flow.
rjfiName :: Lens' RunJobFlow (Text)
rjfiName = lens _rjfiName (\s a -> s { _rjfiName = a })
{-# INLINE rjfiName #-}

-- | The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
rjfiLogUri :: Lens' RunJobFlow (Maybe Text)
rjfiLogUri = lens _rjfiLogUri (\s a -> s { _rjfiLogUri = a })
{-# INLINE rjfiLogUri #-}

-- | A JSON string for selecting additional features.
rjfiAdditionalInfo :: Lens' RunJobFlow (Maybe Text)
rjfiAdditionalInfo = lens _rjfiAdditionalInfo (\s a -> s { _rjfiAdditionalInfo = a })
{-# INLINE rjfiAdditionalInfo #-}

-- | The version of the Amazon Machine Image (AMI) to use when launching Amazon
-- EC2 instances in the job flow. The following values are valid: "latest"
-- (uses the latest AMI) The version number of the AMI to use, for example,
-- "2.0" If the AMI supports multiple versions of Hadoop (for example, AMI 1.0
-- supports both Hadoop 0.18 and 0.20) you can use the JobFlowInstancesConfig
-- HadoopVersion parameter to modify the version of Hadoop from the defaults
-- shown above. For details about the AMI versions currently supported by
-- Amazon Elastic MapReduce, go to AMI Versions Supported in Elastic MapReduce
-- in the Amazon Elastic MapReduce Developer's Guide.
rjfiAmiVersion :: Lens' RunJobFlow (Maybe Text)
rjfiAmiVersion = lens _rjfiAmiVersion (\s a -> s { _rjfiAmiVersion = a })
{-# INLINE rjfiAmiVersion #-}

-- | A specification of the number and type of Amazon EC2 instances on which to
-- run the job flow.
rjfiInstances :: Lens' RunJobFlow (JobFlowInstancesConfig)
rjfiInstances = lens _rjfiInstances (\s a -> s { _rjfiInstances = a })
{-# INLINE rjfiInstances #-}

-- | A list of steps to be executed by the job flow.
rjfiSteps :: Lens' RunJobFlow ([StepConfig])
rjfiSteps = lens _rjfiSteps (\s a -> s { _rjfiSteps = a })
{-# INLINE rjfiSteps #-}

-- | A list of bootstrap actions that will be run before Hadoop is started on
-- the cluster nodes.
rjfiBootstrapActions :: Lens' RunJobFlow ([BootstrapActionConfig])
rjfiBootstrapActions = lens _rjfiBootstrapActions (\s a -> s { _rjfiBootstrapActions = a })
{-# INLINE rjfiBootstrapActions #-}

-- | A list of strings that indicates third-party software to use with the job
-- flow. For more information, go to Use Third Party Applications with Amazon
-- EMR. Currently supported values are: "mapr-m3" - launch the job flow using
-- MapR M3 Edition. "mapr-m5" - launch the job flow using MapR M5 Edition.
rjfiSupportedProducts :: Lens' RunJobFlow ([Text])
rjfiSupportedProducts = lens _rjfiSupportedProducts (\s a -> s { _rjfiSupportedProducts = a })
{-# INLINE rjfiSupportedProducts #-}

-- | A list of strings that indicates third-party software to use with the job
-- flow that accepts a user argument list. EMR accepts and forwards the
-- argument list to the corresponding installation script as bootstrap action
-- arguments. For more information, see Launch a Job Flow on the MapR
-- Distribution for Hadoop. Currently supported values are: "mapr-m3" - launch
-- the job flow using MapR M3 Edition. "mapr-m5" - launch the job flow using
-- MapR M5 Edition. "mapr" with the user arguments specifying "--edition,m3"
-- or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition
-- respectively.
rjfiNewSupportedProducts :: Lens' RunJobFlow ([SupportedProductConfig])
rjfiNewSupportedProducts = lens _rjfiNewSupportedProducts (\s a -> s { _rjfiNewSupportedProducts = a })
{-# INLINE rjfiNewSupportedProducts #-}

-- | Whether the job flow is visible to all IAM users of the AWS account
-- associated with the job flow. If this value is set to true, all IAM users
-- of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it.
rjfiVisibleToAllUsers :: Lens' RunJobFlow (Maybe Bool)
rjfiVisibleToAllUsers = lens _rjfiVisibleToAllUsers (\s a -> s { _rjfiVisibleToAllUsers = a })
{-# INLINE rjfiVisibleToAllUsers #-}

-- | An IAM role for the job flow. The EC2 instances of the job flow assume this
-- role. The default role is EMRJobflowDefault. In order to use the default
-- role, you must have already created it using the CLI.
rjfiJobFlowRole :: Lens' RunJobFlow (Maybe Text)
rjfiJobFlowRole = lens _rjfiJobFlowRole (\s a -> s { _rjfiJobFlowRole = a })
{-# INLINE rjfiJobFlowRole #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
rjfiServiceRole :: Lens' RunJobFlow (Maybe Text)
rjfiServiceRole = lens _rjfiServiceRole (\s a -> s { _rjfiServiceRole = a })
{-# INLINE rjfiServiceRole #-}

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
rjfiTags :: Lens' RunJobFlow ([Tag])
rjfiTags = lens _rjfiTags (\s a -> s { _rjfiTags = a })
{-# INLINE rjfiTags #-}

instance ToPath RunJobFlow

instance ToQuery RunJobFlow

instance ToHeaders RunJobFlow

instance ToJSON RunJobFlow

newtype RunJobFlowResponse = RunJobFlowResponse
    { _rjfoJobFlowId :: Maybe Text
      -- ^ An unique identifier for the job flow.
    } deriving (Show, Generic)

-- | An unique identifier for the job flow.
rjfoJobFlowId :: Lens' RunJobFlowResponse (Maybe Text)
rjfoJobFlowId = lens _rjfoJobFlowId (\s a -> s { _rjfoJobFlowId = a })
{-# INLINE rjfoJobFlowId #-}

instance FromJSON RunJobFlowResponse

instance AWSRequest RunJobFlow where
    type Sv RunJobFlow = EMR
    type Rs RunJobFlow = RunJobFlowResponse

    request = get
    response _ = jsonResponse
