{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.EMR" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.EMR
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.EMR.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.EMR.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using @return ()@:
-- operationName w x $ return ()
-- @
--
module Network.AWS.EMR.Monadic
    (
    -- * AddInstanceGroups
    -- $AddInstanceGroups
      addInstanceGroups
    , addInstanceGroupsCatch

    -- * AddJobFlowSteps
    -- $AddJobFlowSteps
    , addJobFlowSteps
    , addJobFlowStepsCatch

    -- * AddTags
    -- $AddTags
    , addTags
    , addTagsCatch

    -- * DescribeCluster
    -- $DescribeCluster
    , describeCluster
    , describeClusterCatch

    -- * DescribeJobFlows
    -- $DescribeJobFlows
    , describeJobFlows
    , describeJobFlowsCatch

    -- * DescribeStep
    -- $DescribeStep
    , describeStep
    , describeStepCatch

    -- * ListBootstrapActions
    -- $ListBootstrapActions
    , listBootstrapActions
    , listBootstrapActionsCatch

    -- * ListClusters
    -- $ListClusters
    , listClusters
    , listClustersCatch

    -- * ListInstanceGroups
    -- $ListInstanceGroups
    , listInstanceGroups
    , listInstanceGroupsCatch

    -- * ListInstances
    -- $ListInstances
    , listInstances
    , listInstancesCatch

    -- * ListSteps
    -- $ListSteps
    , listSteps
    , listStepsCatch

    -- * ModifyInstanceGroups
    -- $ModifyInstanceGroups
    , modifyInstanceGroups
    , modifyInstanceGroupsCatch

    -- * RemoveTags
    -- $RemoveTags
    , removeTags
    , removeTagsCatch

    -- * RunJobFlow
    -- $RunJobFlow
    , runJobFlow
    , runJobFlowCatch

    -- * SetTerminationProtection
    -- $SetTerminationProtection
    , setTerminationProtection
    , setTerminationProtectionCatch

    -- * SetVisibleToAllUsers
    -- $SetVisibleToAllUsers
    , setVisibleToAllUsers
    , setVisibleToAllUsersCatch

    -- * TerminateJobFlows
    -- $TerminateJobFlows
    , terminateJobFlows
    , terminateJobFlowsCatch

    -- * Re-exported
    , module Network.AWS.EMR

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.EMR


-- $AddInstanceGroups
-- AddInstanceGroups adds an instance group to a running cluster. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.AddInstanceGroups Content-Length: 168 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T223346Z
-- X-Amz-Content-Sha256:
-- ac5a7193b1283898dd822a4b16ca36963879bb010d2dbe57198439973ab2a7d3
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=4c5e7eb762ea45f292a5cd1a1cc56ed60009e19a9dba3d6e5e4e67e96d43af11
-- Accept: */* { "JobFlowId": "j-3U7TSX5GZFD8Y", "InstanceGroups": [{ "Name":
-- "Task Instance Group", "InstanceRole": "TASK", "InstanceCount": 2,
-- "InstanceType": "m1.small", "Market": "ON_DEMAND" }] } HTTP/1.1 200 OK
-- x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { "InstanceGroupIds": ["ig-294A6A2KWT4WB"], "JobFlowId":
-- "j-3U7TSX5GZFD8Y" }.
--
-- See: 'Network.AWS.EMR.AddInstanceGroups'

addInstanceGroups :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => [InstanceGroupConfig] -- ^ 'aigInstanceGroups'
    -> Text -- ^ 'aigJobFlowId'
    -> m AddInstanceGroupsResponse
addInstanceGroups p1 p2 =
    send (mkAddInstanceGroups p1 p2)

addInstanceGroupsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => [InstanceGroupConfig] -- ^ 'aigInstanceGroups'
    -> Text -- ^ 'aigJobFlowId'
    -> m (Either EMRError AddInstanceGroupsResponse)
addInstanceGroupsCatch p1 p2 =
    sendCatch (mkAddInstanceGroups p1 p2)

-- $AddJobFlowSteps
-- AddJobFlowSteps adds new steps to a running job flow. A maximum of 256
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
--
-- See: 'Network.AWS.EMR.AddJobFlowSteps'

addJobFlowSteps :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'ajfsJobFlowId'
    -> [StepConfig] -- ^ 'ajfsSteps'
    -> m AddJobFlowStepsResponse
addJobFlowSteps p1 p2 =
    send (mkAddJobFlowSteps p1 p2)

addJobFlowStepsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'ajfsJobFlowId'
    -> [StepConfig] -- ^ 'ajfsSteps'
    -> m (Either EMRError AddJobFlowStepsResponse)
addJobFlowStepsCatch p1 p2 =
    sendCatch (mkAddJobFlowSteps p1 p2)

-- $AddTags
-- Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see Tagging Amazon EMR
-- Resources. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.AddTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack", "Value": "Production" }, {
-- "Key": "hbase" }] } HTTP/1.1 200 OK x-amzn-RequestId:
-- 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { }.
--
-- See: 'Network.AWS.EMR.AddTags'

addTags :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           )
    => Text -- ^ 'atResourceId'
    -> [Tag] -- ^ 'atTags'
    -> m AddTagsResponse
addTags p1 p2 =
    send (mkAddTags p1 p2)

addTagsCatch :: ( MonadCatch m
                , MonadResource m
                , MonadReader Env m
                )
    => Text -- ^ 'atResourceId'
    -> [Tag] -- ^ 'atTags'
    -> m (Either EMRError AddTagsResponse)
addTagsCatch p1 p2 =
    sendCatch (mkAddTags p1 p2)

-- $DescribeCluster
-- Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the cluster
-- steps, see ListSteps.
--
-- See: 'Network.AWS.EMR.DescribeCluster'

describeCluster :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'dcClusterId'
    -> m DescribeClusterResponse
describeCluster p1 =
    send (mkDescribeCluster p1)

describeClusterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dcClusterId'
    -> m (Either EMRError DescribeClusterResponse)
describeClusterCatch p1 =
    sendCatch (mkDescribeCluster p1)

-- $DescribeJobFlows
-- This API is deprecated and will eventually be removed. We recommend you use
-- ListClusters, DescribeCluster, ListSteps, ListInstanceGroups and
-- ListBootstrapActions instead. DescribeJobFlows returns a list of job flows
-- that match all of the supplied parameters. The parameters can include a
-- list of job flow IDs, job flow states, and restrictions on job flow
-- creation date and time. Regardless of supplied parameters, only job flows
-- created within the last two months are returned. If no parameters are
-- supplied, then job flows matching either of the following criteria are
-- returned: Job flows created and completed in the last two weeks Job flows
-- created within the last two months that are in one of the following states:
-- RUNNING, WAITING, SHUTTING_DOWN, STARTING Amazon Elastic MapReduce can
-- return a maximum of 512 job flow descriptions. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.DescribeJobFlows Content-Length: 62 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T220330Z
-- X-Amz-Content-Sha256:
-- fce83af973f96f173512aca2845c56862b946feb1de0600326f1365b658a0e39
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=29F98a6f44e05ad54fe1e8b3d1a7101ab08dc3ad348995f89c533693cee2bb3b
-- Accept: */* { "JobFlowIds": ["j-ZKIY4CKQRX72"], "DescriptionType":
-- "EXTENDED" } HTTP/1.1 200 OK x-amzn-RequestId:
-- 634d4142-ed9a-11e2-bbba-b56d7d016ec4 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 1624 Date: Mon, 15 Jul 2013
-- 22:03:31 GMT {"JobFlows": [{ "AmiVersion": "2.3.6", "BootstrapActions": [],
-- "ExecutionStatusDetail": { "CreationDateTime": 1.373923429E9,
-- "EndDateTime": 1.373923995E9, "LastStateChangeReason": "Steps completed",
-- "ReadyDateTime": 1.373923754E9, "StartDateTime": 1.373923754E9, "State":
-- "COMPLETED" }, "Instances": { "HadoopVersion": "1.0.3", "InstanceCount": 1,
-- "InstanceGroups": [{ "CreationDateTime": 1.373923429E9, "EndDateTime":
-- 1.373923995E9, "InstanceGroupId": "ig-3SRUWV3E0NB7K",
-- "InstanceRequestCount": 1, "InstanceRole": "MASTER",
-- "InstanceRunningCount": 0, "InstanceType": "m1.small",
-- "LastStateChangeReason": "Job flow terminated", "Market": "ON_DEMAND",
-- "Name": "Master InstanceGroup", "ReadyDateTime": 1.37392375E9,
-- "StartDateTime": 1.373923646E9, "State": "ENDED" }],
-- "KeepJobFlowAliveWhenNoSteps": false, "MasterInstanceId": "i-8c4fbbef",
-- "MasterInstanceType": "m1.small", "MasterPublicDnsName":
-- "ec2-107-20-46-140.compute-1.amazonaws.com", "NormalizedInstanceHours": 1,
-- "Placement": {"AvailabilityZone": "us-east-1a"}, "TerminationProtected":
-- false }, "JobFlowId": "j-ZKIY4CKQRX72", "Name": "Development Job Flow",
-- "Steps": [{ "ExecutionStatusDetail": { "CreationDateTime": 1.373923429E9,
-- "EndDateTime": 1.373923914E9, "StartDateTime": 1.373923754E9, "State":
-- "COMPLETED" }, "StepConfig": { "ActionOnFailure": "CANCEL_AND_WAIT",
-- "HadoopJarStep": { "Args": [ "-input",
-- "s3://elasticmapreduce/samples/wordcount/input", "-output",
-- "s3://examples-bucket/example-output", "-mapper",
-- "s3://elasticmapreduce/samples/wordcount/wordSplitter.py", "-reducer",
-- "aggregate" ], "Jar":
-- "/home/hadoop/contrib/streaming/hadoop-streaming.jar", "Properties": [] },
-- "Name": "Example Streaming Step" } }], "SupportedProducts": [],
-- "VisibleToAllUsers": false }]}.
--
-- See: 'Network.AWS.EMR.DescribeJobFlows'

describeJobFlows :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => State DescribeJobFlows a
    -> m DescribeJobFlowsResponse
describeJobFlows s =
    send (mkDescribeJobFlows &~ s)

describeJobFlowsCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => State DescribeJobFlows a
    -> m (Either EMRError DescribeJobFlowsResponse)
describeJobFlowsCatch s =
    sendCatch (mkDescribeJobFlows &~ s)

-- $DescribeStep
-- Provides more detail about the cluster step.
--
-- See: 'Network.AWS.EMR.DescribeStep'

describeStep :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'dsClusterId'
    -> Text -- ^ 'dsStepId'
    -> m DescribeStepResponse
describeStep p1 p2 =
    send (mkDescribeStep p1 p2)

describeStepCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dsClusterId'
    -> Text -- ^ 'dsStepId'
    -> m (Either EMRError DescribeStepResponse)
describeStepCatch p1 p2 =
    sendCatch (mkDescribeStep p1 p2)

-- $ListBootstrapActions
-- Provides information about the bootstrap actions associated with a cluster.
--
-- See: 'Network.AWS.EMR.ListBootstrapActions'

listBootstrapActions :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'lbaClusterId'
    -> State ListBootstrapActions a
    -> Source m ListBootstrapActionsResponse
listBootstrapActions p1 s =
    paginate $ (mkListBootstrapActions p1) &~ s

listBootstrapActionsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'lbaClusterId'
    -> State ListBootstrapActions a
    -> Source m (Either EMRError ListBootstrapActionsResponse)
listBootstrapActionsCatch p1 s =
    paginateCatch $ (mkListBootstrapActions p1) &~ s

-- $ListClusters
-- Provides the status of all clusters visible to this AWS account. Allows you
-- to filter the list of clusters based on certain criteria; for example,
-- filtering by cluster creation date and time or by status. This call returns
-- a maximum of 50 clusters per call, but returns a marker to track the paging
-- of the cluster list across multiple ListClusters calls.
--
-- See: 'Network.AWS.EMR.ListClusters'

listClusters :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => State ListClusters a
    -> Source m ListClustersResponse
listClusters s =
    paginate (mkListClusters &~ s)

listClustersCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => State ListClusters a
    -> Source m (Either EMRError ListClustersResponse)
listClustersCatch s =
    paginateCatch (mkListClusters &~ s)

-- $ListInstanceGroups
-- Provides all available details about the instance groups in a cluster.
--
-- See: 'Network.AWS.EMR.ListInstanceGroups'

listInstanceGroups :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ligClusterId'
    -> State ListInstanceGroups a
    -> Source m ListInstanceGroupsResponse
listInstanceGroups p1 s =
    paginate $ (mkListInstanceGroups p1) &~ s

listInstanceGroupsCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ligClusterId'
    -> State ListInstanceGroups a
    -> Source m (Either EMRError ListInstanceGroupsResponse)
listInstanceGroupsCatch p1 s =
    paginateCatch $ (mkListInstanceGroups p1) &~ s

-- $ListInstances
-- Provides information about the cluster instances that Amazon EMR provisions
-- on behalf of a user when it creates the cluster. For example, this
-- operation indicates when the EC2 instances reach the Ready state, when
-- instances become available to Amazon EMR to use for jobs, and the IP
-- addresses for cluster instances, etc.
--
-- See: 'Network.AWS.EMR.ListInstances'

listInstances :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'liClusterId'
    -> State ListInstances a
    -> Source m ListInstancesResponse
listInstances p1 s =
    paginate $ (mkListInstances p1) &~ s

listInstancesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'liClusterId'
    -> State ListInstances a
    -> Source m (Either EMRError ListInstancesResponse)
listInstancesCatch p1 s =
    paginateCatch $ (mkListInstances p1) &~ s

-- $ListSteps
-- Provides a list of steps for the cluster.
--
-- See: 'Network.AWS.EMR.ListSteps'

listSteps :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'lsClusterId'
    -> State ListSteps a
    -> Source m ListStepsResponse
listSteps p1 s =
    paginate $ (mkListSteps p1) &~ s

listStepsCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'lsClusterId'
    -> State ListSteps a
    -> Source m (Either EMRError ListStepsResponse)
listStepsCatch p1 s =
    paginateCatch $ (mkListSteps p1) &~ s

-- $ModifyInstanceGroups
-- ModifyInstanceGroups modifies the number of nodes and configuration
-- settings of an instance group. The input parameters include the new target
-- instance count for the group and the instance group ID. The call will
-- either succeed or fail atomically. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.ModifyInstanceGroups Content-Length: 77 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T205843Z
-- X-Amz-Content-Sha256:
-- bb1af3d0c6c6a1a09f21ccd7f04a0e2e6c9ce5b5810b0f6777560fe4f81bda8c
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=17bbbb4448a1f47a14d5657445e9de5cadf16bed58b850585f80865882133b33
-- Accept: */* {"InstanceGroups": [{ "InstanceGroupId": "ig-1S8NWT31S2OVG",
-- "InstanceCount": 5 }]} HTTP/1.1 200 OK x-amzn-RequestId:
-- 80a74808-ee5a-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 20:58:44 GMT.
--
-- See: 'Network.AWS.EMR.ModifyInstanceGroups'

modifyInstanceGroups :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State ModifyInstanceGroups a
    -> m ModifyInstanceGroupsResponse
modifyInstanceGroups s =
    send (mkModifyInstanceGroups &~ s)

modifyInstanceGroupsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State ModifyInstanceGroups a
    -> m (Either EMRError ModifyInstanceGroupsResponse)
modifyInstanceGroupsCatch s =
    sendCatch (mkModifyInstanceGroups &~ s)

-- $RemoveTags
-- Removes tags from an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see Tagging Amazon EMR
-- Resources. The following example removes the stack tag with value Prod from
-- a cluster: POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.RemoveTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack", "Value": "Prod" }] } HTTP/1.1
-- 200 OK x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { } The following example removes the stack and hbase tags
-- from a cluster: POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: ElasticMapReduce.RemoveTags AUTHPARAMS { "ResourceId":
-- "j-3U7TSX5GZFD8Y", "Tags": [{ "Key": "stack" }, { "Key": "hbase" }] }
-- HTTP/1.1 200 OK x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d
-- Content-Type: application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15
-- Jul 2013 22:33:47 GMT { }.
--
-- See: 'Network.AWS.EMR.RemoveTags'

removeTags :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => Text -- ^ 'rtResourceId'
    -> [Text] -- ^ 'rtTagKeys'
    -> m RemoveTagsResponse
removeTags p1 p2 =
    send (mkRemoveTags p1 p2)

removeTagsCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => Text -- ^ 'rtResourceId'
    -> [Text] -- ^ 'rtTagKeys'
    -> m (Either EMRError RemoveTagsResponse)
removeTagsCatch p1 p2 =
    sendCatch (mkRemoveTags p1 p2)

-- $RunJobFlow
-- RunJobFlow creates and starts running a new job flow. The job flow will run
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
--
-- See: 'Network.AWS.EMR.RunJobFlow'

runJobFlow :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => Text -- ^ 'rjfName'
    -> JobFlowInstancesConfig -- ^ 'rjfInstances'
    -> State RunJobFlow a
    -> m RunJobFlowResponse
runJobFlow p1 p5 s =
    send $ (mkRunJobFlow p1 p5) &~ s

runJobFlowCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => Text -- ^ 'rjfName'
    -> JobFlowInstancesConfig -- ^ 'rjfInstances'
    -> State RunJobFlow a
    -> m (Either EMRError RunJobFlowResponse)
runJobFlowCatch p1 p5 s =
    sendCatch $ (mkRunJobFlow p1 p5) &~ s

-- $SetTerminationProtection
-- SetTerminationProtection locks a job flow so the Amazon EC2 instances in
-- the cluster cannot be terminated by user intervention, an API call, or in
-- the event of a job-flow error. The cluster still terminates upon successful
-- completion of the job flow. Calling SetTerminationProtection on a job flow
-- is analogous to calling the Amazon EC2 DisableAPITermination API on all of
-- the EC2 instances in a cluster. SetTerminationProtection is used to prevent
-- accidental termination of a job flow and to ensure that in the event of an
-- error, the instances will persist so you can recover any data stored in
-- their ephemeral instance storage. To terminate a job flow that has been
-- locked by setting SetTerminationProtection to true, you must first unlock
-- the job flow by a subsequent call to SetTerminationProtection in which you
-- set the value to false. For more information, go to Protecting a Job Flow
-- from Termination in the Amazon Elastic MapReduce Developer's Guide. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.SetTerminationProtection Content-Length: 61 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T211420Z
-- X-Amz-Content-Sha256:
-- c362fadae0fce377aa63f04388aeb90c53cedb17a8bfbb8cffcb10c2378137f9
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=764b6aa1a38733cadff35a2e884887e9f1208a422266bc83ac77e8d0b80bd4cf
-- Accept: */* { "JobFlowIds": ["j-3TS0OIYO4NFN"], "TerminationProtected":
-- true } HTTP/1.1 200 OK x-amzn-RequestId:
-- af23b1db-ee5c-11e2-9787-192218ecb460 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 21:14:21 GMT.
--
-- See: 'Network.AWS.EMR.SetTerminationProtection'

setTerminationProtection :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => [Text] -- ^ 'stpJobFlowIds'
    -> Bool -- ^ 'stpTerminationProtected'
    -> m SetTerminationProtectionResponse
setTerminationProtection p1 p2 =
    send (mkSetTerminationProtection p1 p2)

setTerminationProtectionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => [Text] -- ^ 'stpJobFlowIds'
    -> Bool -- ^ 'stpTerminationProtected'
    -> m (Either EMRError SetTerminationProtectionResponse)
setTerminationProtectionCatch p1 p2 =
    sendCatch (mkSetTerminationProtection p1 p2)

-- $SetVisibleToAllUsers
-- Sets whether all AWS Identity and Access Management (IAM) users under your
-- account can access the specified job flows. This action works on running
-- job flows. You can also set the visibility of a job flow when you launch it
-- using the VisibleToAllUsers parameter of RunJobFlow. The
-- SetVisibleToAllUsers action can be called only by an IAM user who created
-- the job flow or the AWS account that owns the job flow. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.SetVisibleToAllUsers Content-Length: 58 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T221616Z
-- X-Amz-Content-Sha256:
-- 2ff32d11eab2383d764ffcb97571454e798689ecd09a7b1bb2327e22b0b930d4
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=e1a00b37787d9ccc43c9de32f1f0a73813b0bd6643d4db7762b62a7092d51997
-- Accept: */* { "JobFlowIds": ["j-ZKIY4CKQRX72"], "VisibleToAllUsers": true }
-- HTTP/1.1 200 OK x-amzn-RequestId: 2be9cde9-ed9c-11e2-82b6-2351cde3f33f
-- Content-Type: application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 15
-- Jul 2013 22:16:18 GMT.
--
-- See: 'Network.AWS.EMR.SetVisibleToAllUsers'

setVisibleToAllUsers :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => [Text] -- ^ 'svtauJobFlowIds'
    -> Bool -- ^ 'svtauVisibleToAllUsers'
    -> m SetVisibleToAllUsersResponse
setVisibleToAllUsers p1 p2 =
    send (mkSetVisibleToAllUsers p1 p2)

setVisibleToAllUsersCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => [Text] -- ^ 'svtauJobFlowIds'
    -> Bool -- ^ 'svtauVisibleToAllUsers'
    -> m (Either EMRError SetVisibleToAllUsersResponse)
setVisibleToAllUsersCatch p1 p2 =
    sendCatch (mkSetVisibleToAllUsers p1 p2)

-- $TerminateJobFlows
-- TerminateJobFlows shuts a list of job flows down. When a job flow is shut
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
--
-- See: 'Network.AWS.EMR.TerminateJobFlows'

terminateJobFlows :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => [Text] -- ^ 'tjfJobFlowIds'
    -> m TerminateJobFlowsResponse
terminateJobFlows p1 =
    send (mkTerminateJobFlows p1)

terminateJobFlowsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => [Text] -- ^ 'tjfJobFlowIds'
    -> m (Either EMRError TerminateJobFlowsResponse)
terminateJobFlowsCatch p1 =
    sendCatch (mkTerminateJobFlows p1)
