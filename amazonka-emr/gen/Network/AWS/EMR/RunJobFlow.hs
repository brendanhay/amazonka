{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EMR.RunJobFlow
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
-- results.
module Network.AWS.EMR.RunJobFlow
    (
    -- * Request
      RunJobFlow
    -- ** Request constructor
    , runJobFlow
    -- ** Request lenses
    , rjfAdditionalInfo
    , rjfAmiVersion
    , rjfBootstrapActions
    , rjfInstances
    , rjfJobFlowRole
    , rjfLogUri
    , rjfName
    , rjfNewSupportedProducts
    , rjfServiceRole
    , rjfSteps
    , rjfSupportedProducts
    , rjfTags
    , rjfVisibleToAllUsers

    -- * Response
    , RunJobFlowResponse
    -- ** Response constructor
    , runJobFlowResponse
    -- ** Response lenses
    , rjfrJobFlowId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types

data RunJobFlow = RunJobFlow
    { _rjfAdditionalInfo       :: Maybe Text
    , _rjfAmiVersion           :: Maybe Text
    , _rjfBootstrapActions     :: [BootstrapActionConfig]
    , _rjfInstances            :: JobFlowInstancesConfig
    , _rjfJobFlowRole          :: Maybe Text
    , _rjfLogUri               :: Maybe Text
    , _rjfName                 :: Text
    , _rjfNewSupportedProducts :: [SupportedProductConfig]
    , _rjfServiceRole          :: Maybe Text
    , _rjfSteps                :: [StepConfig]
    , _rjfSupportedProducts    :: [Text]
    , _rjfTags                 :: [Tag]
    , _rjfVisibleToAllUsers    :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'RunJobFlow' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjfAdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'rjfAmiVersion' @::@ 'Maybe' 'Text'
--
-- * 'rjfBootstrapActions' @::@ ['BootstrapActionConfig']
--
-- * 'rjfInstances' @::@ 'JobFlowInstancesConfig'
--
-- * 'rjfJobFlowRole' @::@ 'Maybe' 'Text'
--
-- * 'rjfLogUri' @::@ 'Maybe' 'Text'
--
-- * 'rjfName' @::@ 'Text'
--
-- * 'rjfNewSupportedProducts' @::@ ['SupportedProductConfig']
--
-- * 'rjfServiceRole' @::@ 'Maybe' 'Text'
--
-- * 'rjfSteps' @::@ ['StepConfig']
--
-- * 'rjfSupportedProducts' @::@ ['Text']
--
-- * 'rjfTags' @::@ ['Tag']
--
-- * 'rjfVisibleToAllUsers' @::@ 'Maybe' 'Bool'
--
runJobFlow :: Text -- ^ 'rjfName'
           -> JobFlowInstancesConfig -- ^ 'rjfInstances'
           -> RunJobFlow
runJobFlow p1 p2 = RunJobFlow
    { _rjfName                 = p1
    , _rjfInstances            = p2
    , _rjfLogUri               = Nothing
    , _rjfAdditionalInfo       = Nothing
    , _rjfAmiVersion           = Nothing
    , _rjfSteps                = mempty
    , _rjfBootstrapActions     = mempty
    , _rjfSupportedProducts    = mempty
    , _rjfNewSupportedProducts = mempty
    , _rjfVisibleToAllUsers    = Nothing
    , _rjfJobFlowRole          = Nothing
    , _rjfServiceRole          = Nothing
    , _rjfTags                 = mempty
    }

-- | A JSON string for selecting additional features.
rjfAdditionalInfo :: Lens' RunJobFlow (Maybe Text)
rjfAdditionalInfo =
    lens _rjfAdditionalInfo (\s a -> s { _rjfAdditionalInfo = a })

-- | The version of the Amazon Machine Image (AMI) to use when launching
-- Amazon EC2 instances in the job flow. The following values are valid:
-- "latest" (uses the latest AMI) The version number of the AMI to use, for
-- example, "2.0" If the AMI supports multiple versions of Hadoop (for
-- example, AMI 1.0 supports both Hadoop 0.18 and 0.20) you can use the
-- JobFlowInstancesConfig HadoopVersion parameter to modify the version of
-- Hadoop from the defaults shown above. For details about the AMI versions
-- currently supported by Amazon Elastic MapReduce, go to AMI Versions
-- Supported in Elastic MapReduce in the Amazon Elastic MapReduce
-- Developer's Guide.
rjfAmiVersion :: Lens' RunJobFlow (Maybe Text)
rjfAmiVersion = lens _rjfAmiVersion (\s a -> s { _rjfAmiVersion = a })

-- | A list of bootstrap actions that will be run before Hadoop is started on
-- the cluster nodes.
rjfBootstrapActions :: Lens' RunJobFlow [BootstrapActionConfig]
rjfBootstrapActions =
    lens _rjfBootstrapActions (\s a -> s { _rjfBootstrapActions = a })

-- | A specification of the number and type of Amazon EC2 instances on which
-- to run the job flow.
rjfInstances :: Lens' RunJobFlow JobFlowInstancesConfig
rjfInstances = lens _rjfInstances (\s a -> s { _rjfInstances = a })

-- | An IAM role for the job flow. The EC2 instances of the job flow assume
-- this role. The default role is EMRJobflowDefault. In order to use the
-- default role, you must have already created it using the CLI.
rjfJobFlowRole :: Lens' RunJobFlow (Maybe Text)
rjfJobFlowRole = lens _rjfJobFlowRole (\s a -> s { _rjfJobFlowRole = a })

-- | The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
rjfLogUri :: Lens' RunJobFlow (Maybe Text)
rjfLogUri = lens _rjfLogUri (\s a -> s { _rjfLogUri = a })

-- | The name of the job flow.
rjfName :: Lens' RunJobFlow Text
rjfName = lens _rjfName (\s a -> s { _rjfName = a })

-- | A list of strings that indicates third-party software to use with the job
-- flow that accepts a user argument list. EMR accepts and forwards the
-- argument list to the corresponding installation script as bootstrap
-- action arguments. For more information, see Launch a Job Flow on the MapR
-- Distribution for Hadoop. Currently supported values are: "mapr-m3" -
-- launch the job flow using MapR M3 Edition. "mapr-m5" - launch the job
-- flow using MapR M5 Edition. "mapr" with the user arguments specifying
-- "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or
-- M5 Edition respectively.
rjfNewSupportedProducts :: Lens' RunJobFlow [SupportedProductConfig]
rjfNewSupportedProducts =
    lens _rjfNewSupportedProducts (\s a -> s { _rjfNewSupportedProducts = a })

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS
-- resources on your behalf.
rjfServiceRole :: Lens' RunJobFlow (Maybe Text)
rjfServiceRole = lens _rjfServiceRole (\s a -> s { _rjfServiceRole = a })

-- | A list of steps to be executed by the job flow.
rjfSteps :: Lens' RunJobFlow [StepConfig]
rjfSteps = lens _rjfSteps (\s a -> s { _rjfSteps = a })

-- | A list of strings that indicates third-party software to use with the job
-- flow. For more information, go to Use Third Party Applications with
-- Amazon EMR. Currently supported values are: "mapr-m3" - launch the job
-- flow using MapR M3 Edition. "mapr-m5" - launch the job flow using MapR M5
-- Edition.
rjfSupportedProducts :: Lens' RunJobFlow [Text]
rjfSupportedProducts =
    lens _rjfSupportedProducts (\s a -> s { _rjfSupportedProducts = a })

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
rjfTags :: Lens' RunJobFlow [Tag]
rjfTags = lens _rjfTags (\s a -> s { _rjfTags = a })

-- | Whether the job flow is visible to all IAM users of the AWS account
-- associated with the job flow. If this value is set to true, all IAM users
-- of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to false, only the IAM
-- user that created the job flow can view and manage it.
rjfVisibleToAllUsers :: Lens' RunJobFlow (Maybe Bool)
rjfVisibleToAllUsers =
    lens _rjfVisibleToAllUsers (\s a -> s { _rjfVisibleToAllUsers = a })

instance ToPath RunJobFlow where
    toPath = const "/"

instance ToQuery RunJobFlow where
    toQuery = const mempty

instance ToHeaders RunJobFlow

instance ToBody RunJobFlow where
    toBody = toBody . encode . _rjfName

newtype RunJobFlowResponse = RunJobFlowResponse
    { _rjfrJobFlowId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RunJobFlowResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjfrJobFlowId' @::@ 'Maybe' 'Text'
--
runJobFlowResponse :: RunJobFlowResponse
runJobFlowResponse = RunJobFlowResponse
    { _rjfrJobFlowId = Nothing
    }

-- | An unique identifier for the job flow.
rjfrJobFlowId :: Lens' RunJobFlowResponse (Maybe Text)
rjfrJobFlowId = lens _rjfrJobFlowId (\s a -> s { _rjfrJobFlowId = a })

instance AWSRequest RunJobFlow where
    type Sv RunJobFlow = EMR
    type Rs RunJobFlow = RunJobFlowResponse

    request  = post
    response = jsonResponse $ \h o -> RunJobFlowResponse
        <$> o .: "JobFlowId"
