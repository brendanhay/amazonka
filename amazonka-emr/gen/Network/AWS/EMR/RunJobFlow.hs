{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RunJobFlow
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- RunJobFlow creates and starts running a new job flow. The job flow will
-- run the steps specified. Once the job flow completes, the cluster is
-- stopped and the HDFS partition is lost. To prevent loss of data,
-- configure the last step of the job flow to store results in Amazon S3.
-- If the JobFlowInstancesConfig @KeepJobFlowAliveWhenNoSteps@ parameter is
-- set to @TRUE@, the job flow will transition to the WAITING state rather
-- than shutting down once the steps have completed.
--
-- For additional protection, you can set the JobFlowInstancesConfig
-- @TerminationProtected@ parameter to @TRUE@ to lock the job flow and
-- prevent it from being terminated by API call, user intervention, or in
-- the event of a job flow error.
--
-- A maximum of 256 steps are allowed in each job flow.
--
-- If your job flow is long-running (such as a Hive data warehouse) or
-- complex, you may require more than 256 steps to process your data. You
-- can bypass the 256-step limitation in various ways, including using the
-- SSH shell to connect to the master node and submitting queries directly
-- to the software running on the master node, such as Hive and Hadoop. For
-- more information on how to do this, go to
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Job Flow>
-- in the /Amazon Elastic MapReduce Developer\'s Guide/.
--
-- For long running job flows, we recommend that you periodically store
-- your results.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_RunJobFlow.html>
module Network.AWS.EMR.RunJobFlow
    (
    -- * Request
      RunJobFlow
    -- ** Request constructor
    , runJobFlow
    -- ** Request lenses
    , rjfrqAMIVersion
    , rjfrqAdditionalInfo
    , rjfrqJobFlowRole
    , rjfrqSteps
    , rjfrqBootstrapActions
    , rjfrqNewSupportedProducts
    , rjfrqLogURI
    , rjfrqSupportedProducts
    , rjfrqVisibleToAllUsers
    , rjfrqTags
    , rjfrqServiceRole
    , rjfrqName
    , rjfrqInstances

    -- * Response
    , RunJobFlowResponse
    -- ** Response constructor
    , runJobFlowResponse
    -- ** Response lenses
    , rjfrsJobFlowId
    , rjfrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the RunJobFlow operation.
--
-- /See:/ 'runJobFlow' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjfrqAMIVersion'
--
-- * 'rjfrqAdditionalInfo'
--
-- * 'rjfrqJobFlowRole'
--
-- * 'rjfrqSteps'
--
-- * 'rjfrqBootstrapActions'
--
-- * 'rjfrqNewSupportedProducts'
--
-- * 'rjfrqLogURI'
--
-- * 'rjfrqSupportedProducts'
--
-- * 'rjfrqVisibleToAllUsers'
--
-- * 'rjfrqTags'
--
-- * 'rjfrqServiceRole'
--
-- * 'rjfrqName'
--
-- * 'rjfrqInstances'
data RunJobFlow = RunJobFlow'
    { _rjfrqAMIVersion           :: !(Maybe Text)
    , _rjfrqAdditionalInfo       :: !(Maybe Text)
    , _rjfrqJobFlowRole          :: !(Maybe Text)
    , _rjfrqSteps                :: !(Maybe [StepConfig])
    , _rjfrqBootstrapActions     :: !(Maybe [BootstrapActionConfig])
    , _rjfrqNewSupportedProducts :: !(Maybe [SupportedProductConfig])
    , _rjfrqLogURI               :: !(Maybe Text)
    , _rjfrqSupportedProducts    :: !(Maybe [Text])
    , _rjfrqVisibleToAllUsers    :: !(Maybe Bool)
    , _rjfrqTags                 :: !(Maybe [Tag])
    , _rjfrqServiceRole          :: !(Maybe Text)
    , _rjfrqName                 :: !Text
    , _rjfrqInstances            :: !JobFlowInstancesConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RunJobFlow' smart constructor.
runJobFlow :: Text -> JobFlowInstancesConfig -> RunJobFlow
runJobFlow pName_ pInstances_ =
    RunJobFlow'
    { _rjfrqAMIVersion = Nothing
    , _rjfrqAdditionalInfo = Nothing
    , _rjfrqJobFlowRole = Nothing
    , _rjfrqSteps = Nothing
    , _rjfrqBootstrapActions = Nothing
    , _rjfrqNewSupportedProducts = Nothing
    , _rjfrqLogURI = Nothing
    , _rjfrqSupportedProducts = Nothing
    , _rjfrqVisibleToAllUsers = Nothing
    , _rjfrqTags = Nothing
    , _rjfrqServiceRole = Nothing
    , _rjfrqName = pName_
    , _rjfrqInstances = pInstances_
    }

-- | The version of the Amazon Machine Image (AMI) to use when launching
-- Amazon EC2 instances in the job flow. The following values are valid:
--
-- -   \"latest\" (uses the latest AMI)
-- -   The version number of the AMI to use, for example, \"2.0\"
--
-- If the AMI supports multiple versions of Hadoop (for example, AMI 1.0
-- supports both Hadoop 0.18 and 0.20) you can use the
-- JobFlowInstancesConfig @HadoopVersion@ parameter to modify the version
-- of Hadoop from the defaults shown above.
--
-- For details about the AMI versions currently supported by Amazon Elastic
-- MapReduce, go to
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/EnvironmentConfig_AMIVersion.html#ami-versions-supported AMI Versions Supported in Elastic MapReduce>
-- in the /Amazon Elastic MapReduce Developer\'s Guide./
rjfrqAMIVersion :: Lens' RunJobFlow (Maybe Text)
rjfrqAMIVersion = lens _rjfrqAMIVersion (\ s a -> s{_rjfrqAMIVersion = a});

-- | A JSON string for selecting additional features.
rjfrqAdditionalInfo :: Lens' RunJobFlow (Maybe Text)
rjfrqAdditionalInfo = lens _rjfrqAdditionalInfo (\ s a -> s{_rjfrqAdditionalInfo = a});

-- | An IAM role for the job flow. The EC2 instances of the job flow assume
-- this role. The default role is @EMRJobflowDefault@. In order to use the
-- default role, you must have already created it using the CLI.
rjfrqJobFlowRole :: Lens' RunJobFlow (Maybe Text)
rjfrqJobFlowRole = lens _rjfrqJobFlowRole (\ s a -> s{_rjfrqJobFlowRole = a});

-- | A list of steps to be executed by the job flow.
rjfrqSteps :: Lens' RunJobFlow [StepConfig]
rjfrqSteps = lens _rjfrqSteps (\ s a -> s{_rjfrqSteps = a}) . _Default;

-- | A list of bootstrap actions that will be run before Hadoop is started on
-- the cluster nodes.
rjfrqBootstrapActions :: Lens' RunJobFlow [BootstrapActionConfig]
rjfrqBootstrapActions = lens _rjfrqBootstrapActions (\ s a -> s{_rjfrqBootstrapActions = a}) . _Default;

-- | A list of strings that indicates third-party software to use with the
-- job flow that accepts a user argument list. EMR accepts and forwards the
-- argument list to the corresponding installation script as bootstrap
-- action arguments. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-mapr.html Launch a Job Flow on the MapR Distribution for Hadoop>.
-- Currently supported values are:
--
-- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
-- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
-- -   \"mapr\" with the user arguments specifying \"--edition,m3\" or
--     \"--edition,m5\" - launch the job flow using MapR M3 or M5 Edition
--     respectively.
rjfrqNewSupportedProducts :: Lens' RunJobFlow [SupportedProductConfig]
rjfrqNewSupportedProducts = lens _rjfrqNewSupportedProducts (\ s a -> s{_rjfrqNewSupportedProducts = a}) . _Default;

-- | The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
rjfrqLogURI :: Lens' RunJobFlow (Maybe Text)
rjfrqLogURI = lens _rjfrqLogURI (\ s a -> s{_rjfrqLogURI = a});

-- | A list of strings that indicates third-party software to use with the
-- job flow. For more information, go to
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-supported-products.html Use Third Party Applications with Amazon EMR>.
-- Currently supported values are:
--
-- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
-- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
rjfrqSupportedProducts :: Lens' RunJobFlow [Text]
rjfrqSupportedProducts = lens _rjfrqSupportedProducts (\ s a -> s{_rjfrqSupportedProducts = a}) . _Default;

-- | Whether the job flow is visible to all IAM users of the AWS account
-- associated with the job flow. If this value is set to @true@, all IAM
-- users of that AWS account can view and (if they have the proper policy
-- permissions set) manage the job flow. If it is set to @false@, only the
-- IAM user that created the job flow can view and manage it.
rjfrqVisibleToAllUsers :: Lens' RunJobFlow (Maybe Bool)
rjfrqVisibleToAllUsers = lens _rjfrqVisibleToAllUsers (\ s a -> s{_rjfrqVisibleToAllUsers = a});

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
rjfrqTags :: Lens' RunJobFlow [Tag]
rjfrqTags = lens _rjfrqTags (\ s a -> s{_rjfrqTags = a}) . _Default;

-- | The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
rjfrqServiceRole :: Lens' RunJobFlow (Maybe Text)
rjfrqServiceRole = lens _rjfrqServiceRole (\ s a -> s{_rjfrqServiceRole = a});

-- | The name of the job flow.
rjfrqName :: Lens' RunJobFlow Text
rjfrqName = lens _rjfrqName (\ s a -> s{_rjfrqName = a});

-- | A specification of the number and type of Amazon EC2 instances on which
-- to run the job flow.
rjfrqInstances :: Lens' RunJobFlow JobFlowInstancesConfig
rjfrqInstances = lens _rjfrqInstances (\ s a -> s{_rjfrqInstances = a});

instance AWSRequest RunJobFlow where
        type Sv RunJobFlow = EMR
        type Rs RunJobFlow = RunJobFlowResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RunJobFlowResponse' <$>
                   (x .?> "JobFlowId") <*> (pure (fromEnum s)))

instance ToHeaders RunJobFlow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.RunJobFlow" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RunJobFlow where
        toJSON RunJobFlow'{..}
          = object
              ["AmiVersion" .= _rjfrqAMIVersion,
               "AdditionalInfo" .= _rjfrqAdditionalInfo,
               "JobFlowRole" .= _rjfrqJobFlowRole,
               "Steps" .= _rjfrqSteps,
               "BootstrapActions" .= _rjfrqBootstrapActions,
               "NewSupportedProducts" .= _rjfrqNewSupportedProducts,
               "LogUri" .= _rjfrqLogURI,
               "SupportedProducts" .= _rjfrqSupportedProducts,
               "VisibleToAllUsers" .= _rjfrqVisibleToAllUsers,
               "Tags" .= _rjfrqTags,
               "ServiceRole" .= _rjfrqServiceRole,
               "Name" .= _rjfrqName, "Instances" .= _rjfrqInstances]

instance ToPath RunJobFlow where
        toPath = const "/"

instance ToQuery RunJobFlow where
        toQuery = const mempty

-- | The result of the RunJobFlow operation.
--
-- /See:/ 'runJobFlowResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjfrsJobFlowId'
--
-- * 'rjfrsStatus'
data RunJobFlowResponse = RunJobFlowResponse'
    { _rjfrsJobFlowId :: !(Maybe Text)
    , _rjfrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RunJobFlowResponse' smart constructor.
runJobFlowResponse :: Int -> RunJobFlowResponse
runJobFlowResponse pStatus_ =
    RunJobFlowResponse'
    { _rjfrsJobFlowId = Nothing
    , _rjfrsStatus = pStatus_
    }

-- | An unique identifier for the job flow.
rjfrsJobFlowId :: Lens' RunJobFlowResponse (Maybe Text)
rjfrsJobFlowId = lens _rjfrsJobFlowId (\ s a -> s{_rjfrsJobFlowId = a});

-- | FIXME: Undocumented member.
rjfrsStatus :: Lens' RunJobFlowResponse Int
rjfrsStatus = lens _rjfrsStatus (\ s a -> s{_rjfrsStatus = a});
