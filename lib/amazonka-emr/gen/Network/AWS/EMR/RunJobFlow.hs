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
-- Module      : Network.AWS.EMR.RunJobFlow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- RunJobFlow creates and starts running a new cluster (job flow). The cluster runs the steps specified. After the steps complete, the cluster stops and the HDFS partition is lost. To prevent loss of data, configure the last step of the job flow to store results in Amazon S3. If the 'JobFlowInstancesConfig' @KeepJobFlowAliveWhenNoSteps@ parameter is set to @TRUE@ , the cluster transitions to the WAITING state rather than shutting down after the steps have completed.
--
--
-- For additional protection, you can set the 'JobFlowInstancesConfig' @TerminationProtected@ parameter to @TRUE@ to lock the cluster and prevent it from being terminated by API call, user intervention, or in the event of a job flow error.
--
-- A maximum of 256 steps are allowed in each job flow.
--
-- If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using the SSH shell to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster> in the /Amazon EMR Management Guide/ .
--
-- For long running clusters, we recommend that you periodically store your results.
--
module Network.AWS.EMR.RunJobFlow
    (
    -- * Creating a Request
      runJobFlow
    , RunJobFlow
    -- * Request Lenses
    , rjfAMIVersion
    , rjfEBSRootVolumeSize
    , rjfAdditionalInfo
    , rjfConfigurations
    , rjfCustomAMIId
    , rjfAutoScalingRole
    , rjfSecurityConfiguration
    , rjfScaleDownBehavior
    , rjfSteps
    , rjfJobFlowRole
    , rjfBootstrapActions
    , rjfReleaseLabel
    , rjfRepoUpgradeOnBoot
    , rjfLogURI
    , rjfKerberosAttributes
    , rjfNewSupportedProducts
    , rjfVisibleToAllUsers
    , rjfSupportedProducts
    , rjfApplications
    , rjfTags
    , rjfServiceRole
    , rjfName
    , rjfInstances

    -- * Destructuring the Response
    , runJobFlowResponse
    , RunJobFlowResponse
    -- * Response Lenses
    , rjfrsJobFlowId
    , rjfrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the 'RunJobFlow' operation.
--
--
--
-- /See:/ 'runJobFlow' smart constructor.
data RunJobFlow = RunJobFlow'
  { _rjfAMIVersion            :: !(Maybe Text)
  , _rjfEBSRootVolumeSize     :: !(Maybe Int)
  , _rjfAdditionalInfo        :: !(Maybe Text)
  , _rjfConfigurations        :: !(Maybe [Configuration])
  , _rjfCustomAMIId           :: !(Maybe Text)
  , _rjfAutoScalingRole       :: !(Maybe Text)
  , _rjfSecurityConfiguration :: !(Maybe Text)
  , _rjfScaleDownBehavior     :: !(Maybe ScaleDownBehavior)
  , _rjfSteps                 :: !(Maybe [StepConfig])
  , _rjfJobFlowRole           :: !(Maybe Text)
  , _rjfBootstrapActions      :: !(Maybe [BootstrapActionConfig])
  , _rjfReleaseLabel          :: !(Maybe Text)
  , _rjfRepoUpgradeOnBoot     :: !(Maybe RepoUpgradeOnBoot)
  , _rjfLogURI                :: !(Maybe Text)
  , _rjfKerberosAttributes    :: !(Maybe KerberosAttributes)
  , _rjfNewSupportedProducts  :: !(Maybe [SupportedProductConfig])
  , _rjfVisibleToAllUsers     :: !(Maybe Bool)
  , _rjfSupportedProducts     :: !(Maybe [Text])
  , _rjfApplications          :: !(Maybe [Application])
  , _rjfTags                  :: !(Maybe [Tag])
  , _rjfServiceRole           :: !(Maybe Text)
  , _rjfName                  :: !Text
  , _rjfInstances             :: !JobFlowInstancesConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunJobFlow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjfAMIVersion' - For Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, the Linux AMI is determined by the @ReleaseLabel@ specified or by @CustomAmiID@ . The version of the Amazon Machine Image (AMI) to use when launching Amazon EC2 instances in the job flow. For details about the AMI versions currently supported in EMR version 3.x and 2.x, see <emr/latest/DeveloperGuide/emr-dg.pdf#nameddest=ami-versions-supported AMI Versions Supported in EMR> in the /Amazon EMR Developer Guide/ .  If the AMI supports multiple versions of Hadoop (for example, AMI 1.0 supports both Hadoop 0.18 and 0.20), you can use the 'JobFlowInstancesConfig' @HadoopVersion@ parameter to modify the version of Hadoop from the defaults shown above.
--
-- * 'rjfEBSRootVolumeSize' - The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- * 'rjfAdditionalInfo' - A JSON string for selecting additional features.
--
-- * 'rjfConfigurations' - For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
--
-- * 'rjfCustomAMIId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead. For information about creating a custom AMI, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
--
-- * 'rjfAutoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- * 'rjfSecurityConfiguration' - The name of a security configuration to apply to the cluster.
--
-- * 'rjfScaleDownBehavior' - Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- * 'rjfSteps' - A list of steps to run.
--
-- * 'rjfJobFlowRole' - Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
--
-- * 'rjfBootstrapActions' - A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
--
-- * 'rjfReleaseLabel' - The release label for the Amazon EMR release. For Amazon EMR 3.x and 2.x AMIs, use @AmiVersion@ instead.
--
-- * 'rjfRepoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
--
-- * 'rjfLogURI' - The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
--
-- * 'rjfKerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
--
-- * 'rjfNewSupportedProducts' - A list of strings that indicates third-party software to use with the job flow that accepts a user argument list. EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action arguments. For more information, see "Launch a Job Flow on the MapR Distribution for Hadoop" in the <http://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Supported values are:     * "mapr-m3" - launch the cluster using MapR M3 Edition.     * "mapr-m5" - launch the cluster using MapR M5 Edition.     * "mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition respectively.     * "mapr-m7" - launch the cluster using MapR M7 Edition.     * "hunk" - launch the cluster with the Hunk Big Data Analtics Platform.     * "hue"- launch the cluster with Hue installed.     * "spark" - launch the cluster with Apache Spark installed.     * "ganglia" - launch the cluster with the Ganglia Monitoring System installed.
--
-- * 'rjfVisibleToAllUsers' - Whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and (if they have the proper policy permissions set) manage the cluster. If it is set to @false@ , only the IAM user that created the cluster can view and manage it.
--
-- * 'rjfSupportedProducts' - A list of strings that indicates third-party software to use. For more information, see the <http://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Currently supported values are:     * "mapr-m3" - launch the job flow using MapR M3 Edition.     * "mapr-m5" - launch the job flow using MapR M5 Edition.
--
-- * 'rjfApplications' - For Amazon EMR releases 4.0 and later. A list of applications for the cluster. Valid values are: "Hadoop", "Hive", "Mahout", "Pig", and "Spark." They are case insensitive.
--
-- * 'rjfTags' - A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
--
-- * 'rjfServiceRole' - The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- * 'rjfName' - The name of the job flow.
--
-- * 'rjfInstances' - A specification of the number and type of Amazon EC2 instances.
runJobFlow
    :: Text -- ^ 'rjfName'
    -> JobFlowInstancesConfig -- ^ 'rjfInstances'
    -> RunJobFlow
runJobFlow pName_ pInstances_ =
  RunJobFlow'
    { _rjfAMIVersion = Nothing
    , _rjfEBSRootVolumeSize = Nothing
    , _rjfAdditionalInfo = Nothing
    , _rjfConfigurations = Nothing
    , _rjfCustomAMIId = Nothing
    , _rjfAutoScalingRole = Nothing
    , _rjfSecurityConfiguration = Nothing
    , _rjfScaleDownBehavior = Nothing
    , _rjfSteps = Nothing
    , _rjfJobFlowRole = Nothing
    , _rjfBootstrapActions = Nothing
    , _rjfReleaseLabel = Nothing
    , _rjfRepoUpgradeOnBoot = Nothing
    , _rjfLogURI = Nothing
    , _rjfKerberosAttributes = Nothing
    , _rjfNewSupportedProducts = Nothing
    , _rjfVisibleToAllUsers = Nothing
    , _rjfSupportedProducts = Nothing
    , _rjfApplications = Nothing
    , _rjfTags = Nothing
    , _rjfServiceRole = Nothing
    , _rjfName = pName_
    , _rjfInstances = pInstances_
    }


-- | For Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, the Linux AMI is determined by the @ReleaseLabel@ specified or by @CustomAmiID@ . The version of the Amazon Machine Image (AMI) to use when launching Amazon EC2 instances in the job flow. For details about the AMI versions currently supported in EMR version 3.x and 2.x, see <emr/latest/DeveloperGuide/emr-dg.pdf#nameddest=ami-versions-supported AMI Versions Supported in EMR> in the /Amazon EMR Developer Guide/ .  If the AMI supports multiple versions of Hadoop (for example, AMI 1.0 supports both Hadoop 0.18 and 0.20), you can use the 'JobFlowInstancesConfig' @HadoopVersion@ parameter to modify the version of Hadoop from the defaults shown above.
rjfAMIVersion :: Lens' RunJobFlow (Maybe Text)
rjfAMIVersion = lens _rjfAMIVersion (\ s a -> s{_rjfAMIVersion = a})

-- | The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
rjfEBSRootVolumeSize :: Lens' RunJobFlow (Maybe Int)
rjfEBSRootVolumeSize = lens _rjfEBSRootVolumeSize (\ s a -> s{_rjfEBSRootVolumeSize = a})

-- | A JSON string for selecting additional features.
rjfAdditionalInfo :: Lens' RunJobFlow (Maybe Text)
rjfAdditionalInfo = lens _rjfAdditionalInfo (\ s a -> s{_rjfAdditionalInfo = a})

-- | For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
rjfConfigurations :: Lens' RunJobFlow [Configuration]
rjfConfigurations = lens _rjfConfigurations (\ s a -> s{_rjfConfigurations = a}) . _Default . _Coerce

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead. For information about creating a custom AMI, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
rjfCustomAMIId :: Lens' RunJobFlow (Maybe Text)
rjfCustomAMIId = lens _rjfCustomAMIId (\ s a -> s{_rjfCustomAMIId = a})

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
rjfAutoScalingRole :: Lens' RunJobFlow (Maybe Text)
rjfAutoScalingRole = lens _rjfAutoScalingRole (\ s a -> s{_rjfAutoScalingRole = a})

-- | The name of a security configuration to apply to the cluster.
rjfSecurityConfiguration :: Lens' RunJobFlow (Maybe Text)
rjfSecurityConfiguration = lens _rjfSecurityConfiguration (\ s a -> s{_rjfSecurityConfiguration = a})

-- | Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
rjfScaleDownBehavior :: Lens' RunJobFlow (Maybe ScaleDownBehavior)
rjfScaleDownBehavior = lens _rjfScaleDownBehavior (\ s a -> s{_rjfScaleDownBehavior = a})

-- | A list of steps to run.
rjfSteps :: Lens' RunJobFlow [StepConfig]
rjfSteps = lens _rjfSteps (\ s a -> s{_rjfSteps = a}) . _Default . _Coerce

-- | Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
rjfJobFlowRole :: Lens' RunJobFlow (Maybe Text)
rjfJobFlowRole = lens _rjfJobFlowRole (\ s a -> s{_rjfJobFlowRole = a})

-- | A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
rjfBootstrapActions :: Lens' RunJobFlow [BootstrapActionConfig]
rjfBootstrapActions = lens _rjfBootstrapActions (\ s a -> s{_rjfBootstrapActions = a}) . _Default . _Coerce

-- | The release label for the Amazon EMR release. For Amazon EMR 3.x and 2.x AMIs, use @AmiVersion@ instead.
rjfReleaseLabel :: Lens' RunJobFlow (Maybe Text)
rjfReleaseLabel = lens _rjfReleaseLabel (\ s a -> s{_rjfReleaseLabel = a})

-- | Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
rjfRepoUpgradeOnBoot :: Lens' RunJobFlow (Maybe RepoUpgradeOnBoot)
rjfRepoUpgradeOnBoot = lens _rjfRepoUpgradeOnBoot (\ s a -> s{_rjfRepoUpgradeOnBoot = a})

-- | The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
rjfLogURI :: Lens' RunJobFlow (Maybe Text)
rjfLogURI = lens _rjfLogURI (\ s a -> s{_rjfLogURI = a})

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
rjfKerberosAttributes :: Lens' RunJobFlow (Maybe KerberosAttributes)
rjfKerberosAttributes = lens _rjfKerberosAttributes (\ s a -> s{_rjfKerberosAttributes = a})

-- | A list of strings that indicates third-party software to use with the job flow that accepts a user argument list. EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action arguments. For more information, see "Launch a Job Flow on the MapR Distribution for Hadoop" in the <http://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Supported values are:     * "mapr-m3" - launch the cluster using MapR M3 Edition.     * "mapr-m5" - launch the cluster using MapR M5 Edition.     * "mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition respectively.     * "mapr-m7" - launch the cluster using MapR M7 Edition.     * "hunk" - launch the cluster with the Hunk Big Data Analtics Platform.     * "hue"- launch the cluster with Hue installed.     * "spark" - launch the cluster with Apache Spark installed.     * "ganglia" - launch the cluster with the Ganglia Monitoring System installed.
rjfNewSupportedProducts :: Lens' RunJobFlow [SupportedProductConfig]
rjfNewSupportedProducts = lens _rjfNewSupportedProducts (\ s a -> s{_rjfNewSupportedProducts = a}) . _Default . _Coerce

-- | Whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and (if they have the proper policy permissions set) manage the cluster. If it is set to @false@ , only the IAM user that created the cluster can view and manage it.
rjfVisibleToAllUsers :: Lens' RunJobFlow (Maybe Bool)
rjfVisibleToAllUsers = lens _rjfVisibleToAllUsers (\ s a -> s{_rjfVisibleToAllUsers = a})

-- | A list of strings that indicates third-party software to use. For more information, see the <http://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Currently supported values are:     * "mapr-m3" - launch the job flow using MapR M3 Edition.     * "mapr-m5" - launch the job flow using MapR M5 Edition.
rjfSupportedProducts :: Lens' RunJobFlow [Text]
rjfSupportedProducts = lens _rjfSupportedProducts (\ s a -> s{_rjfSupportedProducts = a}) . _Default . _Coerce

-- | For Amazon EMR releases 4.0 and later. A list of applications for the cluster. Valid values are: "Hadoop", "Hive", "Mahout", "Pig", and "Spark." They are case insensitive.
rjfApplications :: Lens' RunJobFlow [Application]
rjfApplications = lens _rjfApplications (\ s a -> s{_rjfApplications = a}) . _Default . _Coerce

-- | A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
rjfTags :: Lens' RunJobFlow [Tag]
rjfTags = lens _rjfTags (\ s a -> s{_rjfTags = a}) . _Default . _Coerce

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
rjfServiceRole :: Lens' RunJobFlow (Maybe Text)
rjfServiceRole = lens _rjfServiceRole (\ s a -> s{_rjfServiceRole = a})

-- | The name of the job flow.
rjfName :: Lens' RunJobFlow Text
rjfName = lens _rjfName (\ s a -> s{_rjfName = a})

-- | A specification of the number and type of Amazon EC2 instances.
rjfInstances :: Lens' RunJobFlow JobFlowInstancesConfig
rjfInstances = lens _rjfInstances (\ s a -> s{_rjfInstances = a})

instance AWSRequest RunJobFlow where
        type Rs RunJobFlow = RunJobFlowResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 RunJobFlowResponse' <$>
                   (x .?> "JobFlowId") <*> (pure (fromEnum s)))

instance Hashable RunJobFlow where

instance NFData RunJobFlow where

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
              (catMaybes
                 [("AmiVersion" .=) <$> _rjfAMIVersion,
                  ("EbsRootVolumeSize" .=) <$> _rjfEBSRootVolumeSize,
                  ("AdditionalInfo" .=) <$> _rjfAdditionalInfo,
                  ("Configurations" .=) <$> _rjfConfigurations,
                  ("CustomAmiId" .=) <$> _rjfCustomAMIId,
                  ("AutoScalingRole" .=) <$> _rjfAutoScalingRole,
                  ("SecurityConfiguration" .=) <$>
                    _rjfSecurityConfiguration,
                  ("ScaleDownBehavior" .=) <$> _rjfScaleDownBehavior,
                  ("Steps" .=) <$> _rjfSteps,
                  ("JobFlowRole" .=) <$> _rjfJobFlowRole,
                  ("BootstrapActions" .=) <$> _rjfBootstrapActions,
                  ("ReleaseLabel" .=) <$> _rjfReleaseLabel,
                  ("RepoUpgradeOnBoot" .=) <$> _rjfRepoUpgradeOnBoot,
                  ("LogUri" .=) <$> _rjfLogURI,
                  ("KerberosAttributes" .=) <$> _rjfKerberosAttributes,
                  ("NewSupportedProducts" .=) <$>
                    _rjfNewSupportedProducts,
                  ("VisibleToAllUsers" .=) <$> _rjfVisibleToAllUsers,
                  ("SupportedProducts" .=) <$> _rjfSupportedProducts,
                  ("Applications" .=) <$> _rjfApplications,
                  ("Tags" .=) <$> _rjfTags,
                  ("ServiceRole" .=) <$> _rjfServiceRole,
                  Just ("Name" .= _rjfName),
                  Just ("Instances" .= _rjfInstances)])

instance ToPath RunJobFlow where
        toPath = const "/"

instance ToQuery RunJobFlow where
        toQuery = const mempty

-- | The result of the 'RunJobFlow' operation.
--
--
--
-- /See:/ 'runJobFlowResponse' smart constructor.
data RunJobFlowResponse = RunJobFlowResponse'
  { _rjfrsJobFlowId      :: !(Maybe Text)
  , _rjfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunJobFlowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjfrsJobFlowId' - An unique identifier for the job flow.
--
-- * 'rjfrsResponseStatus' - -- | The response status code.
runJobFlowResponse
    :: Int -- ^ 'rjfrsResponseStatus'
    -> RunJobFlowResponse
runJobFlowResponse pResponseStatus_ =
  RunJobFlowResponse'
    {_rjfrsJobFlowId = Nothing, _rjfrsResponseStatus = pResponseStatus_}


-- | An unique identifier for the job flow.
rjfrsJobFlowId :: Lens' RunJobFlowResponse (Maybe Text)
rjfrsJobFlowId = lens _rjfrsJobFlowId (\ s a -> s{_rjfrsJobFlowId = a})

-- | -- | The response status code.
rjfrsResponseStatus :: Lens' RunJobFlowResponse Int
rjfrsResponseStatus = lens _rjfrsResponseStatus (\ s a -> s{_rjfrsResponseStatus = a})

instance NFData RunJobFlowResponse where
