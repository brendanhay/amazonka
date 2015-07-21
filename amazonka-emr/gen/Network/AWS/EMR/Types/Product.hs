{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Product where

import           Network.AWS.EMR.Types.Sum
import           Network.AWS.Prelude

-- | An application is any Amazon or third-party software that you can add to
-- the cluster. This structure contains a list of strings that indicates
-- the software to use with the cluster and accepts a user argument list.
-- Amazon EMR accepts and forwards the argument list to the corresponding
-- installation script as bootstrap action argument. For more information,
-- see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-mapr.html Launch a Job Flow on the MapR Distribution for Hadoop>.
-- Currently supported values are:
--
-- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
-- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
-- -   \"mapr\" with the user arguments specifying \"--edition,m3\" or
--     \"--edition,m5\" - launch the job flow using MapR M3 or M5 Edition,
--     respectively.
--
-- /See:/ 'application' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'appAdditionalInfo'
--
-- * 'appArgs'
--
-- * 'appName'
--
-- * 'appVersion'
data Application = Application'
    { _appAdditionalInfo :: !(Maybe (Map Text Text))
    , _appArgs           :: !(Maybe [Text])
    , _appName           :: !(Maybe Text)
    , _appVersion        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Application' smart constructor.
application :: Application
application =
    Application'
    { _appAdditionalInfo = Nothing
    , _appArgs = Nothing
    , _appName = Nothing
    , _appVersion = Nothing
    }

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
appAdditionalInfo :: Lens' Application (HashMap Text Text)
appAdditionalInfo = lens _appAdditionalInfo (\ s a -> s{_appAdditionalInfo = a}) . _Default . _Map;

-- | Arguments for Amazon EMR to pass to the application.
appArgs :: Lens' Application [Text]
appArgs = lens _appArgs (\ s a -> s{_appArgs = a}) . _Default;

-- | The name of the application.
appName :: Lens' Application (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a});

-- | The version of the application.
appVersion :: Lens' Application (Maybe Text)
appVersion = lens _appVersion (\ s a -> s{_appVersion = a});

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "AdditionalInfo" .!= mempty) <*>
                     (x .:? "Args" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Version"))

-- | Configuration of a bootstrap action.
--
-- /See:/ 'bootstrapActionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bacName'
--
-- * 'bacScriptBootstrapAction'
data BootstrapActionConfig = BootstrapActionConfig'
    { _bacName                  :: !Text
    , _bacScriptBootstrapAction :: !ScriptBootstrapActionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BootstrapActionConfig' smart constructor.
bootstrapActionConfig :: Text -> ScriptBootstrapActionConfig -> BootstrapActionConfig
bootstrapActionConfig pName pScriptBootstrapAction =
    BootstrapActionConfig'
    { _bacName = pName
    , _bacScriptBootstrapAction = pScriptBootstrapAction
    }

-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig Text
bacName = lens _bacName (\ s a -> s{_bacName = a});

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction = lens _bacScriptBootstrapAction (\ s a -> s{_bacScriptBootstrapAction = a});

instance ToJSON BootstrapActionConfig where
        toJSON BootstrapActionConfig'{..}
          = object
              ["Name" .= _bacName,
               "ScriptBootstrapAction" .= _bacScriptBootstrapAction]

-- | The detailed description of the cluster.
--
-- /See:/ 'cluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cluRequestedAMIVersion'
--
-- * 'cluEC2InstanceAttributes'
--
-- * 'cluNormalizedInstanceHours'
--
-- * 'cluLogURI'
--
-- * 'cluRunningAMIVersion'
--
-- * 'cluMasterPublicDNSName'
--
-- * 'cluAutoTerminate'
--
-- * 'cluTerminationProtected'
--
-- * 'cluVisibleToAllUsers'
--
-- * 'cluApplications'
--
-- * 'cluTags'
--
-- * 'cluServiceRole'
--
-- * 'cluId'
--
-- * 'cluName'
--
-- * 'cluStatus'
data Cluster = Cluster'
    { _cluRequestedAMIVersion     :: !(Maybe Text)
    , _cluEC2InstanceAttributes   :: !(Maybe EC2InstanceAttributes)
    , _cluNormalizedInstanceHours :: !(Maybe Int)
    , _cluLogURI                  :: !(Maybe Text)
    , _cluRunningAMIVersion       :: !(Maybe Text)
    , _cluMasterPublicDNSName     :: !(Maybe Text)
    , _cluAutoTerminate           :: !(Maybe Bool)
    , _cluTerminationProtected    :: !(Maybe Bool)
    , _cluVisibleToAllUsers       :: !(Maybe Bool)
    , _cluApplications            :: !(Maybe [Application])
    , _cluTags                    :: !(Maybe [Tag])
    , _cluServiceRole             :: !(Maybe Text)
    , _cluId                      :: !Text
    , _cluName                    :: !Text
    , _cluStatus                  :: !ClusterStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Cluster' smart constructor.
cluster :: Text -> Text -> ClusterStatus -> Cluster
cluster pId pName pStatus =
    Cluster'
    { _cluRequestedAMIVersion = Nothing
    , _cluEC2InstanceAttributes = Nothing
    , _cluNormalizedInstanceHours = Nothing
    , _cluLogURI = Nothing
    , _cluRunningAMIVersion = Nothing
    , _cluMasterPublicDNSName = Nothing
    , _cluAutoTerminate = Nothing
    , _cluTerminationProtected = Nothing
    , _cluVisibleToAllUsers = Nothing
    , _cluApplications = Nothing
    , _cluTags = Nothing
    , _cluServiceRole = Nothing
    , _cluId = pId
    , _cluName = pName
    , _cluStatus = pStatus
    }

-- | The AMI version requested for this cluster.
cluRequestedAMIVersion :: Lens' Cluster (Maybe Text)
cluRequestedAMIVersion = lens _cluRequestedAMIVersion (\ s a -> s{_cluRequestedAMIVersion = a});

-- | FIXME: Undocumented member.
cluEC2InstanceAttributes :: Lens' Cluster (Maybe EC2InstanceAttributes)
cluEC2InstanceAttributes = lens _cluEC2InstanceAttributes (\ s a -> s{_cluEC2InstanceAttributes = a});

-- | An approximation of the cost of the job flow, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
cluNormalizedInstanceHours :: Lens' Cluster (Maybe Int)
cluNormalizedInstanceHours = lens _cluNormalizedInstanceHours (\ s a -> s{_cluNormalizedInstanceHours = a});

-- | The path to the Amazon S3 location where logs for this cluster are
-- stored.
cluLogURI :: Lens' Cluster (Maybe Text)
cluLogURI = lens _cluLogURI (\ s a -> s{_cluLogURI = a});

-- | The AMI version running on this cluster. This differs from the requested
-- version only if the requested version is a meta version, such as
-- \"latest\".
cluRunningAMIVersion :: Lens' Cluster (Maybe Text)
cluRunningAMIVersion = lens _cluRunningAMIVersion (\ s a -> s{_cluRunningAMIVersion = a});

-- | The public DNS name of the master Ec2 instance.
cluMasterPublicDNSName :: Lens' Cluster (Maybe Text)
cluMasterPublicDNSName = lens _cluMasterPublicDNSName (\ s a -> s{_cluMasterPublicDNSName = a});

-- | Specifies whether the cluster should terminate after completing all
-- steps.
cluAutoTerminate :: Lens' Cluster (Maybe Bool)
cluAutoTerminate = lens _cluAutoTerminate (\ s a -> s{_cluAutoTerminate = a});

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
cluTerminationProtected :: Lens' Cluster (Maybe Bool)
cluTerminationProtected = lens _cluTerminationProtected (\ s a -> s{_cluTerminationProtected = a});

-- | Indicates whether the job flow is visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to @true@,
-- all IAM users of that AWS account can view and manage the job flow if
-- they have the proper policy permissions set. If this value is @false@,
-- only the IAM user that created the cluster can view and manage it. This
-- value can be changed using the SetVisibleToAllUsers action.
cluVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cluVisibleToAllUsers = lens _cluVisibleToAllUsers (\ s a -> s{_cluVisibleToAllUsers = a});

-- | The applications installed on this cluster.
cluApplications :: Lens' Cluster [Application]
cluApplications = lens _cluApplications (\ s a -> s{_cluApplications = a}) . _Default;

-- | A list of tags associated with a cluster.
cluTags :: Lens' Cluster [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default;

-- | The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
cluServiceRole :: Lens' Cluster (Maybe Text)
cluServiceRole = lens _cluServiceRole (\ s a -> s{_cluServiceRole = a});

-- | The unique identifier for the cluster.
cluId :: Lens' Cluster Text
cluId = lens _cluId (\ s a -> s{_cluId = a});

-- | The name of the cluster.
cluName :: Lens' Cluster Text
cluName = lens _cluName (\ s a -> s{_cluName = a});

-- | The current status details about the cluster.
cluStatus :: Lens' Cluster ClusterStatus
cluStatus = lens _cluStatus (\ s a -> s{_cluStatus = a});

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "RequestedAmiVersion") <*>
                     (x .:? "Ec2InstanceAttributes")
                     <*> (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "LogUri")
                     <*> (x .:? "RunningAmiVersion")
                     <*> (x .:? "MasterPublicDnsName")
                     <*> (x .:? "AutoTerminate")
                     <*> (x .:? "TerminationProtected")
                     <*> (x .:? "VisibleToAllUsers")
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "ServiceRole")
                     <*> (x .: "Id")
                     <*> (x .: "Name")
                     <*> (x .: "Status"))

-- | The reason that the cluster changed to its current state.
--
-- /See:/ 'clusterStateChangeReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscrCode'
--
-- * 'cscrMessage'
data ClusterStateChangeReason = ClusterStateChangeReason'
    { _cscrCode    :: !(Maybe ClusterStateChangeReasonCode)
    , _cscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ClusterStateChangeReason' smart constructor.
clusterStateChangeReason :: ClusterStateChangeReason
clusterStateChangeReason =
    ClusterStateChangeReason'
    { _cscrCode = Nothing
    , _cscrMessage = Nothing
    }

-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe ClusterStateChangeReasonCode)
cscrCode = lens _cscrCode (\ s a -> s{_cscrCode = a});

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\ s a -> s{_cscrMessage = a});

instance FromJSON ClusterStateChangeReason where
        parseJSON
          = withObject "ClusterStateChangeReason"
              (\ x ->
                 ClusterStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

-- | The detailed status of the cluster.
--
-- /See:/ 'clusterStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csState'
--
-- * 'csStateChangeReason'
--
-- * 'csTimeline'
data ClusterStatus = ClusterStatus'
    { _csState             :: !(Maybe ClusterState)
    , _csStateChangeReason :: !(Maybe ClusterStateChangeReason)
    , _csTimeline          :: !(Maybe ClusterTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ClusterStatus' smart constructor.
clusterStatus :: ClusterStatus
clusterStatus =
    ClusterStatus'
    { _csState = Nothing
    , _csStateChangeReason = Nothing
    , _csTimeline = Nothing
    }

-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe ClusterState)
csState = lens _csState (\ s a -> s{_csState = a});

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason = lens _csStateChangeReason (\ s a -> s{_csStateChangeReason = a});

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\ s a -> s{_csTimeline = a});

instance FromJSON ClusterStatus where
        parseJSON
          = withObject "ClusterStatus"
              (\ x ->
                 ClusterStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

-- | The summary description of the cluster.
--
-- /See:/ 'clusterSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csStatus'
--
-- * 'csNormalizedInstanceHours'
--
-- * 'csName'
--
-- * 'csId'
data ClusterSummary = ClusterSummary'
    { _csStatus                  :: !(Maybe ClusterStatus)
    , _csNormalizedInstanceHours :: !(Maybe Int)
    , _csName                    :: !(Maybe Text)
    , _csId                      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ClusterSummary' smart constructor.
clusterSummary :: ClusterSummary
clusterSummary =
    ClusterSummary'
    { _csStatus = Nothing
    , _csNormalizedInstanceHours = Nothing
    , _csName = Nothing
    , _csId = Nothing
    }

-- | The details about the current status of the cluster.
csStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a});

-- | An approximation of the cost of the job flow, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
csNormalizedInstanceHours :: Lens' ClusterSummary (Maybe Int)
csNormalizedInstanceHours = lens _csNormalizedInstanceHours (\ s a -> s{_csNormalizedInstanceHours = a});

-- | The name of the cluster.
csName :: Lens' ClusterSummary (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a});

-- | The unique identifier for the cluster.
csId :: Lens' ClusterSummary (Maybe Text)
csId = lens _csId (\ s a -> s{_csId = a});

instance FromJSON ClusterSummary where
        parseJSON
          = withObject "ClusterSummary"
              (\ x ->
                 ClusterSummary' <$>
                   (x .:? "Status") <*>
                     (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

-- | Represents the timeline of the cluster\'s lifecycle.
--
-- /See:/ 'clusterTimeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctReadyDateTime'
--
-- * 'ctCreationDateTime'
--
-- * 'ctEndDateTime'
data ClusterTimeline = ClusterTimeline'
    { _ctReadyDateTime    :: !(Maybe POSIX)
    , _ctCreationDateTime :: !(Maybe POSIX)
    , _ctEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ClusterTimeline' smart constructor.
clusterTimeline :: ClusterTimeline
clusterTimeline =
    ClusterTimeline'
    { _ctReadyDateTime = Nothing
    , _ctCreationDateTime = Nothing
    , _ctEndDateTime = Nothing
    }

-- | The date and time when the cluster was ready to execute steps.
ctReadyDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctReadyDateTime = lens _ctReadyDateTime (\ s a -> s{_ctReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctCreationDateTime = lens _ctCreationDateTime (\ s a -> s{_ctCreationDateTime = a}) . mapping _Time;

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctEndDateTime = lens _ctEndDateTime (\ s a -> s{_ctEndDateTime = a}) . mapping _Time;

instance FromJSON ClusterTimeline where
        parseJSON
          = withObject "ClusterTimeline"
              (\ x ->
                 ClusterTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'command' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comArgs'
--
-- * 'comScriptPath'
--
-- * 'comName'
data Command = Command'
    { _comArgs       :: !(Maybe [Text])
    , _comScriptPath :: !(Maybe Text)
    , _comName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Command' smart constructor.
command :: Command
command =
    Command'
    { _comArgs = Nothing
    , _comScriptPath = Nothing
    , _comName = Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
comArgs :: Lens' Command [Text]
comArgs = lens _comArgs (\ s a -> s{_comArgs = a}) . _Default;

-- | The Amazon S3 location of the command script.
comScriptPath :: Lens' Command (Maybe Text)
comScriptPath = lens _comScriptPath (\ s a -> s{_comScriptPath = a});

-- | The name of the command.
comName :: Lens' Command (Maybe Text)
comName = lens _comName (\ s a -> s{_comName = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "ScriptPath")
                     <*> (x .:? "Name"))

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
--
-- /See:/ 'ec2InstanceAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiaEC2KeyName'
--
-- * 'eiaEmrManagedSlaveSecurityGroup'
--
-- * 'eiaAdditionalSlaveSecurityGroups'
--
-- * 'eiaIAMInstanceProfile'
--
-- * 'eiaAdditionalMasterSecurityGroups'
--
-- * 'eiaEmrManagedMasterSecurityGroup'
--
-- * 'eiaEC2SubnetId'
--
-- * 'eiaEC2AvailabilityZone'
data EC2InstanceAttributes = EC2InstanceAttributes'
    { _eiaEC2KeyName                     :: !(Maybe Text)
    , _eiaEmrManagedSlaveSecurityGroup   :: !(Maybe Text)
    , _eiaAdditionalSlaveSecurityGroups  :: !(Maybe [Text])
    , _eiaIAMInstanceProfile             :: !(Maybe Text)
    , _eiaAdditionalMasterSecurityGroups :: !(Maybe [Text])
    , _eiaEmrManagedMasterSecurityGroup  :: !(Maybe Text)
    , _eiaEC2SubnetId                    :: !(Maybe Text)
    , _eiaEC2AvailabilityZone            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EC2InstanceAttributes' smart constructor.
ec2InstanceAttributes :: EC2InstanceAttributes
ec2InstanceAttributes =
    EC2InstanceAttributes'
    { _eiaEC2KeyName = Nothing
    , _eiaEmrManagedSlaveSecurityGroup = Nothing
    , _eiaAdditionalSlaveSecurityGroups = Nothing
    , _eiaIAMInstanceProfile = Nothing
    , _eiaAdditionalMasterSecurityGroups = Nothing
    , _eiaEmrManagedMasterSecurityGroup = Nothing
    , _eiaEC2SubnetId = Nothing
    , _eiaEC2AvailabilityZone = Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named \"hadoop\".
eiaEC2KeyName :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2KeyName = lens _eiaEC2KeyName (\ s a -> s{_eiaEC2KeyName = a});

-- | The identifier of the Amazon EC2 security group (managed by Amazon
-- Elastic MapReduce) for the slave nodes.
eiaEmrManagedSlaveSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedSlaveSecurityGroup = lens _eiaEmrManagedSlaveSecurityGroup (\ s a -> s{_eiaEmrManagedSlaveSecurityGroup = a});

-- | A list of additional Amazon EC2 security group IDs for the slave nodes.
eiaAdditionalSlaveSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalSlaveSecurityGroups = lens _eiaAdditionalSlaveSecurityGroups (\ s a -> s{_eiaAdditionalSlaveSecurityGroups = a}) . _Default;

-- | The IAM role that was specified when the job flow was launched. The EC2
-- instances of the job flow assume this role.
eiaIAMInstanceProfile :: Lens' EC2InstanceAttributes (Maybe Text)
eiaIAMInstanceProfile = lens _eiaIAMInstanceProfile (\ s a -> s{_eiaIAMInstanceProfile = a});

-- | A list of additional Amazon EC2 security group IDs for the master node.
eiaAdditionalMasterSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalMasterSecurityGroups = lens _eiaAdditionalMasterSecurityGroups (\ s a -> s{_eiaAdditionalMasterSecurityGroups = a}) . _Default;

-- | The identifier of the Amazon EC2 security group (managed by Amazon
-- Elastic MapReduce) for the master node.
eiaEmrManagedMasterSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedMasterSecurityGroup = lens _eiaEmrManagedMasterSecurityGroup (\ s a -> s{_eiaEmrManagedMasterSecurityGroup = a});

-- | To launch the job flow in Amazon VPC, set this parameter to the
-- identifier of the Amazon VPC subnet where you want the job flow to
-- launch. If you do not specify this value, the job flow is launched in
-- the normal AWS cloud, outside of a VPC.
--
-- Amazon VPC currently does not support cluster compute quadruple extra
-- large (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge
-- instance type for nodes of a job flow launched in a VPC.
eiaEC2SubnetId :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2SubnetId = lens _eiaEC2SubnetId (\ s a -> s{_eiaEC2SubnetId = a});

-- | The Availability Zone in which the cluster will run.
eiaEC2AvailabilityZone :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2AvailabilityZone = lens _eiaEC2AvailabilityZone (\ s a -> s{_eiaEC2AvailabilityZone = a});

instance FromJSON EC2InstanceAttributes where
        parseJSON
          = withObject "EC2InstanceAttributes"
              (\ x ->
                 EC2InstanceAttributes' <$>
                   (x .:? "Ec2KeyName") <*>
                     (x .:? "EmrManagedSlaveSecurityGroup")
                     <*>
                     (x .:? "AdditionalSlaveSecurityGroups" .!= mempty)
                     <*> (x .:? "IamInstanceProfile")
                     <*>
                     (x .:? "AdditionalMasterSecurityGroups" .!= mempty)
                     <*> (x .:? "EmrManagedMasterSecurityGroup")
                     <*> (x .:? "Ec2SubnetId")
                     <*> (x .:? "Ec2AvailabilityZone"))

-- | A job flow step consisting of a JAR file whose main function will be
-- executed. The main function submits a job for Hadoop to execute and
-- waits for the job to finish or fail.
--
-- /See:/ 'hadoopJARStepConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hjscArgs'
--
-- * 'hjscMainClass'
--
-- * 'hjscProperties'
--
-- * 'hjscJAR'
data HadoopJARStepConfig = HadoopJARStepConfig'
    { _hjscArgs       :: !(Maybe [Text])
    , _hjscMainClass  :: !(Maybe Text)
    , _hjscProperties :: !(Maybe [KeyValue])
    , _hjscJAR        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HadoopJARStepConfig' smart constructor.
hadoopJARStepConfig :: Text -> HadoopJARStepConfig
hadoopJARStepConfig pJAR =
    HadoopJARStepConfig'
    { _hjscArgs = Nothing
    , _hjscMainClass = Nothing
    , _hjscProperties = Nothing
    , _hjscJAR = pJAR
    }

-- | A list of command line arguments passed to the JAR file\'s main function
-- when executed.
hjscArgs :: Lens' HadoopJARStepConfig [Text]
hjscArgs = lens _hjscArgs (\ s a -> s{_hjscArgs = a}) . _Default;

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJARStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\ s a -> s{_hjscMainClass = a});

-- | A list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJARStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\ s a -> s{_hjscProperties = a}) . _Default;

-- | A path to a JAR file run during the step.
hjscJAR :: Lens' HadoopJARStepConfig Text
hjscJAR = lens _hjscJAR (\ s a -> s{_hjscJAR = a});

instance ToJSON HadoopJARStepConfig where
        toJSON HadoopJARStepConfig'{..}
          = object
              ["Args" .= _hjscArgs, "MainClass" .= _hjscMainClass,
               "Properties" .= _hjscProperties, "Jar" .= _hjscJAR]

-- | A cluster step consisting of a JAR file whose main function will be
-- executed. The main function submits a job for Hadoop to execute and
-- waits for the job to finish or fail.
--
-- /See:/ 'hadoopStepConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hscArgs'
--
-- * 'hscJAR'
--
-- * 'hscMainClass'
--
-- * 'hscProperties'
data HadoopStepConfig = HadoopStepConfig'
    { _hscArgs       :: !(Maybe [Text])
    , _hscJAR        :: !(Maybe Text)
    , _hscMainClass  :: !(Maybe Text)
    , _hscProperties :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HadoopStepConfig' smart constructor.
hadoopStepConfig :: HadoopStepConfig
hadoopStepConfig =
    HadoopStepConfig'
    { _hscArgs = Nothing
    , _hscJAR = Nothing
    , _hscMainClass = Nothing
    , _hscProperties = Nothing
    }

-- | The list of command line arguments to pass to the JAR file\'s main
-- function for execution.
hscArgs :: Lens' HadoopStepConfig [Text]
hscArgs = lens _hscArgs (\ s a -> s{_hscArgs = a}) . _Default;

-- | The path to the JAR file that runs during the step.
hscJAR :: Lens' HadoopStepConfig (Maybe Text)
hscJAR = lens _hscJAR (\ s a -> s{_hscJAR = a});

-- | The name of the main class in the specified Java file. If not specified,
-- the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\ s a -> s{_hscMainClass = a});

-- | The list of Java properties that are set when the step runs. You can use
-- these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (HashMap Text Text)
hscProperties = lens _hscProperties (\ s a -> s{_hscProperties = a}) . _Default . _Map;

instance FromJSON HadoopStepConfig where
        parseJSON
          = withObject "HadoopStepConfig"
              (\ x ->
                 HadoopStepConfig' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "Jar") <*>
                     (x .:? "MainClass")
                     <*> (x .:? "Properties" .!= mempty))

-- | Represents an EC2 instance provisioned as part of cluster.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insStatus'
--
-- * 'insPublicDNSName'
--
-- * 'insEC2InstanceId'
--
-- * 'insPrivateIPAddress'
--
-- * 'insId'
--
-- * 'insPrivateDNSName'
--
-- * 'insPublicIPAddress'
data Instance = Instance'
    { _insStatus           :: !(Maybe InstanceStatus)
    , _insPublicDNSName    :: !(Maybe Text)
    , _insEC2InstanceId    :: !(Maybe Text)
    , _insPrivateIPAddress :: !(Maybe Text)
    , _insId               :: !(Maybe Text)
    , _insPrivateDNSName   :: !(Maybe Text)
    , _insPublicIPAddress  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' =
    Instance'
    { _insStatus = Nothing
    , _insPublicDNSName = Nothing
    , _insEC2InstanceId = Nothing
    , _insPrivateIPAddress = Nothing
    , _insId = Nothing
    , _insPrivateDNSName = Nothing
    , _insPublicIPAddress = Nothing
    }

-- | The current status of the instance.
insStatus :: Lens' Instance (Maybe InstanceStatus)
insStatus = lens _insStatus (\ s a -> s{_insStatus = a});

-- | The public DNS name of the instance.
insPublicDNSName :: Lens' Instance (Maybe Text)
insPublicDNSName = lens _insPublicDNSName (\ s a -> s{_insPublicDNSName = a});

-- | The unique identifier of the instance in Amazon EC2.
insEC2InstanceId :: Lens' Instance (Maybe Text)
insEC2InstanceId = lens _insEC2InstanceId (\ s a -> s{_insEC2InstanceId = a});

-- | The private IP address of the instance.
insPrivateIPAddress :: Lens' Instance (Maybe Text)
insPrivateIPAddress = lens _insPrivateIPAddress (\ s a -> s{_insPrivateIPAddress = a});

-- | The unique identifier for the instance in Amazon EMR.
insId :: Lens' Instance (Maybe Text)
insId = lens _insId (\ s a -> s{_insId = a});

-- | The private DNS name of the instance.
insPrivateDNSName :: Lens' Instance (Maybe Text)
insPrivateDNSName = lens _insPrivateDNSName (\ s a -> s{_insPrivateDNSName = a});

-- | The public IP address of the instance.
insPublicIPAddress :: Lens' Instance (Maybe Text)
insPublicIPAddress = lens _insPublicIPAddress (\ s a -> s{_insPublicIPAddress = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "Status") <*> (x .:? "PublicDnsName") <*>
                     (x .:? "Ec2InstanceId")
                     <*> (x .:? "PrivateIpAddress")
                     <*> (x .:? "Id")
                     <*> (x .:? "PrivateDnsName")
                     <*> (x .:? "PublicIpAddress"))

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
--
-- /See:/ 'instanceGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igStatus'
--
-- * 'igBidPrice'
--
-- * 'igRequestedInstanceCount'
--
-- * 'igRunningInstanceCount'
--
-- * 'igInstanceGroupType'
--
-- * 'igInstanceType'
--
-- * 'igMarket'
--
-- * 'igName'
--
-- * 'igId'
data InstanceGroup = InstanceGroup'
    { _igStatus                 :: !(Maybe InstanceGroupStatus)
    , _igBidPrice               :: !(Maybe Text)
    , _igRequestedInstanceCount :: !(Maybe Int)
    , _igRunningInstanceCount   :: !(Maybe Int)
    , _igInstanceGroupType      :: !(Maybe InstanceGroupType)
    , _igInstanceType           :: !(Maybe Text)
    , _igMarket                 :: !(Maybe MarketType)
    , _igName                   :: !(Maybe Text)
    , _igId                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroup' smart constructor.
instanceGroup :: InstanceGroup
instanceGroup =
    InstanceGroup'
    { _igStatus = Nothing
    , _igBidPrice = Nothing
    , _igRequestedInstanceCount = Nothing
    , _igRunningInstanceCount = Nothing
    , _igInstanceGroupType = Nothing
    , _igInstanceType = Nothing
    , _igMarket = Nothing
    , _igName = Nothing
    , _igId = Nothing
    }

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus = lens _igStatus (\ s a -> s{_igStatus = a});

-- | The bid price for each EC2 instance in the instance group when launching
-- nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice = lens _igBidPrice (\ s a -> s{_igBidPrice = a});

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRequestedInstanceCount = lens _igRequestedInstanceCount (\ s a -> s{_igRequestedInstanceCount = a});

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRunningInstanceCount = lens _igRunningInstanceCount (\ s a -> s{_igRunningInstanceCount = a});

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe InstanceGroupType)
igInstanceGroupType = lens _igInstanceGroupType (\ s a -> s{_igInstanceGroupType = a});

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType = lens _igInstanceType (\ s a -> s{_igInstanceType = a});

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe MarketType)
igMarket = lens _igMarket (\ s a -> s{_igMarket = a});

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName = lens _igName (\ s a -> s{_igName = a});

-- | The identifier of the instance group.
igId :: Lens' InstanceGroup (Maybe Text)
igId = lens _igId (\ s a -> s{_igId = a});

instance FromJSON InstanceGroup where
        parseJSON
          = withObject "InstanceGroup"
              (\ x ->
                 InstanceGroup' <$>
                   (x .:? "Status") <*> (x .:? "BidPrice") <*>
                     (x .:? "RequestedInstanceCount")
                     <*> (x .:? "RunningInstanceCount")
                     <*> (x .:? "InstanceGroupType")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "Market")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

-- | Configuration defining a new instance group.
--
-- /See:/ 'instanceGroupConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igcBidPrice'
--
-- * 'igcMarket'
--
-- * 'igcName'
--
-- * 'igcInstanceRole'
--
-- * 'igcInstanceType'
--
-- * 'igcInstanceCount'
data InstanceGroupConfig = InstanceGroupConfig'
    { _igcBidPrice      :: !(Maybe Text)
    , _igcMarket        :: !(Maybe MarketType)
    , _igcName          :: !(Maybe Text)
    , _igcInstanceRole  :: !InstanceRoleType
    , _igcInstanceType  :: !Text
    , _igcInstanceCount :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroupConfig' smart constructor.
instanceGroupConfig :: InstanceRoleType -> Text -> Int -> InstanceGroupConfig
instanceGroupConfig pInstanceRole pInstanceType pInstanceCount =
    InstanceGroupConfig'
    { _igcBidPrice = Nothing
    , _igcMarket = Nothing
    , _igcName = Nothing
    , _igcInstanceRole = pInstanceRole
    , _igcInstanceType = pInstanceType
    , _igcInstanceCount = pInstanceCount
    }

-- | Bid price for each Amazon EC2 instance in the instance group when
-- launching nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\ s a -> s{_igcBidPrice = a});

-- | Market type of the Amazon EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket = lens _igcMarket (\ s a -> s{_igcMarket = a});

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\ s a -> s{_igcName = a});

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig InstanceRoleType
igcInstanceRole = lens _igcInstanceRole (\ s a -> s{_igcInstanceRole = a});

-- | The Amazon EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig Text
igcInstanceType = lens _igcInstanceType (\ s a -> s{_igcInstanceType = a});

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig Int
igcInstanceCount = lens _igcInstanceCount (\ s a -> s{_igcInstanceCount = a});

instance ToJSON InstanceGroupConfig where
        toJSON InstanceGroupConfig'{..}
          = object
              ["BidPrice" .= _igcBidPrice, "Market" .= _igcMarket,
               "Name" .= _igcName,
               "InstanceRole" .= _igcInstanceRole,
               "InstanceType" .= _igcInstanceType,
               "InstanceCount" .= _igcInstanceCount]

-- | Modify an instance group size.
--
-- /See:/ 'instanceGroupModifyConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igmcInstanceCount'
--
-- * 'igmcEC2InstanceIdsToTerminate'
--
-- * 'igmcInstanceGroupId'
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
    { _igmcInstanceCount             :: !(Maybe Int)
    , _igmcEC2InstanceIdsToTerminate :: !(Maybe [Text])
    , _igmcInstanceGroupId           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroupModifyConfig' smart constructor.
instanceGroupModifyConfig :: Text -> InstanceGroupModifyConfig
instanceGroupModifyConfig pInstanceGroupId =
    InstanceGroupModifyConfig'
    { _igmcInstanceCount = Nothing
    , _igmcEC2InstanceIdsToTerminate = Nothing
    , _igmcInstanceGroupId = pInstanceGroupId
    }

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Int)
igmcInstanceCount = lens _igmcInstanceCount (\ s a -> s{_igmcInstanceCount = a});

-- | The EC2 InstanceIds to terminate. For advanced users only. Once you
-- terminate the instances, the instance group will not return to its
-- original requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig [Text]
igmcEC2InstanceIdsToTerminate = lens _igmcEC2InstanceIdsToTerminate (\ s a -> s{_igmcEC2InstanceIdsToTerminate = a}) . _Default;

-- | Unique ID of the instance group to expand or shrink.
igmcInstanceGroupId :: Lens' InstanceGroupModifyConfig Text
igmcInstanceGroupId = lens _igmcInstanceGroupId (\ s a -> s{_igmcInstanceGroupId = a});

instance ToJSON InstanceGroupModifyConfig where
        toJSON InstanceGroupModifyConfig'{..}
          = object
              ["InstanceCount" .= _igmcInstanceCount,
               "EC2InstanceIdsToTerminate" .=
                 _igmcEC2InstanceIdsToTerminate,
               "InstanceGroupId" .= _igmcInstanceGroupId]

-- | The status change reason details for the instance group.
--
-- /See:/ 'instanceGroupStateChangeReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igscrCode'
--
-- * 'igscrMessage'
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
    { _igscrCode    :: !(Maybe InstanceGroupStateChangeReasonCode)
    , _igscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroupStateChangeReason' smart constructor.
instanceGroupStateChangeReason :: InstanceGroupStateChangeReason
instanceGroupStateChangeReason =
    InstanceGroupStateChangeReason'
    { _igscrCode = Nothing
    , _igscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
igscrCode :: Lens' InstanceGroupStateChangeReason (Maybe InstanceGroupStateChangeReasonCode)
igscrCode = lens _igscrCode (\ s a -> s{_igscrCode = a});

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage = lens _igscrMessage (\ s a -> s{_igscrMessage = a});

instance FromJSON InstanceGroupStateChangeReason
         where
        parseJSON
          = withObject "InstanceGroupStateChangeReason"
              (\ x ->
                 InstanceGroupStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

-- | The details of the instance group status.
--
-- /See:/ 'instanceGroupStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igsState'
--
-- * 'igsStateChangeReason'
--
-- * 'igsTimeline'
data InstanceGroupStatus = InstanceGroupStatus'
    { _igsState             :: !(Maybe InstanceGroupState)
    , _igsStateChangeReason :: !(Maybe InstanceGroupStateChangeReason)
    , _igsTimeline          :: !(Maybe InstanceGroupTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroupStatus' smart constructor.
instanceGroupStatus :: InstanceGroupStatus
instanceGroupStatus =
    InstanceGroupStatus'
    { _igsState = Nothing
    , _igsStateChangeReason = Nothing
    , _igsTimeline = Nothing
    }

-- | The current state of the instance group.
igsState :: Lens' InstanceGroupStatus (Maybe InstanceGroupState)
igsState = lens _igsState (\ s a -> s{_igsState = a});

-- | The status change reason details for the instance group.
igsStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
igsStateChangeReason = lens _igsStateChangeReason (\ s a -> s{_igsStateChangeReason = a});

-- | The timeline of the instance group status over time.
igsTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
igsTimeline = lens _igsTimeline (\ s a -> s{_igsTimeline = a});

instance FromJSON InstanceGroupStatus where
        parseJSON
          = withObject "InstanceGroupStatus"
              (\ x ->
                 InstanceGroupStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

-- | The timeline of the instance group lifecycle.
--
-- /See:/ 'instanceGroupTimeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igtReadyDateTime'
--
-- * 'igtCreationDateTime'
--
-- * 'igtEndDateTime'
data InstanceGroupTimeline = InstanceGroupTimeline'
    { _igtReadyDateTime    :: !(Maybe POSIX)
    , _igtCreationDateTime :: !(Maybe POSIX)
    , _igtEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceGroupTimeline' smart constructor.
instanceGroupTimeline :: InstanceGroupTimeline
instanceGroupTimeline =
    InstanceGroupTimeline'
    { _igtReadyDateTime = Nothing
    , _igtCreationDateTime = Nothing
    , _igtEndDateTime = Nothing
    }

-- | The date and time when the instance group became ready to perform tasks.
igtReadyDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtReadyDateTime = lens _igtReadyDateTime (\ s a -> s{_igtReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the instance group.
igtCreationDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtCreationDateTime = lens _igtCreationDateTime (\ s a -> s{_igtCreationDateTime = a}) . mapping _Time;

-- | The date and time when the instance group terminated.
igtEndDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtEndDateTime = lens _igtEndDateTime (\ s a -> s{_igtEndDateTime = a}) . mapping _Time;

instance FromJSON InstanceGroupTimeline where
        parseJSON
          = withObject "InstanceGroupTimeline"
              (\ x ->
                 InstanceGroupTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

-- | The details of the status change reason for the instance.
--
-- /See:/ 'instanceStateChangeReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iscrCode'
--
-- * 'iscrMessage'
data InstanceStateChangeReason = InstanceStateChangeReason'
    { _iscrCode    :: !(Maybe InstanceStateChangeReasonCode)
    , _iscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceStateChangeReason' smart constructor.
instanceStateChangeReason :: InstanceStateChangeReason
instanceStateChangeReason =
    InstanceStateChangeReason'
    { _iscrCode = Nothing
    , _iscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe InstanceStateChangeReasonCode)
iscrCode = lens _iscrCode (\ s a -> s{_iscrCode = a});

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\ s a -> s{_iscrMessage = a});

instance FromJSON InstanceStateChangeReason where
        parseJSON
          = withObject "InstanceStateChangeReason"
              (\ x ->
                 InstanceStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

-- | The instance status details.
--
-- /See:/ 'instanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isState'
--
-- * 'isStateChangeReason'
--
-- * 'isTimeline'
data InstanceStatus = InstanceStatus'
    { _isState             :: !(Maybe InstanceState)
    , _isStateChangeReason :: !(Maybe InstanceStateChangeReason)
    , _isTimeline          :: !(Maybe InstanceTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceStatus' smart constructor.
instanceStatus :: InstanceStatus
instanceStatus =
    InstanceStatus'
    { _isState = Nothing
    , _isStateChangeReason = Nothing
    , _isTimeline = Nothing
    }

-- | The current state of the instance.
isState :: Lens' InstanceStatus (Maybe InstanceState)
isState = lens _isState (\ s a -> s{_isState = a});

-- | The details of the status change reason for the instance.
isStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
isStateChangeReason = lens _isStateChangeReason (\ s a -> s{_isStateChangeReason = a});

-- | The timeline of the instance status over time.
isTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
isTimeline = lens _isTimeline (\ s a -> s{_isTimeline = a});

instance FromJSON InstanceStatus where
        parseJSON
          = withObject "InstanceStatus"
              (\ x ->
                 InstanceStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

-- | The timeline of the instance lifecycle.
--
-- /See:/ 'instanceTimeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'itReadyDateTime'
--
-- * 'itCreationDateTime'
--
-- * 'itEndDateTime'
data InstanceTimeline = InstanceTimeline'
    { _itReadyDateTime    :: !(Maybe POSIX)
    , _itCreationDateTime :: !(Maybe POSIX)
    , _itEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceTimeline' smart constructor.
instanceTimeline :: InstanceTimeline
instanceTimeline =
    InstanceTimeline'
    { _itReadyDateTime = Nothing
    , _itCreationDateTime = Nothing
    , _itEndDateTime = Nothing
    }

-- | The date and time when the instance was ready to perform tasks.
itReadyDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itReadyDateTime = lens _itReadyDateTime (\ s a -> s{_itReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the instance.
itCreationDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itCreationDateTime = lens _itCreationDateTime (\ s a -> s{_itCreationDateTime = a}) . mapping _Time;

-- | The date and time when the instance was terminated.
itEndDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itEndDateTime = lens _itEndDateTime (\ s a -> s{_itEndDateTime = a}) . mapping _Time;

instance FromJSON InstanceTimeline where
        parseJSON
          = withObject "InstanceTimeline"
              (\ x ->
                 InstanceTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

-- | A description of the Amazon EC2 instance running the job flow. A valid
-- JobFlowInstancesConfig must contain at least InstanceGroups, which is
-- the recommended configuration. However, a valid alternative is to have
-- MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must
-- be present).
--
-- /See:/ 'jobFlowInstancesConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jficSlaveInstanceType'
--
-- * 'jficEC2KeyName'
--
-- * 'jficInstanceCount'
--
-- * 'jficEmrManagedSlaveSecurityGroup'
--
-- * 'jficAdditionalSlaveSecurityGroups'
--
-- * 'jficHadoopVersion'
--
-- * 'jficAdditionalMasterSecurityGroups'
--
-- * 'jficEmrManagedMasterSecurityGroup'
--
-- * 'jficEC2SubnetId'
--
-- * 'jficMasterInstanceType'
--
-- * 'jficInstanceGroups'
--
-- * 'jficKeepJobFlowAliveWhenNoSteps'
--
-- * 'jficTerminationProtected'
--
-- * 'jficPlacement'
data JobFlowInstancesConfig = JobFlowInstancesConfig'
    { _jficSlaveInstanceType              :: !(Maybe Text)
    , _jficEC2KeyName                     :: !(Maybe Text)
    , _jficInstanceCount                  :: !(Maybe Int)
    , _jficEmrManagedSlaveSecurityGroup   :: !(Maybe Text)
    , _jficAdditionalSlaveSecurityGroups  :: !(Maybe [Text])
    , _jficHadoopVersion                  :: !(Maybe Text)
    , _jficAdditionalMasterSecurityGroups :: !(Maybe [Text])
    , _jficEmrManagedMasterSecurityGroup  :: !(Maybe Text)
    , _jficEC2SubnetId                    :: !(Maybe Text)
    , _jficMasterInstanceType             :: !(Maybe Text)
    , _jficInstanceGroups                 :: !(Maybe [InstanceGroupConfig])
    , _jficKeepJobFlowAliveWhenNoSteps    :: !(Maybe Bool)
    , _jficTerminationProtected           :: !(Maybe Bool)
    , _jficPlacement                      :: !(Maybe PlacementType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'JobFlowInstancesConfig' smart constructor.
jobFlowInstancesConfig :: JobFlowInstancesConfig
jobFlowInstancesConfig =
    JobFlowInstancesConfig'
    { _jficSlaveInstanceType = Nothing
    , _jficEC2KeyName = Nothing
    , _jficInstanceCount = Nothing
    , _jficEmrManagedSlaveSecurityGroup = Nothing
    , _jficAdditionalSlaveSecurityGroups = Nothing
    , _jficHadoopVersion = Nothing
    , _jficAdditionalMasterSecurityGroups = Nothing
    , _jficEmrManagedMasterSecurityGroup = Nothing
    , _jficEC2SubnetId = Nothing
    , _jficMasterInstanceType = Nothing
    , _jficInstanceGroups = Nothing
    , _jficKeepJobFlowAliveWhenNoSteps = Nothing
    , _jficTerminationProtected = Nothing
    , _jficPlacement = Nothing
    }

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType = lens _jficSlaveInstanceType (\ s a -> s{_jficSlaveInstanceType = a});

-- | The name of the Amazon EC2 key pair that can be used to ssh to the
-- master node as the user called \"hadoop.\"
jficEC2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEC2KeyName = lens _jficEC2KeyName (\ s a -> s{_jficEC2KeyName = a});

-- | The number of Amazon EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Int)
jficInstanceCount = lens _jficInstanceCount (\ s a -> s{_jficInstanceCount = a});

-- | The identifier of the Amazon EC2 security group (managed by Amazon
-- ElasticMapReduce) for the slave nodes.
jficEmrManagedSlaveSecurityGroup :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEmrManagedSlaveSecurityGroup = lens _jficEmrManagedSlaveSecurityGroup (\ s a -> s{_jficEmrManagedSlaveSecurityGroup = a});

-- | A list of additional Amazon EC2 security group IDs for the slave nodes.
jficAdditionalSlaveSecurityGroups :: Lens' JobFlowInstancesConfig [Text]
jficAdditionalSlaveSecurityGroups = lens _jficAdditionalSlaveSecurityGroups (\ s a -> s{_jficAdditionalSlaveSecurityGroups = a}) . _Default;

-- | The Hadoop version for the job flow. Valid inputs are \"0.18\",
-- \"0.20\", \"0.20.205\", \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do
-- not set this value, the default of 0.18 is used, unless the AmiVersion
-- parameter is set in the RunJobFlow call, in which case the default
-- version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion = lens _jficHadoopVersion (\ s a -> s{_jficHadoopVersion = a});

-- | A list of additional Amazon EC2 security group IDs for the master node.
jficAdditionalMasterSecurityGroups :: Lens' JobFlowInstancesConfig [Text]
jficAdditionalMasterSecurityGroups = lens _jficAdditionalMasterSecurityGroups (\ s a -> s{_jficAdditionalMasterSecurityGroups = a}) . _Default;

-- | The identifier of the Amazon EC2 security group (managed by Amazon
-- ElasticMapReduce) for the master node.
jficEmrManagedMasterSecurityGroup :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEmrManagedMasterSecurityGroup = lens _jficEmrManagedMasterSecurityGroup (\ s a -> s{_jficEmrManagedMasterSecurityGroup = a});

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the job flow to launch. If you do not specify this value, the job flow
-- is launched in the normal Amazon Web Services cloud, outside of an
-- Amazon VPC.
--
-- Amazon VPC currently does not support cluster compute quadruple extra
-- large (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge
-- instance type for nodes of a job flow launched in a Amazon VPC.
jficEC2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEC2SubnetId = lens _jficEC2SubnetId (\ s a -> s{_jficEC2SubnetId = a});

-- | The EC2 instance type of the master node.
jficMasterInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficMasterInstanceType = lens _jficMasterInstanceType (\ s a -> s{_jficMasterInstanceType = a});

-- | Configuration for the job flow\'s instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig [InstanceGroupConfig]
jficInstanceGroups = lens _jficInstanceGroups (\ s a -> s{_jficInstanceGroups = a}) . _Default;

-- | Specifies whether the job flow should terminate after completing all
-- steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps = lens _jficKeepJobFlowAliveWhenNoSteps (\ s a -> s{_jficKeepJobFlowAliveWhenNoSteps = a});

-- | Specifies whether to lock the job flow to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected = lens _jficTerminationProtected (\ s a -> s{_jficTerminationProtected = a});

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement = lens _jficPlacement (\ s a -> s{_jficPlacement = a});

instance ToJSON JobFlowInstancesConfig where
        toJSON JobFlowInstancesConfig'{..}
          = object
              ["SlaveInstanceType" .= _jficSlaveInstanceType,
               "Ec2KeyName" .= _jficEC2KeyName,
               "InstanceCount" .= _jficInstanceCount,
               "EmrManagedSlaveSecurityGroup" .=
                 _jficEmrManagedSlaveSecurityGroup,
               "AdditionalSlaveSecurityGroups" .=
                 _jficAdditionalSlaveSecurityGroups,
               "HadoopVersion" .= _jficHadoopVersion,
               "AdditionalMasterSecurityGroups" .=
                 _jficAdditionalMasterSecurityGroups,
               "EmrManagedMasterSecurityGroup" .=
                 _jficEmrManagedMasterSecurityGroup,
               "Ec2SubnetId" .= _jficEC2SubnetId,
               "MasterInstanceType" .= _jficMasterInstanceType,
               "InstanceGroups" .= _jficInstanceGroups,
               "KeepJobFlowAliveWhenNoSteps" .=
                 _jficKeepJobFlowAliveWhenNoSteps,
               "TerminationProtected" .= _jficTerminationProtected,
               "Placement" .= _jficPlacement]

-- | A key value pair.
--
-- /See:/ 'keyValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kvValue'
--
-- * 'kvKey'
data KeyValue = KeyValue'
    { _kvValue :: !(Maybe Text)
    , _kvKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'KeyValue' smart constructor.
keyValue :: KeyValue
keyValue =
    KeyValue'
    { _kvValue = Nothing
    , _kvKey = Nothing
    }

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue = lens _kvValue (\ s a -> s{_kvValue = a});

-- | The unique identifier of a key value pair.
kvKey :: Lens' KeyValue (Maybe Text)
kvKey = lens _kvKey (\ s a -> s{_kvKey = a});

instance ToJSON KeyValue where
        toJSON KeyValue'{..}
          = object ["Value" .= _kvValue, "Key" .= _kvKey]

-- | The Amazon EC2 location for the job flow.
--
-- /See:/ 'placementType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptAvailabilityZone'
newtype PlacementType = PlacementType'
    { _ptAvailabilityZone :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PlacementType' smart constructor.
placementType :: Text -> PlacementType
placementType pAvailabilityZone =
    PlacementType'
    { _ptAvailabilityZone = pAvailabilityZone
    }

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType Text
ptAvailabilityZone = lens _ptAvailabilityZone (\ s a -> s{_ptAvailabilityZone = a});

instance ToJSON PlacementType where
        toJSON PlacementType'{..}
          = object ["AvailabilityZone" .= _ptAvailabilityZone]

-- | Configuration of the script to run during a bootstrap action.
--
-- /See:/ 'scriptBootstrapActionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sbacArgs'
--
-- * 'sbacPath'
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
    { _sbacArgs :: !(Maybe [Text])
    , _sbacPath :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ScriptBootstrapActionConfig' smart constructor.
scriptBootstrapActionConfig :: Text -> ScriptBootstrapActionConfig
scriptBootstrapActionConfig pPath =
    ScriptBootstrapActionConfig'
    { _sbacArgs = Nothing
    , _sbacPath = pPath
    }

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig [Text]
sbacArgs = lens _sbacArgs (\ s a -> s{_sbacArgs = a}) . _Default;

-- | Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig Text
sbacPath = lens _sbacPath (\ s a -> s{_sbacPath = a});

instance ToJSON ScriptBootstrapActionConfig where
        toJSON ScriptBootstrapActionConfig'{..}
          = object ["Args" .= _sbacArgs, "Path" .= _sbacPath]

-- | This represents a step in a cluster.
--
-- /See:/ 'step' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'steStatus'
--
-- * 'steActionOnFailure'
--
-- * 'steConfig'
--
-- * 'steName'
--
-- * 'steId'
data Step = Step'
    { _steStatus          :: !(Maybe StepStatus)
    , _steActionOnFailure :: !(Maybe ActionOnFailure)
    , _steConfig          :: !(Maybe HadoopStepConfig)
    , _steName            :: !(Maybe Text)
    , _steId              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Step' smart constructor.
step :: Step
step =
    Step'
    { _steStatus = Nothing
    , _steActionOnFailure = Nothing
    , _steConfig = Nothing
    , _steName = Nothing
    , _steId = Nothing
    }

-- | The current execution status details of the cluster step.
steStatus :: Lens' Step (Maybe StepStatus)
steStatus = lens _steStatus (\ s a -> s{_steStatus = a});

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
steActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
steActionOnFailure = lens _steActionOnFailure (\ s a -> s{_steActionOnFailure = a});

-- | The Hadoop job configuration of the cluster step.
steConfig :: Lens' Step (Maybe HadoopStepConfig)
steConfig = lens _steConfig (\ s a -> s{_steConfig = a});

-- | The name of the cluster step.
steName :: Lens' Step (Maybe Text)
steName = lens _steName (\ s a -> s{_steName = a});

-- | The identifier of the cluster step.
steId :: Lens' Step (Maybe Text)
steId = lens _steId (\ s a -> s{_steId = a});

instance FromJSON Step where
        parseJSON
          = withObject "Step"
              (\ x ->
                 Step' <$>
                   (x .:? "Status") <*> (x .:? "ActionOnFailure") <*>
                     (x .:? "Config")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

-- | Specification of a job flow step.
--
-- /See:/ 'stepConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scActionOnFailure'
--
-- * 'scName'
--
-- * 'scHadoopJARStep'
data StepConfig = StepConfig'
    { _scActionOnFailure :: !(Maybe ActionOnFailure)
    , _scName            :: !Text
    , _scHadoopJARStep   :: !HadoopJARStepConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StepConfig' smart constructor.
stepConfig :: Text -> HadoopJARStepConfig -> StepConfig
stepConfig pName pHadoopJARStep =
    StepConfig'
    { _scActionOnFailure = Nothing
    , _scName = pName
    , _scHadoopJARStep = pHadoopJARStep
    }

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure = lens _scActionOnFailure (\ s a -> s{_scActionOnFailure = a});

-- | The name of the job flow step.
scName :: Lens' StepConfig Text
scName = lens _scName (\ s a -> s{_scName = a});

-- | The JAR file used for the job flow step.
scHadoopJARStep :: Lens' StepConfig HadoopJARStepConfig
scHadoopJARStep = lens _scHadoopJARStep (\ s a -> s{_scHadoopJARStep = a});

instance ToJSON StepConfig where
        toJSON StepConfig'{..}
          = object
              ["ActionOnFailure" .= _scActionOnFailure,
               "Name" .= _scName,
               "HadoopJarStep" .= _scHadoopJARStep]

-- | The details of the step state change reason.
--
-- /See:/ 'stepStateChangeReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sscrCode'
--
-- * 'sscrMessage'
data StepStateChangeReason = StepStateChangeReason'
    { _sscrCode    :: !(Maybe StepStateChangeReasonCode)
    , _sscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StepStateChangeReason' smart constructor.
stepStateChangeReason :: StepStateChangeReason
stepStateChangeReason =
    StepStateChangeReason'
    { _sscrCode = Nothing
    , _sscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
sscrCode :: Lens' StepStateChangeReason (Maybe StepStateChangeReasonCode)
sscrCode = lens _sscrCode (\ s a -> s{_sscrCode = a});

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage = lens _sscrMessage (\ s a -> s{_sscrMessage = a});

instance FromJSON StepStateChangeReason where
        parseJSON
          = withObject "StepStateChangeReason"
              (\ x ->
                 StepStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

-- | The execution status details of the cluster step.
--
-- /See:/ 'stepStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssState'
--
-- * 'ssStateChangeReason'
--
-- * 'ssTimeline'
data StepStatus = StepStatus'
    { _ssState             :: !(Maybe StepState)
    , _ssStateChangeReason :: !(Maybe StepStateChangeReason)
    , _ssTimeline          :: !(Maybe StepTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StepStatus' smart constructor.
stepStatus :: StepStatus
stepStatus =
    StepStatus'
    { _ssState = Nothing
    , _ssStateChangeReason = Nothing
    , _ssTimeline = Nothing
    }

-- | The execution state of the cluster step.
ssState :: Lens' StepStatus (Maybe StepState)
ssState = lens _ssState (\ s a -> s{_ssState = a});

-- | The reason for the step execution status change.
ssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
ssStateChangeReason = lens _ssStateChangeReason (\ s a -> s{_ssStateChangeReason = a});

-- | The timeline of the cluster step status over time.
ssTimeline :: Lens' StepStatus (Maybe StepTimeline)
ssTimeline = lens _ssTimeline (\ s a -> s{_ssTimeline = a});

instance FromJSON StepStatus where
        parseJSON
          = withObject "StepStatus"
              (\ x ->
                 StepStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

-- | The summary of the cluster step.
--
-- /See:/ 'stepSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssStatus'
--
-- * 'ssActionOnFailure'
--
-- * 'ssConfig'
--
-- * 'ssName'
--
-- * 'ssId'
data StepSummary = StepSummary'
    { _ssStatus          :: !(Maybe StepStatus)
    , _ssActionOnFailure :: !(Maybe ActionOnFailure)
    , _ssConfig          :: !(Maybe HadoopStepConfig)
    , _ssName            :: !(Maybe Text)
    , _ssId              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StepSummary' smart constructor.
stepSummary :: StepSummary
stepSummary =
    StepSummary'
    { _ssStatus = Nothing
    , _ssActionOnFailure = Nothing
    , _ssConfig = Nothing
    , _ssName = Nothing
    , _ssId = Nothing
    }

-- | The current execution status details of the cluster step.
ssStatus :: Lens' StepSummary (Maybe StepStatus)
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a});

-- | This specifies what action to take when the cluster step fails. Possible
-- values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
ssActionOnFailure :: Lens' StepSummary (Maybe ActionOnFailure)
ssActionOnFailure = lens _ssActionOnFailure (\ s a -> s{_ssActionOnFailure = a});

-- | The Hadoop job configuration of the cluster step.
ssConfig :: Lens' StepSummary (Maybe HadoopStepConfig)
ssConfig = lens _ssConfig (\ s a -> s{_ssConfig = a});

-- | The name of the cluster step.
ssName :: Lens' StepSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The identifier of the cluster step.
ssId :: Lens' StepSummary (Maybe Text)
ssId = lens _ssId (\ s a -> s{_ssId = a});

instance FromJSON StepSummary where
        parseJSON
          = withObject "StepSummary"
              (\ x ->
                 StepSummary' <$>
                   (x .:? "Status") <*> (x .:? "ActionOnFailure") <*>
                     (x .:? "Config")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'stepTimeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stCreationDateTime'
--
-- * 'stEndDateTime'
--
-- * 'stStartDateTime'
data StepTimeline = StepTimeline'
    { _stCreationDateTime :: !(Maybe POSIX)
    , _stEndDateTime      :: !(Maybe POSIX)
    , _stStartDateTime    :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StepTimeline' smart constructor.
stepTimeline :: StepTimeline
stepTimeline =
    StepTimeline'
    { _stCreationDateTime = Nothing
    , _stEndDateTime = Nothing
    , _stStartDateTime = Nothing
    }

-- | The date and time when the cluster step was created.
stCreationDateTime :: Lens' StepTimeline (Maybe UTCTime)
stCreationDateTime = lens _stCreationDateTime (\ s a -> s{_stCreationDateTime = a}) . mapping _Time;

-- | The date and time when the cluster step execution completed or failed.
stEndDateTime :: Lens' StepTimeline (Maybe UTCTime)
stEndDateTime = lens _stEndDateTime (\ s a -> s{_stEndDateTime = a}) . mapping _Time;

-- | The date and time when the cluster step execution started.
stStartDateTime :: Lens' StepTimeline (Maybe UTCTime)
stStartDateTime = lens _stStartDateTime (\ s a -> s{_stStartDateTime = a}) . mapping _Time;

instance FromJSON StepTimeline where
        parseJSON
          = withObject "StepTimeline"
              (\ x ->
                 StepTimeline' <$>
                   (x .:? "CreationDateTime") <*> (x .:? "EndDateTime")
                     <*> (x .:? "StartDateTime"))

-- | The list of supported product configurations which allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
--
-- /See:/ 'supportedProductConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spcArgs'
--
-- * 'spcName'
data SupportedProductConfig = SupportedProductConfig'
    { _spcArgs :: !(Maybe [Text])
    , _spcName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SupportedProductConfig' smart constructor.
supportedProductConfig :: SupportedProductConfig
supportedProductConfig =
    SupportedProductConfig'
    { _spcArgs = Nothing
    , _spcName = Nothing
    }

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig [Text]
spcArgs = lens _spcArgs (\ s a -> s{_spcArgs = a}) . _Default;

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\ s a -> s{_spcName = a});

instance ToJSON SupportedProductConfig where
        toJSON SupportedProductConfig'{..}
          = object ["Args" .= _spcArgs, "Name" .= _spcName]

-- | A key\/value pair containing user-defined metadata that you can
-- associate with an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clu\\ sters to track your
-- Amazon EMR resource allocation costs. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | A user-defined value, which is optional in a tag. For more information,
-- see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A user-defined key, which is the minimum required information for a
-- valid tag. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object ["Value" .= _tagValue, "Key" .= _tagKey]
