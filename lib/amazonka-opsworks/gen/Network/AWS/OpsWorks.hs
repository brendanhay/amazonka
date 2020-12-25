{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS OpsWorks__
--
-- Welcome to the /AWS OpsWorks Stacks API Reference/ . This guide provides descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions and data types, including common parameters and error codes.
-- AWS OpsWorks Stacks is an application management service that provides an integrated experience for overseeing the complete application lifecycle. For information about this product, go to the <http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.
-- __SDKs and CLI__
-- The most common way to use the AWS OpsWorks Stacks API is by using the AWS Command Line Interface (CLI) or by using one of the AWS SDKs to implement applications in your preferred language. For more information, see:
--
--     * <https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
--
--
--     * <https://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
--
--
--     * <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
--
--
--     * <https://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
--
--
--     * <http://docs.aws.amazon.com/sdkforruby/api/ AWS SDK for Ruby>
--
--
--     * <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
--
--
--     * <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>
--
--
-- __Endpoints__
-- AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Stacks can only be accessed or managed within the endpoint in which they are created.
--
--     * opsworks.us-east-1.amazonaws.com
--
--
--     * opsworks.us-east-2.amazonaws.com
--
--
--     * opsworks.us-west-1.amazonaws.com
--
--
--     * opsworks.us-west-2.amazonaws.com
--
--
--     * opsworks.ca-central-1.amazonaws.com (API only; not available in the AWS console)
--
--
--     * opsworks.eu-west-1.amazonaws.com
--
--
--     * opsworks.eu-west-2.amazonaws.com
--
--
--     * opsworks.eu-west-3.amazonaws.com
--
--
--     * opsworks.eu-central-1.amazonaws.com
--
--
--     * opsworks.ap-northeast-1.amazonaws.com
--
--
--     * opsworks.ap-northeast-2.amazonaws.com
--
--
--     * opsworks.ap-south-1.amazonaws.com
--
--
--     * opsworks.ap-southeast-1.amazonaws.com
--
--
--     * opsworks.ap-southeast-2.amazonaws.com
--
--
--     * opsworks.sa-east-1.amazonaws.com
--
--
-- __Chef Versions__
-- When you call 'CreateStack' , 'CloneStack' , or 'UpdateStack' we recommend you use the @ConfigurationManager@ parameter to specify the Chef version. The recommended and default value for Linux stacks is currently 12. Windows stacks use Chef 12.2. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions> .
module Network.AWS.OpsWorks
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    mkInstanceTerminated,

    -- ** DeploymentSuccessful
    mkDeploymentSuccessful,

    -- ** InstanceStopped
    mkInstanceStopped,

    -- ** InstanceOnline
    mkInstanceOnline,

    -- ** AppExists
    mkAppExists,

    -- ** InstanceRegistered
    mkInstanceRegistered,

    -- * Operations
    -- $operations

    -- ** DescribeRdsDbInstances
    module Network.AWS.OpsWorks.DescribeRdsDbInstances,

    -- ** DeleteStack
    module Network.AWS.OpsWorks.DeleteStack,

    -- ** UpdateStack
    module Network.AWS.OpsWorks.UpdateStack,

    -- ** CreateLayer
    module Network.AWS.OpsWorks.CreateLayer,

    -- ** SetLoadBasedAutoScaling
    module Network.AWS.OpsWorks.SetLoadBasedAutoScaling,

    -- ** DeregisterRdsDbInstance
    module Network.AWS.OpsWorks.DeregisterRdsDbInstance,

    -- ** UnassignVolume
    module Network.AWS.OpsWorks.UnassignVolume,

    -- ** CreateInstance
    module Network.AWS.OpsWorks.CreateInstance,

    -- ** DescribeLayers
    module Network.AWS.OpsWorks.DescribeLayers,

    -- ** RegisterElasticIp
    module Network.AWS.OpsWorks.RegisterElasticIp,

    -- ** DescribeAgentVersions
    module Network.AWS.OpsWorks.DescribeAgentVersions,

    -- ** CreateDeployment
    module Network.AWS.OpsWorks.CreateDeployment,

    -- ** AssignInstance
    module Network.AWS.OpsWorks.AssignInstance,

    -- ** DescribeStacks
    module Network.AWS.OpsWorks.DescribeStacks,

    -- ** DeleteInstance
    module Network.AWS.OpsWorks.DeleteInstance,

    -- ** UpdateInstance
    module Network.AWS.OpsWorks.UpdateInstance,

    -- ** DeregisterVolume
    module Network.AWS.OpsWorks.DeregisterVolume,

    -- ** RebootInstance
    module Network.AWS.OpsWorks.RebootInstance,

    -- ** DeleteApp
    module Network.AWS.OpsWorks.DeleteApp,

    -- ** UpdateApp
    module Network.AWS.OpsWorks.UpdateApp,

    -- ** UpdateRdsDbInstance
    module Network.AWS.OpsWorks.UpdateRdsDbInstance,

    -- ** DescribeTimeBasedAutoScaling
    module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling,

    -- ** StopStack
    module Network.AWS.OpsWorks.StopStack,

    -- ** DescribeVolumes
    module Network.AWS.OpsWorks.DescribeVolumes,

    -- ** DisassociateElasticIp
    module Network.AWS.OpsWorks.DisassociateElasticIp,

    -- ** RegisterEcsCluster
    module Network.AWS.OpsWorks.RegisterEcsCluster,

    -- ** StopInstance
    module Network.AWS.OpsWorks.StopInstance,

    -- ** RegisterVolume
    module Network.AWS.OpsWorks.RegisterVolume,

    -- ** SetTimeBasedAutoScaling
    module Network.AWS.OpsWorks.SetTimeBasedAutoScaling,

    -- ** DescribeUserProfiles
    module Network.AWS.OpsWorks.DescribeUserProfiles,

    -- ** AttachElasticLoadBalancer
    module Network.AWS.OpsWorks.AttachElasticLoadBalancer,

    -- ** DeregisterElasticIp
    module Network.AWS.OpsWorks.DeregisterElasticIp,

    -- ** DeregisterEcsCluster
    module Network.AWS.OpsWorks.DeregisterEcsCluster,

    -- ** DescribeApps
    module Network.AWS.OpsWorks.DescribeApps,

    -- ** UpdateMyUserProfile
    module Network.AWS.OpsWorks.UpdateMyUserProfile,

    -- ** DescribeStackSummary
    module Network.AWS.OpsWorks.DescribeStackSummary,

    -- ** DescribeInstances
    module Network.AWS.OpsWorks.DescribeInstances,

    -- ** DescribeDeployments
    module Network.AWS.OpsWorks.DescribeDeployments,

    -- ** DescribeElasticIps
    module Network.AWS.OpsWorks.DescribeElasticIps,

    -- ** GrantAccess
    module Network.AWS.OpsWorks.GrantAccess,

    -- ** DeleteLayer
    module Network.AWS.OpsWorks.DeleteLayer,

    -- ** UpdateLayer
    module Network.AWS.OpsWorks.UpdateLayer,

    -- ** CreateStack
    module Network.AWS.OpsWorks.CreateStack,

    -- ** UpdateElasticIp
    module Network.AWS.OpsWorks.UpdateElasticIp,

    -- ** CreateApp
    module Network.AWS.OpsWorks.CreateApp,

    -- ** GetHostnameSuggestion
    module Network.AWS.OpsWorks.GetHostnameSuggestion,

    -- ** CloneStack
    module Network.AWS.OpsWorks.CloneStack,

    -- ** DescribePermissions
    module Network.AWS.OpsWorks.DescribePermissions,

    -- ** DetachElasticLoadBalancer
    module Network.AWS.OpsWorks.DetachElasticLoadBalancer,

    -- ** RegisterInstance
    module Network.AWS.OpsWorks.RegisterInstance,

    -- ** AssociateElasticIp
    module Network.AWS.OpsWorks.AssociateElasticIp,

    -- ** DescribeLoadBasedAutoScaling
    module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling,

    -- ** DescribeStackProvisioningParameters
    module Network.AWS.OpsWorks.DescribeStackProvisioningParameters,

    -- ** TagResource
    module Network.AWS.OpsWorks.TagResource,

    -- ** ListTags
    module Network.AWS.OpsWorks.ListTags,

    -- ** UnassignInstance
    module Network.AWS.OpsWorks.UnassignInstance,

    -- ** DescribeMyUserProfile
    module Network.AWS.OpsWorks.DescribeMyUserProfile,

    -- ** DeleteUserProfile
    module Network.AWS.OpsWorks.DeleteUserProfile,

    -- ** UpdateUserProfile
    module Network.AWS.OpsWorks.UpdateUserProfile,

    -- ** DescribeServiceErrors
    module Network.AWS.OpsWorks.DescribeServiceErrors,

    -- ** RegisterRdsDbInstance
    module Network.AWS.OpsWorks.RegisterRdsDbInstance,

    -- ** UntagResource
    module Network.AWS.OpsWorks.UntagResource,

    -- ** StartStack
    module Network.AWS.OpsWorks.StartStack,

    -- ** CreateUserProfile
    module Network.AWS.OpsWorks.CreateUserProfile,

    -- ** DescribeOperatingSystems
    module Network.AWS.OpsWorks.DescribeOperatingSystems,

    -- ** DescribeCommands
    module Network.AWS.OpsWorks.DescribeCommands,

    -- ** AssignVolume
    module Network.AWS.OpsWorks.AssignVolume,

    -- ** DescribeElasticLoadBalancers
    module Network.AWS.OpsWorks.DescribeElasticLoadBalancers,

    -- ** SetPermission
    module Network.AWS.OpsWorks.SetPermission,

    -- ** DeregisterInstance
    module Network.AWS.OpsWorks.DeregisterInstance,

    -- ** DescribeEcsClusters (Paginated)
    module Network.AWS.OpsWorks.DescribeEcsClusters,

    -- ** DescribeRaidArrays
    module Network.AWS.OpsWorks.DescribeRaidArrays,

    -- ** UpdateVolume
    module Network.AWS.OpsWorks.UpdateVolume,

    -- ** StartInstance
    module Network.AWS.OpsWorks.StartInstance,

    -- * Types

    -- ** SslConfiguration
    SslConfiguration (..),
    mkSslConfiguration,
    scCertificate,
    scPrivateKey,
    scChain,

    -- ** VirtualizationType
    VirtualizationType (..),

    -- ** Command
    Command (..),
    mkCommand,
    cAcknowledgedAt,
    cCommandId,
    cCompletedAt,
    cCreatedAt,
    cDeploymentId,
    cExitCode,
    cInstanceId,
    cLogUrl,
    cStatus,
    cType,

    -- ** RaidArray
    RaidArray (..),
    mkRaidArray,
    raAvailabilityZone,
    raCreatedAt,
    raDevice,
    raInstanceId,
    raIops,
    raMountPoint,
    raName,
    raNumberOfDisks,
    raRaidArrayId,
    raRaidLevel,
    raSize,
    raStackId,
    raVolumeType,

    -- ** ElasticLoadBalancer
    ElasticLoadBalancer (..),
    mkElasticLoadBalancer,
    elbAvailabilityZones,
    elbDnsName,
    elbEc2InstanceIds,
    elbElasticLoadBalancerName,
    elbLayerId,
    elbRegion,
    elbStackId,
    elbSubnetIds,
    elbVpcId,

    -- ** CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    mkCloudWatchLogsConfiguration,
    cwlcEnabled,
    cwlcLogStreams,

    -- ** Switch
    Switch (..),

    -- ** LifecycleEventConfiguration
    LifecycleEventConfiguration (..),
    mkLifecycleEventConfiguration,
    lecShutdown,

    -- ** RdsDbInstance
    RdsDbInstance (..),
    mkRdsDbInstance,
    rdiAddress,
    rdiDbInstanceIdentifier,
    rdiDbPassword,
    rdiDbUser,
    rdiEngine,
    rdiMissingOnRds,
    rdiRdsDbInstanceArn,
    rdiRegion,
    rdiStackId,

    -- ** AppAttributesKeys
    AppAttributesKeys (..),

    -- ** StackSummary
    StackSummary (..),
    mkStackSummary,
    ssAppsCount,
    ssArn,
    ssInstancesCount,
    ssLayersCount,
    ssName,
    ssStackId,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmDeviceName,
    bdmEbs,
    bdmNoDevice,
    bdmVirtualName,

    -- ** StackAttributesKeys
    StackAttributesKeys (..),

    -- ** LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (..),
    mkLoadBasedAutoScalingConfiguration,
    lbascDownScaling,
    lbascEnable,
    lbascLayerId,
    lbascUpScaling,

    -- ** String
    String (..),

    -- ** SourceType
    SourceType (..),

    -- ** OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osConfigurationManagers,
    osId,
    osName,
    osReportedName,
    osReportedVersion,
    osSupported,
    osType,

    -- ** Volume
    Volume (..),
    mkVolume,
    vAvailabilityZone,
    vDevice,
    vEc2VolumeId,
    vEncrypted,
    vInstanceId,
    vIops,
    vMountPoint,
    vName,
    vRaidArrayId,
    vRegion,
    vSize,
    vStatus,
    vVolumeId,
    vVolumeType,

    -- ** CloudWatchLogsInitialPosition
    CloudWatchLogsInitialPosition (..),

    -- ** ChefConfiguration
    ChefConfiguration (..),
    mkChefConfiguration,
    ccBerkshelfVersion,
    ccManageBerkshelf,

    -- ** AgentVersion
    AgentVersion (..),
    mkAgentVersion,
    avConfigurationManager,
    avVersion,

    -- ** LayerType
    LayerType (..),

    -- ** AutoScalingThresholds
    AutoScalingThresholds (..),
    mkAutoScalingThresholds,
    astAlarms,
    astCpuThreshold,
    astIgnoreMetricsTime,
    astInstanceCount,
    astLoadThreshold,
    astMemoryThreshold,
    astThresholdsWaitTime,

    -- ** TagValue
    TagValue (..),

    -- ** App
    App (..),
    mkApp,
    aAppId,
    aAppSource,
    aAttributes,
    aCreatedAt,
    aDataSources,
    aDescription,
    aDomains,
    aEnableSsl,
    aEnvironment,
    aName,
    aShortname,
    aSslConfiguration,
    aStackId,
    aType,

    -- ** ElasticIp
    ElasticIp (..),
    mkElasticIp,
    eiDomain,
    eiInstanceId,
    eiIp,
    eiName,
    eiRegion,

    -- ** CloudWatchLogsEncoding
    CloudWatchLogsEncoding (..),

    -- ** NextToken
    NextToken (..),

    -- ** ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    mkShutdownEventConfiguration,
    secDelayUntilElbConnectionsDrained,
    secExecutionTimeout,

    -- ** CloudWatchLogsTimeZone
    CloudWatchLogsTimeZone (..),

    -- ** CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    mkCloudWatchLogsLogStream,
    cwllsBatchCount,
    cwllsBatchSize,
    cwllsBufferDuration,
    cwllsDatetimeFormat,
    cwllsEncoding,
    cwllsFile,
    cwllsFileFingerprintLines,
    cwllsInitialPosition,
    cwllsLogGroupName,
    cwllsMultiLineStartPattern,
    cwllsTimeZone,

    -- ** ResourceArn
    ResourceArn (..),

    -- ** EcsCluster
    EcsCluster (..),
    mkEcsCluster,
    ecEcsClusterArn,
    ecEcsClusterName,
    ecRegisteredAt,
    ecStackId,

    -- ** InstanceIdentity
    InstanceIdentity (..),
    mkInstanceIdentity,
    iiDocument,
    iiSignature,

    -- ** UserProfile
    UserProfile (..),
    mkUserProfile,
    upAllowSelfManagement,
    upIamUserArn,
    upName,
    upSshPublicKey,
    upSshUsername,

    -- ** AutoScalingType
    AutoScalingType (..),

    -- ** Source
    Source (..),
    mkSource,
    sPassword,
    sRevision,
    sSshKey,
    sType,
    sUrl,
    sUsername,

    -- ** DataSource
    DataSource (..),
    mkDataSource,
    dsArn,
    dsDatabaseName,
    dsType,

    -- ** Architecture
    Architecture (..),

    -- ** StackConfigurationManager
    StackConfigurationManager (..),
    mkStackConfigurationManager,
    scmName,
    scmVersion,

    -- ** EbsBlockDevice
    EbsBlockDevice (..),
    mkEbsBlockDevice,
    ebdDeleteOnTermination,
    ebdIops,
    ebdSnapshotId,
    ebdVolumeSize,
    ebdVolumeType,

    -- ** ServiceError'
    ServiceError' (..),
    mkServiceError',
    seCreatedAt,
    seInstanceId,
    seMessage,
    seServiceErrorId,
    seStackId,
    seType,

    -- ** TagKey
    TagKey (..),

    -- ** Hour
    Hour (..),

    -- ** LayerAttributesKeys
    LayerAttributesKeys (..),

    -- ** TemporaryCredential
    TemporaryCredential (..),
    mkTemporaryCredential,
    tcInstanceId,
    tcPassword,
    tcUsername,
    tcValidForInMinutes,

    -- ** VolumeConfiguration
    VolumeConfiguration (..),
    mkVolumeConfiguration,
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,
    vcEncrypted,
    vcIops,
    vcRaidLevel,
    vcVolumeType,

    -- ** VolumeType
    VolumeType (..),

    -- ** ReportedOs
    ReportedOs (..),
    mkReportedOs,
    roFamily,
    roName,
    roVersion,

    -- ** Permission
    Permission (..),
    mkPermission,
    pAllowSsh,
    pAllowSudo,
    pIamUserArn,
    pLevel,
    pStackId,

    -- ** EnvironmentVariable
    EnvironmentVariable (..),
    mkEnvironmentVariable,
    evKey,
    evValue,
    evSecure,

    -- ** OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    mkOperatingSystemConfigurationManager,
    oscmName,
    oscmVersion,

    -- ** Layer
    Layer (..),
    mkLayer,
    lArn,
    lAttributes,
    lAutoAssignElasticIps,
    lAutoAssignPublicIps,
    lCloudWatchLogsConfiguration,
    lCreatedAt,
    lCustomInstanceProfileArn,
    lCustomJson,
    lCustomRecipes,
    lCustomSecurityGroupIds,
    lDefaultRecipes,
    lDefaultSecurityGroupNames,
    lEnableAutoHealing,
    lInstallUpdatesOnBoot,
    lLayerId,
    lLifecycleEventConfiguration,
    lName,
    lPackages,
    lShortname,
    lStackId,
    lType,
    lUseEbsOptimizedInstances,
    lVolumeConfigurations,

    -- ** Recipes
    Recipes (..),
    mkRecipes,
    rConfigure,
    rDeploy,
    rSetup,
    rShutdown,
    rUndeploy,

    -- ** TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    mkTimeBasedAutoScalingConfiguration,
    tbascAutoScalingSchedule,
    tbascInstanceId,

    -- ** SelfUserProfile
    SelfUserProfile (..),
    mkSelfUserProfile,
    supIamUserArn,
    supName,
    supSshPublicKey,
    supSshUsername,

    -- ** RootDeviceType
    RootDeviceType (..),

    -- ** Stack
    Stack (..),
    mkStack,
    sAgentVersion,
    sArn,
    sAttributes,
    sChefConfiguration,
    sConfigurationManager,
    sCreatedAt,
    sCustomCookbooksSource,
    sCustomJson,
    sDefaultAvailabilityZone,
    sDefaultInstanceProfileArn,
    sDefaultOs,
    sDefaultRootDeviceType,
    sDefaultSshKeyName,
    sDefaultSubnetId,
    sHostnameTheme,
    sName,
    sRegion,
    sServiceRoleArn,
    sStackId,
    sUseCustomCookbooks,
    sUseOpsworksSecurityGroups,
    sVpcId,

    -- ** DeploymentCommand
    DeploymentCommand (..),
    mkDeploymentCommand,
    dcName,
    dcArgs,

    -- ** WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    mkWeeklyAutoScalingSchedule,
    wassFriday,
    wassMonday,
    wassSaturday,
    wassSunday,
    wassThursday,
    wassTuesday,
    wassWednesday,

    -- ** DeploymentCommandName
    DeploymentCommandName (..),

    -- ** DateTime
    DateTime (..),

    -- ** Instance
    Instance (..),
    mkInstance,
    iAgentVersion,
    iAmiId,
    iArchitecture,
    iArn,
    iAutoScalingType,
    iAvailabilityZone,
    iBlockDeviceMappings,
    iCreatedAt,
    iEbsOptimized,
    iEc2InstanceId,
    iEcsClusterArn,
    iEcsContainerInstanceArn,
    iElasticIp,
    iHostname,
    iInfrastructureClass,
    iInstallUpdatesOnBoot,
    iInstanceId,
    iInstanceProfileArn,
    iInstanceType,
    iLastServiceErrorId,
    iLayerIds,
    iOs,
    iPlatform,
    iPrivateDns,
    iPrivateIp,
    iPublicDns,
    iPublicIp,
    iRegisteredBy,
    iReportedAgentVersion,
    iReportedOs,
    iRootDeviceType,
    iRootDeviceVolumeId,
    iSecurityGroupIds,
    iSshHostDsaKeyFingerprint,
    iSshHostRsaKeyFingerprint,
    iSshKeyName,
    iStackId,
    iStatus,
    iSubnetId,
    iTenancy,
    iVirtualizationType,

    -- ** Deployment
    Deployment (..),
    mkDeployment,
    dAppId,
    dCommand,
    dComment,
    dCompletedAt,
    dCreatedAt,
    dCustomJson,
    dDeploymentId,
    dDuration,
    dIamUserArn,
    dInstanceIds,
    dStackId,
    dStatus,

    -- ** InstancesCount
    InstancesCount (..),
    mkInstancesCount,
    icAssigning,
    icBooting,
    icConnectionLost,
    icDeregistering,
    icOnline,
    icPending,
    icRebooting,
    icRegistered,
    icRegistering,
    icRequested,
    icRunningSetup,
    icSetupFailed,
    icShuttingDown,
    icStartFailed,
    icStopFailed,
    icStopped,
    icStopping,
    icTerminated,
    icTerminating,
    icUnassigning,

    -- ** AppType
    AppType (..),

    -- ** StackId
    StackId (..),

    -- ** DeploymentId
    DeploymentId (..),

    -- ** InstanceId
    InstanceId (..),

    -- ** AppId
    AppId (..),

    -- ** Comment
    Comment (..),

    -- ** CustomJson
    CustomJson (..),

    -- ** Certificate
    Certificate (..),

    -- ** PrivateKey
    PrivateKey (..),

    -- ** Chain
    Chain (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** AmiId
    AmiId (..),

    -- ** AvailabilityZone
    AvailabilityZone (..),

    -- ** Hostname
    Hostname (..),

    -- ** Os
    Os (..),

    -- ** SshKeyName
    SshKeyName (..),

    -- ** SubnetId
    SubnetId (..),

    -- ** Tenancy
    Tenancy (..),

    -- ** AcknowledgedAt
    AcknowledgedAt (..),

    -- ** CommandId
    CommandId (..),

    -- ** CompletedAt
    CompletedAt (..),

    -- ** CreatedAt
    CreatedAt (..),

    -- ** LogUrl
    LogUrl (..),

    -- ** Status
    Status (..),

    -- ** Type
    Type (..),

    -- ** RaidArrayId
    RaidArrayId (..),

    -- ** Device
    Device (..),

    -- ** MountPoint
    MountPoint (..),

    -- ** Name
    Name (..),

    -- ** IamUserArn
    IamUserArn (..),

    -- ** DnsName
    DnsName (..),

    -- ** ElasticLoadBalancerName
    ElasticLoadBalancerName (..),

    -- ** LayerId
    LayerId (..),

    -- ** Region
    Region (..),

    -- ** VpcId
    VpcId (..),

    -- ** EcsClusterArn
    EcsClusterArn (..),

    -- ** RdsDbInstanceArn
    RdsDbInstanceArn (..),

    -- ** DbPassword
    DbPassword (..),

    -- ** DbUser
    DbUser (..),

    -- ** Address
    Address (..),

    -- ** DbInstanceIdentifier
    DbInstanceIdentifier (..),

    -- ** Engine
    Engine (..),

    -- ** Arn
    Arn (..),

    -- ** DeviceName
    DeviceName (..),

    -- ** NoDevice
    NoDevice (..),

    -- ** VirtualName
    VirtualName (..),

    -- ** RegisteredAt
    RegisteredAt (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.OpsWorks.AssignInstance
import Network.AWS.OpsWorks.AssignVolume
import Network.AWS.OpsWorks.AssociateElasticIp
import Network.AWS.OpsWorks.AttachElasticLoadBalancer
import Network.AWS.OpsWorks.CloneStack
import Network.AWS.OpsWorks.CreateApp
import Network.AWS.OpsWorks.CreateDeployment
import Network.AWS.OpsWorks.CreateInstance
import Network.AWS.OpsWorks.CreateLayer
import Network.AWS.OpsWorks.CreateStack
import Network.AWS.OpsWorks.CreateUserProfile
import Network.AWS.OpsWorks.DeleteApp
import Network.AWS.OpsWorks.DeleteInstance
import Network.AWS.OpsWorks.DeleteLayer
import Network.AWS.OpsWorks.DeleteStack
import Network.AWS.OpsWorks.DeleteUserProfile
import Network.AWS.OpsWorks.DeregisterEcsCluster
import Network.AWS.OpsWorks.DeregisterElasticIp
import Network.AWS.OpsWorks.DeregisterInstance
import Network.AWS.OpsWorks.DeregisterRdsDbInstance
import Network.AWS.OpsWorks.DeregisterVolume
import Network.AWS.OpsWorks.DescribeAgentVersions
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeCommands
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeEcsClusters
import Network.AWS.OpsWorks.DescribeElasticIps
import Network.AWS.OpsWorks.DescribeElasticLoadBalancers
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeLayers
import Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
import Network.AWS.OpsWorks.DescribeMyUserProfile
import Network.AWS.OpsWorks.DescribeOperatingSystems
import Network.AWS.OpsWorks.DescribePermissions
import Network.AWS.OpsWorks.DescribeRaidArrays
import Network.AWS.OpsWorks.DescribeRdsDbInstances
import Network.AWS.OpsWorks.DescribeServiceErrors
import Network.AWS.OpsWorks.DescribeStackProvisioningParameters
import Network.AWS.OpsWorks.DescribeStackSummary
import Network.AWS.OpsWorks.DescribeStacks
import Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
import Network.AWS.OpsWorks.DescribeUserProfiles
import Network.AWS.OpsWorks.DescribeVolumes
import Network.AWS.OpsWorks.DetachElasticLoadBalancer
import Network.AWS.OpsWorks.DisassociateElasticIp
import Network.AWS.OpsWorks.GetHostnameSuggestion
import Network.AWS.OpsWorks.GrantAccess
import Network.AWS.OpsWorks.ListTags
import Network.AWS.OpsWorks.RebootInstance
import Network.AWS.OpsWorks.RegisterEcsCluster
import Network.AWS.OpsWorks.RegisterElasticIp
import Network.AWS.OpsWorks.RegisterInstance
import Network.AWS.OpsWorks.RegisterRdsDbInstance
import Network.AWS.OpsWorks.RegisterVolume
import Network.AWS.OpsWorks.SetLoadBasedAutoScaling
import Network.AWS.OpsWorks.SetPermission
import Network.AWS.OpsWorks.SetTimeBasedAutoScaling
import Network.AWS.OpsWorks.StartInstance
import Network.AWS.OpsWorks.StartStack
import Network.AWS.OpsWorks.StopInstance
import Network.AWS.OpsWorks.StopStack
import Network.AWS.OpsWorks.TagResource
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.UnassignInstance
import Network.AWS.OpsWorks.UnassignVolume
import Network.AWS.OpsWorks.UntagResource
import Network.AWS.OpsWorks.UpdateApp
import Network.AWS.OpsWorks.UpdateElasticIp
import Network.AWS.OpsWorks.UpdateInstance
import Network.AWS.OpsWorks.UpdateLayer
import Network.AWS.OpsWorks.UpdateMyUserProfile
import Network.AWS.OpsWorks.UpdateRdsDbInstance
import Network.AWS.OpsWorks.UpdateStack
import Network.AWS.OpsWorks.UpdateUserProfile
import Network.AWS.OpsWorks.UpdateVolume
import Network.AWS.OpsWorks.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OpsWorks'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
