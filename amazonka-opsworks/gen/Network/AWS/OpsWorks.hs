{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS OpsWorks
--
-- Welcome to the /AWS OpsWorks Stacks API Reference/. This guide provides
-- descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions
-- and data types, including common parameters and error codes.
--
-- AWS OpsWorks Stacks is an application management service that provides
-- an integrated experience for overseeing the complete application
-- lifecycle. For information about this product, go to the
-- <http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.
--
-- __SDKs and CLI__
--
-- The most common way to use the AWS OpsWorks Stacks API is by using the
-- AWS Command Line Interface (CLI) or by using one of the AWS SDKs to
-- implement applications in your preferred language. For more information,
-- see:
--
-- -   <https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
--
-- -   <https://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
--
-- -   <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
--
-- -   <https://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
--
-- -   <http://docs.aws.amazon.com/sdkforruby/api/ AWS SDK for Ruby>
--
-- -   <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
--
-- -   <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>
--
-- __Endpoints__
--
-- AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You
-- must connect to one of the following endpoints. Stacks can only be
-- accessed or managed within the endpoint in which they are created.
--
-- -   opsworks.us-east-1.amazonaws.com
--
-- -   opsworks.us-east-2.amazonaws.com
--
-- -   opsworks.us-west-1.amazonaws.com
--
-- -   opsworks.us-west-2.amazonaws.com
--
-- -   opsworks.ca-central-1.amazonaws.com (API only; not available in the
--     AWS console)
--
-- -   opsworks.eu-west-1.amazonaws.com
--
-- -   opsworks.eu-west-2.amazonaws.com
--
-- -   opsworks.eu-west-3.amazonaws.com
--
-- -   opsworks.eu-central-1.amazonaws.com
--
-- -   opsworks.ap-northeast-1.amazonaws.com
--
-- -   opsworks.ap-northeast-2.amazonaws.com
--
-- -   opsworks.ap-south-1.amazonaws.com
--
-- -   opsworks.ap-southeast-1.amazonaws.com
--
-- -   opsworks.ap-southeast-2.amazonaws.com
--
-- -   opsworks.sa-east-1.amazonaws.com
--
-- __Chef Versions__
--
-- When you call CreateStack, CloneStack, or UpdateStack we recommend you
-- use the @ConfigurationManager@ parameter to specify the Chef version.
-- The recommended and default value for Linux stacks is currently 12.
-- Windows stacks use Chef 12.2. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions>.
--
-- You can specify Chef 12, 11.10, or 11.4 for your Linux stack. We
-- recommend migrating your existing Linux stacks to Chef 12 as soon as
-- possible.
module Network.AWS.OpsWorks
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    newInstanceTerminated,

    -- ** InstanceRegistered
    newInstanceRegistered,

    -- ** AppExists
    newAppExists,

    -- ** InstanceOnline
    newInstanceOnline,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** DeploymentSuccessful
    newDeploymentSuccessful,

    -- * Operations
    -- $operations

    -- ** DescribeInstances
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DescribeDeployments
    DescribeDeployments (DescribeDeployments'),
    newDescribeDeployments,
    DescribeDeploymentsResponse (DescribeDeploymentsResponse'),
    newDescribeDeploymentsResponse,

    -- ** UpdateMyUserProfile
    UpdateMyUserProfile (UpdateMyUserProfile'),
    newUpdateMyUserProfile,
    UpdateMyUserProfileResponse (UpdateMyUserProfileResponse'),
    newUpdateMyUserProfileResponse,

    -- ** DeregisterElasticIp
    DeregisterElasticIp (DeregisterElasticIp'),
    newDeregisterElasticIp,
    DeregisterElasticIpResponse (DeregisterElasticIpResponse'),
    newDeregisterElasticIpResponse,

    -- ** SetTimeBasedAutoScaling
    SetTimeBasedAutoScaling (SetTimeBasedAutoScaling'),
    newSetTimeBasedAutoScaling,
    SetTimeBasedAutoScalingResponse (SetTimeBasedAutoScalingResponse'),
    newSetTimeBasedAutoScalingResponse,

    -- ** DescribeRdsDbInstances
    DescribeRdsDbInstances (DescribeRdsDbInstances'),
    newDescribeRdsDbInstances,
    DescribeRdsDbInstancesResponse (DescribeRdsDbInstancesResponse'),
    newDescribeRdsDbInstancesResponse,

    -- ** AttachElasticLoadBalancer
    AttachElasticLoadBalancer (AttachElasticLoadBalancer'),
    newAttachElasticLoadBalancer,
    AttachElasticLoadBalancerResponse (AttachElasticLoadBalancerResponse'),
    newAttachElasticLoadBalancerResponse,

    -- ** StartInstance
    StartInstance (StartInstance'),
    newStartInstance,
    StartInstanceResponse (StartInstanceResponse'),
    newStartInstanceResponse,

    -- ** SetPermission
    SetPermission (SetPermission'),
    newSetPermission,
    SetPermissionResponse (SetPermissionResponse'),
    newSetPermissionResponse,

    -- ** RegisterVolume
    RegisterVolume (RegisterVolume'),
    newRegisterVolume,
    RegisterVolumeResponse (RegisterVolumeResponse'),
    newRegisterVolumeResponse,

    -- ** StopInstance
    StopInstance (StopInstance'),
    newStopInstance,
    StopInstanceResponse (StopInstanceResponse'),
    newStopInstanceResponse,

    -- ** DescribeEcsClusters (Paginated)
    DescribeEcsClusters (DescribeEcsClusters'),
    newDescribeEcsClusters,
    DescribeEcsClustersResponse (DescribeEcsClustersResponse'),
    newDescribeEcsClustersResponse,

    -- ** DescribeVolumes
    DescribeVolumes (DescribeVolumes'),
    newDescribeVolumes,
    DescribeVolumesResponse (DescribeVolumesResponse'),
    newDescribeVolumesResponse,

    -- ** DescribeOperatingSystems
    DescribeOperatingSystems (DescribeOperatingSystems'),
    newDescribeOperatingSystems,
    DescribeOperatingSystemsResponse (DescribeOperatingSystemsResponse'),
    newDescribeOperatingSystemsResponse,

    -- ** DisassociateElasticIp
    DisassociateElasticIp (DisassociateElasticIp'),
    newDisassociateElasticIp,
    DisassociateElasticIpResponse (DisassociateElasticIpResponse'),
    newDisassociateElasticIpResponse,

    -- ** StartStack
    StartStack (StartStack'),
    newStartStack,
    StartStackResponse (StartStackResponse'),
    newStartStackResponse,

    -- ** StopStack
    StopStack (StopStack'),
    newStopStack,
    StopStackResponse (StopStackResponse'),
    newStopStackResponse,

    -- ** RegisterRdsDbInstance
    RegisterRdsDbInstance (RegisterRdsDbInstance'),
    newRegisterRdsDbInstance,
    RegisterRdsDbInstanceResponse (RegisterRdsDbInstanceResponse'),
    newRegisterRdsDbInstanceResponse,

    -- ** DescribeServiceErrors
    DescribeServiceErrors (DescribeServiceErrors'),
    newDescribeServiceErrors,
    DescribeServiceErrorsResponse (DescribeServiceErrorsResponse'),
    newDescribeServiceErrorsResponse,

    -- ** DescribeTimeBasedAutoScaling
    DescribeTimeBasedAutoScaling (DescribeTimeBasedAutoScaling'),
    newDescribeTimeBasedAutoScaling,
    DescribeTimeBasedAutoScalingResponse (DescribeTimeBasedAutoScalingResponse'),
    newDescribeTimeBasedAutoScalingResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

    -- ** DescribeMyUserProfile
    DescribeMyUserProfile (DescribeMyUserProfile'),
    newDescribeMyUserProfile,
    DescribeMyUserProfileResponse (DescribeMyUserProfileResponse'),
    newDescribeMyUserProfileResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

    -- ** AssignInstance
    AssignInstance (AssignInstance'),
    newAssignInstance,
    AssignInstanceResponse (AssignInstanceResponse'),
    newAssignInstanceResponse,

    -- ** DetachElasticLoadBalancer
    DetachElasticLoadBalancer (DetachElasticLoadBalancer'),
    newDetachElasticLoadBalancer,
    DetachElasticLoadBalancerResponse (DetachElasticLoadBalancerResponse'),
    newDetachElasticLoadBalancerResponse,

    -- ** DescribeStackProvisioningParameters
    DescribeStackProvisioningParameters (DescribeStackProvisioningParameters'),
    newDescribeStackProvisioningParameters,
    DescribeStackProvisioningParametersResponse (DescribeStackProvisioningParametersResponse'),
    newDescribeStackProvisioningParametersResponse,

    -- ** DeregisterVolume
    DeregisterVolume (DeregisterVolume'),
    newDeregisterVolume,
    DeregisterVolumeResponse (DeregisterVolumeResponse'),
    newDeregisterVolumeResponse,

    -- ** DescribeStacks
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** RebootInstance
    RebootInstance (RebootInstance'),
    newRebootInstance,
    RebootInstanceResponse (RebootInstanceResponse'),
    newRebootInstanceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UpdateInstance
    UpdateInstance (UpdateInstance'),
    newUpdateInstance,
    UpdateInstanceResponse (UpdateInstanceResponse'),
    newUpdateInstanceResponse,

    -- ** CloneStack
    CloneStack (CloneStack'),
    newCloneStack,
    CloneStackResponse (CloneStackResponse'),
    newCloneStackResponse,

    -- ** RegisterElasticIp
    RegisterElasticIp (RegisterElasticIp'),
    newRegisterElasticIp,
    RegisterElasticIpResponse (RegisterElasticIpResponse'),
    newRegisterElasticIpResponse,

    -- ** DescribeAgentVersions
    DescribeAgentVersions (DescribeAgentVersions'),
    newDescribeAgentVersions,
    DescribeAgentVersionsResponse (DescribeAgentVersionsResponse'),
    newDescribeAgentVersionsResponse,

    -- ** UpdateLayer
    UpdateLayer (UpdateLayer'),
    newUpdateLayer,
    UpdateLayerResponse (UpdateLayerResponse'),
    newUpdateLayerResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** UnassignVolume
    UnassignVolume (UnassignVolume'),
    newUnassignVolume,
    UnassignVolumeResponse (UnassignVolumeResponse'),
    newUnassignVolumeResponse,

    -- ** GrantAccess
    GrantAccess (GrantAccess'),
    newGrantAccess,
    GrantAccessResponse (GrantAccessResponse'),
    newGrantAccessResponse,

    -- ** DeleteLayer
    DeleteLayer (DeleteLayer'),
    newDeleteLayer,
    DeleteLayerResponse (DeleteLayerResponse'),
    newDeleteLayerResponse,

    -- ** DescribeApps
    DescribeApps (DescribeApps'),
    newDescribeApps,
    DescribeAppsResponse (DescribeAppsResponse'),
    newDescribeAppsResponse,

    -- ** DeregisterEcsCluster
    DeregisterEcsCluster (DeregisterEcsCluster'),
    newDeregisterEcsCluster,
    DeregisterEcsClusterResponse (DeregisterEcsClusterResponse'),
    newDeregisterEcsClusterResponse,

    -- ** DescribeStackSummary
    DescribeStackSummary (DescribeStackSummary'),
    newDescribeStackSummary,
    DescribeStackSummaryResponse (DescribeStackSummaryResponse'),
    newDescribeStackSummaryResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** SetLoadBasedAutoScaling
    SetLoadBasedAutoScaling (SetLoadBasedAutoScaling'),
    newSetLoadBasedAutoScaling,
    SetLoadBasedAutoScalingResponse (SetLoadBasedAutoScalingResponse'),
    newSetLoadBasedAutoScalingResponse,

    -- ** CreateLayer
    CreateLayer (CreateLayer'),
    newCreateLayer,
    CreateLayerResponse (CreateLayerResponse'),
    newCreateLayerResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- ** DescribeUserProfiles
    DescribeUserProfiles (DescribeUserProfiles'),
    newDescribeUserProfiles,
    DescribeUserProfilesResponse (DescribeUserProfilesResponse'),
    newDescribeUserProfilesResponse,

    -- ** DescribeElasticLoadBalancers
    DescribeElasticLoadBalancers (DescribeElasticLoadBalancers'),
    newDescribeElasticLoadBalancers,
    DescribeElasticLoadBalancersResponse (DescribeElasticLoadBalancersResponse'),
    newDescribeElasticLoadBalancersResponse,

    -- ** DescribeCommands
    DescribeCommands (DescribeCommands'),
    newDescribeCommands,
    DescribeCommandsResponse (DescribeCommandsResponse'),
    newDescribeCommandsResponse,

    -- ** UpdateVolume
    UpdateVolume (UpdateVolume'),
    newUpdateVolume,
    UpdateVolumeResponse (UpdateVolumeResponse'),
    newUpdateVolumeResponse,

    -- ** AssignVolume
    AssignVolume (AssignVolume'),
    newAssignVolume,
    AssignVolumeResponse (AssignVolumeResponse'),
    newAssignVolumeResponse,

    -- ** DescribeRaidArrays
    DescribeRaidArrays (DescribeRaidArrays'),
    newDescribeRaidArrays,
    DescribeRaidArraysResponse (DescribeRaidArraysResponse'),
    newDescribeRaidArraysResponse,

    -- ** DeregisterInstance
    DeregisterInstance (DeregisterInstance'),
    newDeregisterInstance,
    DeregisterInstanceResponse (DeregisterInstanceResponse'),
    newDeregisterInstanceResponse,

    -- ** RegisterEcsCluster
    RegisterEcsCluster (RegisterEcsCluster'),
    newRegisterEcsCluster,
    RegisterEcsClusterResponse (RegisterEcsClusterResponse'),
    newRegisterEcsClusterResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** UpdateRdsDbInstance
    UpdateRdsDbInstance (UpdateRdsDbInstance'),
    newUpdateRdsDbInstance,
    UpdateRdsDbInstanceResponse (UpdateRdsDbInstanceResponse'),
    newUpdateRdsDbInstanceResponse,

    -- ** UnassignInstance
    UnassignInstance (UnassignInstance'),
    newUnassignInstance,
    UnassignInstanceResponse (UnassignInstanceResponse'),
    newUnassignInstanceResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DescribeLoadBasedAutoScaling
    DescribeLoadBasedAutoScaling (DescribeLoadBasedAutoScaling'),
    newDescribeLoadBasedAutoScaling,
    DescribeLoadBasedAutoScalingResponse (DescribeLoadBasedAutoScalingResponse'),
    newDescribeLoadBasedAutoScalingResponse,

    -- ** RegisterInstance
    RegisterInstance (RegisterInstance'),
    newRegisterInstance,
    RegisterInstanceResponse (RegisterInstanceResponse'),
    newRegisterInstanceResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** AssociateElasticIp
    AssociateElasticIp (AssociateElasticIp'),
    newAssociateElasticIp,
    AssociateElasticIpResponse (AssociateElasticIpResponse'),
    newAssociateElasticIpResponse,

    -- ** UpdateElasticIp
    UpdateElasticIp (UpdateElasticIp'),
    newUpdateElasticIp,
    UpdateElasticIpResponse (UpdateElasticIpResponse'),
    newUpdateElasticIpResponse,

    -- ** DescribePermissions
    DescribePermissions (DescribePermissions'),
    newDescribePermissions,
    DescribePermissionsResponse (DescribePermissionsResponse'),
    newDescribePermissionsResponse,

    -- ** GetHostnameSuggestion
    GetHostnameSuggestion (GetHostnameSuggestion'),
    newGetHostnameSuggestion,
    GetHostnameSuggestionResponse (GetHostnameSuggestionResponse'),
    newGetHostnameSuggestionResponse,

    -- ** CreateInstance
    CreateInstance (CreateInstance'),
    newCreateInstance,
    CreateInstanceResponse (CreateInstanceResponse'),
    newCreateInstanceResponse,

    -- ** DescribeLayers
    DescribeLayers (DescribeLayers'),
    newDescribeLayers,
    DescribeLayersResponse (DescribeLayersResponse'),
    newDescribeLayersResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** DeregisterRdsDbInstance
    DeregisterRdsDbInstance (DeregisterRdsDbInstance'),
    newDeregisterRdsDbInstance,
    DeregisterRdsDbInstanceResponse (DeregisterRdsDbInstanceResponse'),
    newDeregisterRdsDbInstanceResponse,

    -- ** DescribeElasticIps
    DescribeElasticIps (DescribeElasticIps'),
    newDescribeElasticIps,
    DescribeElasticIpsResponse (DescribeElasticIpsResponse'),
    newDescribeElasticIpsResponse,

    -- * Types

    -- ** AppAttributesKeys
    AppAttributesKeys (..),

    -- ** AppType
    AppType (..),

    -- ** Architecture
    Architecture (..),

    -- ** AutoScalingType
    AutoScalingType (..),

    -- ** CloudWatchLogsEncoding
    CloudWatchLogsEncoding (..),

    -- ** CloudWatchLogsInitialPosition
    CloudWatchLogsInitialPosition (..),

    -- ** CloudWatchLogsTimeZone
    CloudWatchLogsTimeZone (..),

    -- ** DeploymentCommandName
    DeploymentCommandName (..),

    -- ** LayerAttributesKeys
    LayerAttributesKeys (..),

    -- ** LayerType
    LayerType (..),

    -- ** RootDeviceType
    RootDeviceType (..),

    -- ** SourceType
    SourceType (..),

    -- ** StackAttributesKeys
    StackAttributesKeys (..),

    -- ** VirtualizationType
    VirtualizationType (..),

    -- ** VolumeType
    VolumeType (..),

    -- ** AgentVersion
    AgentVersion (AgentVersion'),
    newAgentVersion,

    -- ** App
    App (App'),
    newApp,

    -- ** AutoScalingThresholds
    AutoScalingThresholds (AutoScalingThresholds'),
    newAutoScalingThresholds,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (BlockDeviceMapping'),
    newBlockDeviceMapping,

    -- ** ChefConfiguration
    ChefConfiguration (ChefConfiguration'),
    newChefConfiguration,

    -- ** CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (CloudWatchLogsConfiguration'),
    newCloudWatchLogsConfiguration,

    -- ** CloudWatchLogsLogStream
    CloudWatchLogsLogStream (CloudWatchLogsLogStream'),
    newCloudWatchLogsLogStream,

    -- ** Command
    Command (Command'),
    newCommand,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DeploymentCommand
    DeploymentCommand (DeploymentCommand'),
    newDeploymentCommand,

    -- ** EbsBlockDevice
    EbsBlockDevice (EbsBlockDevice'),
    newEbsBlockDevice,

    -- ** EcsCluster
    EcsCluster (EcsCluster'),
    newEcsCluster,

    -- ** ElasticIp
    ElasticIp (ElasticIp'),
    newElasticIp,

    -- ** ElasticLoadBalancer
    ElasticLoadBalancer (ElasticLoadBalancer'),
    newElasticLoadBalancer,

    -- ** EnvironmentVariable
    EnvironmentVariable (EnvironmentVariable'),
    newEnvironmentVariable,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceIdentity
    InstanceIdentity (InstanceIdentity'),
    newInstanceIdentity,

    -- ** InstancesCount
    InstancesCount (InstancesCount'),
    newInstancesCount,

    -- ** Layer
    Layer (Layer'),
    newLayer,

    -- ** LifecycleEventConfiguration
    LifecycleEventConfiguration (LifecycleEventConfiguration'),
    newLifecycleEventConfiguration,

    -- ** LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (LoadBasedAutoScalingConfiguration'),
    newLoadBasedAutoScalingConfiguration,

    -- ** OperatingSystem
    OperatingSystem (OperatingSystem'),
    newOperatingSystem,

    -- ** OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (OperatingSystemConfigurationManager'),
    newOperatingSystemConfigurationManager,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** RaidArray
    RaidArray (RaidArray'),
    newRaidArray,

    -- ** RdsDbInstance
    RdsDbInstance (RdsDbInstance'),
    newRdsDbInstance,

    -- ** Recipes
    Recipes (Recipes'),
    newRecipes,

    -- ** ReportedOs
    ReportedOs (ReportedOs'),
    newReportedOs,

    -- ** SelfUserProfile
    SelfUserProfile (SelfUserProfile'),
    newSelfUserProfile,

    -- ** ServiceError'
    ServiceError' (ServiceError''),
    newServiceError',

    -- ** ShutdownEventConfiguration
    ShutdownEventConfiguration (ShutdownEventConfiguration'),
    newShutdownEventConfiguration,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SslConfiguration
    SslConfiguration (SslConfiguration'),
    newSslConfiguration,

    -- ** Stack
    Stack (Stack'),
    newStack,

    -- ** StackConfigurationManager
    StackConfigurationManager (StackConfigurationManager'),
    newStackConfigurationManager,

    -- ** StackSummary
    StackSummary (StackSummary'),
    newStackSummary,

    -- ** TemporaryCredential
    TemporaryCredential (TemporaryCredential'),
    newTemporaryCredential,

    -- ** TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (TimeBasedAutoScalingConfiguration'),
    newTimeBasedAutoScalingConfiguration,

    -- ** UserProfile
    UserProfile (UserProfile'),
    newUserProfile,

    -- ** Volume
    Volume (Volume'),
    newVolume,

    -- ** VolumeConfiguration
    VolumeConfiguration (VolumeConfiguration'),
    newVolumeConfiguration,

    -- ** WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (WeeklyAutoScalingSchedule'),
    newWeeklyAutoScalingSchedule,
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
import Network.AWS.OpsWorks.Lens
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
