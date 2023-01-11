{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OpsWorks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-02-18@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.OpsWorks
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** AppExists
    newAppExists,

    -- ** DeploymentSuccessful
    newDeploymentSuccessful,

    -- ** InstanceOnline
    newInstanceOnline,

    -- ** InstanceRegistered
    newInstanceRegistered,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** InstanceTerminated
    newInstanceTerminated,

    -- * Operations
    -- $operations

    -- ** AssignInstance
    AssignInstance (AssignInstance'),
    newAssignInstance,
    AssignInstanceResponse (AssignInstanceResponse'),
    newAssignInstanceResponse,

    -- ** AssignVolume
    AssignVolume (AssignVolume'),
    newAssignVolume,
    AssignVolumeResponse (AssignVolumeResponse'),
    newAssignVolumeResponse,

    -- ** AssociateElasticIp
    AssociateElasticIp (AssociateElasticIp'),
    newAssociateElasticIp,
    AssociateElasticIpResponse (AssociateElasticIpResponse'),
    newAssociateElasticIpResponse,

    -- ** AttachElasticLoadBalancer
    AttachElasticLoadBalancer (AttachElasticLoadBalancer'),
    newAttachElasticLoadBalancer,
    AttachElasticLoadBalancerResponse (AttachElasticLoadBalancerResponse'),
    newAttachElasticLoadBalancerResponse,

    -- ** CloneStack
    CloneStack (CloneStack'),
    newCloneStack,
    CloneStackResponse (CloneStackResponse'),
    newCloneStackResponse,

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

    -- ** CreateInstance
    CreateInstance (CreateInstance'),
    newCreateInstance,
    CreateInstanceResponse (CreateInstanceResponse'),
    newCreateInstanceResponse,

    -- ** CreateLayer
    CreateLayer (CreateLayer'),
    newCreateLayer,
    CreateLayerResponse (CreateLayerResponse'),
    newCreateLayerResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** DeleteLayer
    DeleteLayer (DeleteLayer'),
    newDeleteLayer,
    DeleteLayerResponse (DeleteLayerResponse'),
    newDeleteLayerResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

    -- ** DeregisterEcsCluster
    DeregisterEcsCluster (DeregisterEcsCluster'),
    newDeregisterEcsCluster,
    DeregisterEcsClusterResponse (DeregisterEcsClusterResponse'),
    newDeregisterEcsClusterResponse,

    -- ** DeregisterElasticIp
    DeregisterElasticIp (DeregisterElasticIp'),
    newDeregisterElasticIp,
    DeregisterElasticIpResponse (DeregisterElasticIpResponse'),
    newDeregisterElasticIpResponse,

    -- ** DeregisterInstance
    DeregisterInstance (DeregisterInstance'),
    newDeregisterInstance,
    DeregisterInstanceResponse (DeregisterInstanceResponse'),
    newDeregisterInstanceResponse,

    -- ** DeregisterRdsDbInstance
    DeregisterRdsDbInstance (DeregisterRdsDbInstance'),
    newDeregisterRdsDbInstance,
    DeregisterRdsDbInstanceResponse (DeregisterRdsDbInstanceResponse'),
    newDeregisterRdsDbInstanceResponse,

    -- ** DeregisterVolume
    DeregisterVolume (DeregisterVolume'),
    newDeregisterVolume,
    DeregisterVolumeResponse (DeregisterVolumeResponse'),
    newDeregisterVolumeResponse,

    -- ** DescribeAgentVersions
    DescribeAgentVersions (DescribeAgentVersions'),
    newDescribeAgentVersions,
    DescribeAgentVersionsResponse (DescribeAgentVersionsResponse'),
    newDescribeAgentVersionsResponse,

    -- ** DescribeApps
    DescribeApps (DescribeApps'),
    newDescribeApps,
    DescribeAppsResponse (DescribeAppsResponse'),
    newDescribeAppsResponse,

    -- ** DescribeCommands
    DescribeCommands (DescribeCommands'),
    newDescribeCommands,
    DescribeCommandsResponse (DescribeCommandsResponse'),
    newDescribeCommandsResponse,

    -- ** DescribeDeployments
    DescribeDeployments (DescribeDeployments'),
    newDescribeDeployments,
    DescribeDeploymentsResponse (DescribeDeploymentsResponse'),
    newDescribeDeploymentsResponse,

    -- ** DescribeEcsClusters (Paginated)
    DescribeEcsClusters (DescribeEcsClusters'),
    newDescribeEcsClusters,
    DescribeEcsClustersResponse (DescribeEcsClustersResponse'),
    newDescribeEcsClustersResponse,

    -- ** DescribeElasticIps
    DescribeElasticIps (DescribeElasticIps'),
    newDescribeElasticIps,
    DescribeElasticIpsResponse (DescribeElasticIpsResponse'),
    newDescribeElasticIpsResponse,

    -- ** DescribeElasticLoadBalancers
    DescribeElasticLoadBalancers (DescribeElasticLoadBalancers'),
    newDescribeElasticLoadBalancers,
    DescribeElasticLoadBalancersResponse (DescribeElasticLoadBalancersResponse'),
    newDescribeElasticLoadBalancersResponse,

    -- ** DescribeInstances
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DescribeLayers
    DescribeLayers (DescribeLayers'),
    newDescribeLayers,
    DescribeLayersResponse (DescribeLayersResponse'),
    newDescribeLayersResponse,

    -- ** DescribeLoadBasedAutoScaling
    DescribeLoadBasedAutoScaling (DescribeLoadBasedAutoScaling'),
    newDescribeLoadBasedAutoScaling,
    DescribeLoadBasedAutoScalingResponse (DescribeLoadBasedAutoScalingResponse'),
    newDescribeLoadBasedAutoScalingResponse,

    -- ** DescribeMyUserProfile
    DescribeMyUserProfile (DescribeMyUserProfile'),
    newDescribeMyUserProfile,
    DescribeMyUserProfileResponse (DescribeMyUserProfileResponse'),
    newDescribeMyUserProfileResponse,

    -- ** DescribeOperatingSystems
    DescribeOperatingSystems (DescribeOperatingSystems'),
    newDescribeOperatingSystems,
    DescribeOperatingSystemsResponse (DescribeOperatingSystemsResponse'),
    newDescribeOperatingSystemsResponse,

    -- ** DescribePermissions
    DescribePermissions (DescribePermissions'),
    newDescribePermissions,
    DescribePermissionsResponse (DescribePermissionsResponse'),
    newDescribePermissionsResponse,

    -- ** DescribeRaidArrays
    DescribeRaidArrays (DescribeRaidArrays'),
    newDescribeRaidArrays,
    DescribeRaidArraysResponse (DescribeRaidArraysResponse'),
    newDescribeRaidArraysResponse,

    -- ** DescribeRdsDbInstances
    DescribeRdsDbInstances (DescribeRdsDbInstances'),
    newDescribeRdsDbInstances,
    DescribeRdsDbInstancesResponse (DescribeRdsDbInstancesResponse'),
    newDescribeRdsDbInstancesResponse,

    -- ** DescribeServiceErrors
    DescribeServiceErrors (DescribeServiceErrors'),
    newDescribeServiceErrors,
    DescribeServiceErrorsResponse (DescribeServiceErrorsResponse'),
    newDescribeServiceErrorsResponse,

    -- ** DescribeStackProvisioningParameters
    DescribeStackProvisioningParameters (DescribeStackProvisioningParameters'),
    newDescribeStackProvisioningParameters,
    DescribeStackProvisioningParametersResponse (DescribeStackProvisioningParametersResponse'),
    newDescribeStackProvisioningParametersResponse,

    -- ** DescribeStackSummary
    DescribeStackSummary (DescribeStackSummary'),
    newDescribeStackSummary,
    DescribeStackSummaryResponse (DescribeStackSummaryResponse'),
    newDescribeStackSummaryResponse,

    -- ** DescribeStacks
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** DescribeTimeBasedAutoScaling
    DescribeTimeBasedAutoScaling (DescribeTimeBasedAutoScaling'),
    newDescribeTimeBasedAutoScaling,
    DescribeTimeBasedAutoScalingResponse (DescribeTimeBasedAutoScalingResponse'),
    newDescribeTimeBasedAutoScalingResponse,

    -- ** DescribeUserProfiles
    DescribeUserProfiles (DescribeUserProfiles'),
    newDescribeUserProfiles,
    DescribeUserProfilesResponse (DescribeUserProfilesResponse'),
    newDescribeUserProfilesResponse,

    -- ** DescribeVolumes
    DescribeVolumes (DescribeVolumes'),
    newDescribeVolumes,
    DescribeVolumesResponse (DescribeVolumesResponse'),
    newDescribeVolumesResponse,

    -- ** DetachElasticLoadBalancer
    DetachElasticLoadBalancer (DetachElasticLoadBalancer'),
    newDetachElasticLoadBalancer,
    DetachElasticLoadBalancerResponse (DetachElasticLoadBalancerResponse'),
    newDetachElasticLoadBalancerResponse,

    -- ** DisassociateElasticIp
    DisassociateElasticIp (DisassociateElasticIp'),
    newDisassociateElasticIp,
    DisassociateElasticIpResponse (DisassociateElasticIpResponse'),
    newDisassociateElasticIpResponse,

    -- ** GetHostnameSuggestion
    GetHostnameSuggestion (GetHostnameSuggestion'),
    newGetHostnameSuggestion,
    GetHostnameSuggestionResponse (GetHostnameSuggestionResponse'),
    newGetHostnameSuggestionResponse,

    -- ** GrantAccess
    GrantAccess (GrantAccess'),
    newGrantAccess,
    GrantAccessResponse (GrantAccessResponse'),
    newGrantAccessResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** RebootInstance
    RebootInstance (RebootInstance'),
    newRebootInstance,
    RebootInstanceResponse (RebootInstanceResponse'),
    newRebootInstanceResponse,

    -- ** RegisterEcsCluster
    RegisterEcsCluster (RegisterEcsCluster'),
    newRegisterEcsCluster,
    RegisterEcsClusterResponse (RegisterEcsClusterResponse'),
    newRegisterEcsClusterResponse,

    -- ** RegisterElasticIp
    RegisterElasticIp (RegisterElasticIp'),
    newRegisterElasticIp,
    RegisterElasticIpResponse (RegisterElasticIpResponse'),
    newRegisterElasticIpResponse,

    -- ** RegisterInstance
    RegisterInstance (RegisterInstance'),
    newRegisterInstance,
    RegisterInstanceResponse (RegisterInstanceResponse'),
    newRegisterInstanceResponse,

    -- ** RegisterRdsDbInstance
    RegisterRdsDbInstance (RegisterRdsDbInstance'),
    newRegisterRdsDbInstance,
    RegisterRdsDbInstanceResponse (RegisterRdsDbInstanceResponse'),
    newRegisterRdsDbInstanceResponse,

    -- ** RegisterVolume
    RegisterVolume (RegisterVolume'),
    newRegisterVolume,
    RegisterVolumeResponse (RegisterVolumeResponse'),
    newRegisterVolumeResponse,

    -- ** SetLoadBasedAutoScaling
    SetLoadBasedAutoScaling (SetLoadBasedAutoScaling'),
    newSetLoadBasedAutoScaling,
    SetLoadBasedAutoScalingResponse (SetLoadBasedAutoScalingResponse'),
    newSetLoadBasedAutoScalingResponse,

    -- ** SetPermission
    SetPermission (SetPermission'),
    newSetPermission,
    SetPermissionResponse (SetPermissionResponse'),
    newSetPermissionResponse,

    -- ** SetTimeBasedAutoScaling
    SetTimeBasedAutoScaling (SetTimeBasedAutoScaling'),
    newSetTimeBasedAutoScaling,
    SetTimeBasedAutoScalingResponse (SetTimeBasedAutoScalingResponse'),
    newSetTimeBasedAutoScalingResponse,

    -- ** StartInstance
    StartInstance (StartInstance'),
    newStartInstance,
    StartInstanceResponse (StartInstanceResponse'),
    newStartInstanceResponse,

    -- ** StartStack
    StartStack (StartStack'),
    newStartStack,
    StartStackResponse (StartStackResponse'),
    newStartStackResponse,

    -- ** StopInstance
    StopInstance (StopInstance'),
    newStopInstance,
    StopInstanceResponse (StopInstanceResponse'),
    newStopInstanceResponse,

    -- ** StopStack
    StopStack (StopStack'),
    newStopStack,
    StopStackResponse (StopStackResponse'),
    newStopStackResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnassignInstance
    UnassignInstance (UnassignInstance'),
    newUnassignInstance,
    UnassignInstanceResponse (UnassignInstanceResponse'),
    newUnassignInstanceResponse,

    -- ** UnassignVolume
    UnassignVolume (UnassignVolume'),
    newUnassignVolume,
    UnassignVolumeResponse (UnassignVolumeResponse'),
    newUnassignVolumeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** UpdateElasticIp
    UpdateElasticIp (UpdateElasticIp'),
    newUpdateElasticIp,
    UpdateElasticIpResponse (UpdateElasticIpResponse'),
    newUpdateElasticIpResponse,

    -- ** UpdateInstance
    UpdateInstance (UpdateInstance'),
    newUpdateInstance,
    UpdateInstanceResponse (UpdateInstanceResponse'),
    newUpdateInstanceResponse,

    -- ** UpdateLayer
    UpdateLayer (UpdateLayer'),
    newUpdateLayer,
    UpdateLayerResponse (UpdateLayerResponse'),
    newUpdateLayerResponse,

    -- ** UpdateMyUserProfile
    UpdateMyUserProfile (UpdateMyUserProfile'),
    newUpdateMyUserProfile,
    UpdateMyUserProfileResponse (UpdateMyUserProfileResponse'),
    newUpdateMyUserProfileResponse,

    -- ** UpdateRdsDbInstance
    UpdateRdsDbInstance (UpdateRdsDbInstance'),
    newUpdateRdsDbInstance,
    UpdateRdsDbInstanceResponse (UpdateRdsDbInstanceResponse'),
    newUpdateRdsDbInstanceResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

    -- ** UpdateVolume
    UpdateVolume (UpdateVolume'),
    newUpdateVolume,
    UpdateVolumeResponse (UpdateVolumeResponse'),
    newUpdateVolumeResponse,

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

    -- ** ServiceError
    ServiceError (ServiceError'),
    newServiceError,

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

import Amazonka.OpsWorks.AssignInstance
import Amazonka.OpsWorks.AssignVolume
import Amazonka.OpsWorks.AssociateElasticIp
import Amazonka.OpsWorks.AttachElasticLoadBalancer
import Amazonka.OpsWorks.CloneStack
import Amazonka.OpsWorks.CreateApp
import Amazonka.OpsWorks.CreateDeployment
import Amazonka.OpsWorks.CreateInstance
import Amazonka.OpsWorks.CreateLayer
import Amazonka.OpsWorks.CreateStack
import Amazonka.OpsWorks.CreateUserProfile
import Amazonka.OpsWorks.DeleteApp
import Amazonka.OpsWorks.DeleteInstance
import Amazonka.OpsWorks.DeleteLayer
import Amazonka.OpsWorks.DeleteStack
import Amazonka.OpsWorks.DeleteUserProfile
import Amazonka.OpsWorks.DeregisterEcsCluster
import Amazonka.OpsWorks.DeregisterElasticIp
import Amazonka.OpsWorks.DeregisterInstance
import Amazonka.OpsWorks.DeregisterRdsDbInstance
import Amazonka.OpsWorks.DeregisterVolume
import Amazonka.OpsWorks.DescribeAgentVersions
import Amazonka.OpsWorks.DescribeApps
import Amazonka.OpsWorks.DescribeCommands
import Amazonka.OpsWorks.DescribeDeployments
import Amazonka.OpsWorks.DescribeEcsClusters
import Amazonka.OpsWorks.DescribeElasticIps
import Amazonka.OpsWorks.DescribeElasticLoadBalancers
import Amazonka.OpsWorks.DescribeInstances
import Amazonka.OpsWorks.DescribeLayers
import Amazonka.OpsWorks.DescribeLoadBasedAutoScaling
import Amazonka.OpsWorks.DescribeMyUserProfile
import Amazonka.OpsWorks.DescribeOperatingSystems
import Amazonka.OpsWorks.DescribePermissions
import Amazonka.OpsWorks.DescribeRaidArrays
import Amazonka.OpsWorks.DescribeRdsDbInstances
import Amazonka.OpsWorks.DescribeServiceErrors
import Amazonka.OpsWorks.DescribeStackProvisioningParameters
import Amazonka.OpsWorks.DescribeStackSummary
import Amazonka.OpsWorks.DescribeStacks
import Amazonka.OpsWorks.DescribeTimeBasedAutoScaling
import Amazonka.OpsWorks.DescribeUserProfiles
import Amazonka.OpsWorks.DescribeVolumes
import Amazonka.OpsWorks.DetachElasticLoadBalancer
import Amazonka.OpsWorks.DisassociateElasticIp
import Amazonka.OpsWorks.GetHostnameSuggestion
import Amazonka.OpsWorks.GrantAccess
import Amazonka.OpsWorks.Lens
import Amazonka.OpsWorks.ListTags
import Amazonka.OpsWorks.RebootInstance
import Amazonka.OpsWorks.RegisterEcsCluster
import Amazonka.OpsWorks.RegisterElasticIp
import Amazonka.OpsWorks.RegisterInstance
import Amazonka.OpsWorks.RegisterRdsDbInstance
import Amazonka.OpsWorks.RegisterVolume
import Amazonka.OpsWorks.SetLoadBasedAutoScaling
import Amazonka.OpsWorks.SetPermission
import Amazonka.OpsWorks.SetTimeBasedAutoScaling
import Amazonka.OpsWorks.StartInstance
import Amazonka.OpsWorks.StartStack
import Amazonka.OpsWorks.StopInstance
import Amazonka.OpsWorks.StopStack
import Amazonka.OpsWorks.TagResource
import Amazonka.OpsWorks.Types
import Amazonka.OpsWorks.UnassignInstance
import Amazonka.OpsWorks.UnassignVolume
import Amazonka.OpsWorks.UntagResource
import Amazonka.OpsWorks.UpdateApp
import Amazonka.OpsWorks.UpdateElasticIp
import Amazonka.OpsWorks.UpdateInstance
import Amazonka.OpsWorks.UpdateLayer
import Amazonka.OpsWorks.UpdateMyUserProfile
import Amazonka.OpsWorks.UpdateRdsDbInstance
import Amazonka.OpsWorks.UpdateStack
import Amazonka.OpsWorks.UpdateUserProfile
import Amazonka.OpsWorks.UpdateVolume
import Amazonka.OpsWorks.Waiters

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
