{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS OpsWorks__
--
-- Welcome to the /AWS OpsWorks Stacks API Reference/ . This guide provides descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions and data types, including common parameters and error codes.
--
-- AWS OpsWorks Stacks is an application management service that provides an integrated experience for overseeing the complete application lifecycle. For information about this product, go to the <http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.
--
-- __SDKs and CLI__
--
-- The most common way to use the AWS OpsWorks Stacks API is by using the AWS Command Line Interface (CLI) or by using one of the AWS SDKs to implement applications in your preferred language. For more information, see:
--
--     * <http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
--
--     * <http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
--
--     * <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
--
--     * <http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
--
--     * <http://docs.aws.amazon.com/sdkforruby/api/ AWS SDK for Ruby>
--
--     * <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
--
--     * <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>
--
--
--
-- __Endpoints__
--
-- AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Stacks can only be accessed or managed within the endpoint in which they are created.
--
--     * opsworks.us-east-1.amazonaws.com
--
--     * opsworks.us-east-2.amazonaws.com
--
--     * opsworks.us-west-1.amazonaws.com
--
--     * opsworks.us-west-2.amazonaws.com
--
--     * opsworks.ca-central-1.amazonaws.com (API only; not available in the AWS console)
--
--     * opsworks.eu-west-1.amazonaws.com
--
--     * opsworks.eu-west-2.amazonaws.com
--
--     * opsworks.eu-west-3.amazonaws.com
--
--     * opsworks.eu-central-1.amazonaws.com
--
--     * opsworks.ap-northeast-1.amazonaws.com
--
--     * opsworks.ap-northeast-2.amazonaws.com
--
--     * opsworks.ap-south-1.amazonaws.com
--
--     * opsworks.ap-southeast-1.amazonaws.com
--
--     * opsworks.ap-southeast-2.amazonaws.com
--
--     * opsworks.sa-east-1.amazonaws.com
--
--
--
-- __Chef Versions__
--
-- When you call 'CreateStack' , 'CloneStack' , or 'UpdateStack' we recommend you use the @ConfigurationManager@ parameter to specify the Chef version. The recommended and default value for Linux stacks is currently 12. Windows stacks use Chef 12.2. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions> .
--
module Network.AWS.OpsWorks
    (
    -- * Service Configuration
      opsWorks

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    , instanceTerminated

    -- ** DeploymentSuccessful
    , deploymentSuccessful

    -- ** InstanceStopped
    , instanceStopped

    -- ** InstanceOnline
    , instanceOnline

    -- ** AppExists
    , appExists

    -- ** InstanceRegistered
    , instanceRegistered

    -- * Operations
    -- $operations

    -- ** DescribeRDSDBInstances
    , module Network.AWS.OpsWorks.DescribeRDSDBInstances

    -- ** DeleteStack
    , module Network.AWS.OpsWorks.DeleteStack

    -- ** UpdateStack
    , module Network.AWS.OpsWorks.UpdateStack

    -- ** CreateLayer
    , module Network.AWS.OpsWorks.CreateLayer

    -- ** SetLoadBasedAutoScaling
    , module Network.AWS.OpsWorks.SetLoadBasedAutoScaling

    -- ** DeregisterRDSDBInstance
    , module Network.AWS.OpsWorks.DeregisterRDSDBInstance

    -- ** UnassignVolume
    , module Network.AWS.OpsWorks.UnassignVolume

    -- ** CreateInstance
    , module Network.AWS.OpsWorks.CreateInstance

    -- ** DescribeLayers
    , module Network.AWS.OpsWorks.DescribeLayers

    -- ** RegisterElasticIP
    , module Network.AWS.OpsWorks.RegisterElasticIP

    -- ** DescribeAgentVersions
    , module Network.AWS.OpsWorks.DescribeAgentVersions

    -- ** CreateDeployment
    , module Network.AWS.OpsWorks.CreateDeployment

    -- ** AssignInstance
    , module Network.AWS.OpsWorks.AssignInstance

    -- ** DescribeStacks
    , module Network.AWS.OpsWorks.DescribeStacks

    -- ** DeleteInstance
    , module Network.AWS.OpsWorks.DeleteInstance

    -- ** UpdateInstance
    , module Network.AWS.OpsWorks.UpdateInstance

    -- ** DeregisterVolume
    , module Network.AWS.OpsWorks.DeregisterVolume

    -- ** RebootInstance
    , module Network.AWS.OpsWorks.RebootInstance

    -- ** DeleteApp
    , module Network.AWS.OpsWorks.DeleteApp

    -- ** UpdateApp
    , module Network.AWS.OpsWorks.UpdateApp

    -- ** UpdateRDSDBInstance
    , module Network.AWS.OpsWorks.UpdateRDSDBInstance

    -- ** DescribeTimeBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling

    -- ** StopStack
    , module Network.AWS.OpsWorks.StopStack

    -- ** DescribeVolumes
    , module Network.AWS.OpsWorks.DescribeVolumes

    -- ** DisassociateElasticIP
    , module Network.AWS.OpsWorks.DisassociateElasticIP

    -- ** RegisterEcsCluster
    , module Network.AWS.OpsWorks.RegisterEcsCluster

    -- ** StopInstance
    , module Network.AWS.OpsWorks.StopInstance

    -- ** RegisterVolume
    , module Network.AWS.OpsWorks.RegisterVolume

    -- ** SetTimeBasedAutoScaling
    , module Network.AWS.OpsWorks.SetTimeBasedAutoScaling

    -- ** DescribeUserProfiles
    , module Network.AWS.OpsWorks.DescribeUserProfiles

    -- ** AttachElasticLoadBalancer
    , module Network.AWS.OpsWorks.AttachElasticLoadBalancer

    -- ** DeregisterElasticIP
    , module Network.AWS.OpsWorks.DeregisterElasticIP

    -- ** DeregisterEcsCluster
    , module Network.AWS.OpsWorks.DeregisterEcsCluster

    -- ** DescribeApps
    , module Network.AWS.OpsWorks.DescribeApps

    -- ** UpdateMyUserProfile
    , module Network.AWS.OpsWorks.UpdateMyUserProfile

    -- ** DescribeStackSummary
    , module Network.AWS.OpsWorks.DescribeStackSummary

    -- ** DescribeInstances
    , module Network.AWS.OpsWorks.DescribeInstances

    -- ** DescribeDeployments
    , module Network.AWS.OpsWorks.DescribeDeployments

    -- ** DescribeElasticIPs
    , module Network.AWS.OpsWorks.DescribeElasticIPs

    -- ** GrantAccess
    , module Network.AWS.OpsWorks.GrantAccess

    -- ** DeleteLayer
    , module Network.AWS.OpsWorks.DeleteLayer

    -- ** UpdateLayer
    , module Network.AWS.OpsWorks.UpdateLayer

    -- ** CreateStack
    , module Network.AWS.OpsWorks.CreateStack

    -- ** UpdateElasticIP
    , module Network.AWS.OpsWorks.UpdateElasticIP

    -- ** CreateApp
    , module Network.AWS.OpsWorks.CreateApp

    -- ** GetHostnameSuggestion
    , module Network.AWS.OpsWorks.GetHostnameSuggestion

    -- ** CloneStack
    , module Network.AWS.OpsWorks.CloneStack

    -- ** DescribePermissions
    , module Network.AWS.OpsWorks.DescribePermissions

    -- ** DetachElasticLoadBalancer
    , module Network.AWS.OpsWorks.DetachElasticLoadBalancer

    -- ** RegisterInstance
    , module Network.AWS.OpsWorks.RegisterInstance

    -- ** AssociateElasticIP
    , module Network.AWS.OpsWorks.AssociateElasticIP

    -- ** DescribeLoadBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling

    -- ** DescribeStackProvisioningParameters
    , module Network.AWS.OpsWorks.DescribeStackProvisioningParameters

    -- ** TagResource
    , module Network.AWS.OpsWorks.TagResource

    -- ** ListTags
    , module Network.AWS.OpsWorks.ListTags

    -- ** UnassignInstance
    , module Network.AWS.OpsWorks.UnassignInstance

    -- ** DescribeMyUserProfile
    , module Network.AWS.OpsWorks.DescribeMyUserProfile

    -- ** DeleteUserProfile
    , module Network.AWS.OpsWorks.DeleteUserProfile

    -- ** UpdateUserProfile
    , module Network.AWS.OpsWorks.UpdateUserProfile

    -- ** DescribeServiceErrors
    , module Network.AWS.OpsWorks.DescribeServiceErrors

    -- ** RegisterRDSDBInstance
    , module Network.AWS.OpsWorks.RegisterRDSDBInstance

    -- ** UntagResource
    , module Network.AWS.OpsWorks.UntagResource

    -- ** StartStack
    , module Network.AWS.OpsWorks.StartStack

    -- ** CreateUserProfile
    , module Network.AWS.OpsWorks.CreateUserProfile

    -- ** DescribeOperatingSystems
    , module Network.AWS.OpsWorks.DescribeOperatingSystems

    -- ** DescribeCommands
    , module Network.AWS.OpsWorks.DescribeCommands

    -- ** AssignVolume
    , module Network.AWS.OpsWorks.AssignVolume

    -- ** DescribeElasticLoadBalancers
    , module Network.AWS.OpsWorks.DescribeElasticLoadBalancers

    -- ** SetPermission
    , module Network.AWS.OpsWorks.SetPermission

    -- ** DeregisterInstance
    , module Network.AWS.OpsWorks.DeregisterInstance

    -- ** DescribeEcsClusters (Paginated)
    , module Network.AWS.OpsWorks.DescribeEcsClusters

    -- ** DescribeRAIdArrays
    , module Network.AWS.OpsWorks.DescribeRAIdArrays

    -- ** UpdateVolume
    , module Network.AWS.OpsWorks.UpdateVolume

    -- ** StartInstance
    , module Network.AWS.OpsWorks.StartInstance

    -- * Types

    -- ** AppAttributesKeys
    , AppAttributesKeys (..)

    -- ** AppType
    , AppType (..)

    -- ** Architecture
    , Architecture (..)

    -- ** AutoScalingType
    , AutoScalingType (..)

    -- ** CloudWatchLogsEncoding
    , CloudWatchLogsEncoding (..)

    -- ** CloudWatchLogsInitialPosition
    , CloudWatchLogsInitialPosition (..)

    -- ** CloudWatchLogsTimeZone
    , CloudWatchLogsTimeZone (..)

    -- ** DeploymentCommandName
    , DeploymentCommandName (..)

    -- ** LayerAttributesKeys
    , LayerAttributesKeys (..)

    -- ** LayerType
    , LayerType (..)

    -- ** RootDeviceType
    , RootDeviceType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** StackAttributesKeys
    , StackAttributesKeys (..)

    -- ** VirtualizationType
    , VirtualizationType (..)

    -- ** VolumeType
    , VolumeType (..)

    -- ** AgentVersion
    , AgentVersion
    , agentVersion
    , avVersion
    , avConfigurationManager

    -- ** App
    , App
    , app
    , appSSLConfiguration
    , appEnvironment
    , appEnableSSL
    , appCreatedAt
    , appShortname
    , appDataSources
    , appAppSource
    , appAppId
    , appAttributes
    , appName
    , appType
    , appStackId
    , appDomains
    , appDescription

    -- ** AutoScalingThresholds
    , AutoScalingThresholds
    , autoScalingThresholds
    , astInstanceCount
    , astIgnoreMetricsTime
    , astLoadThreshold
    , astThresholdsWaitTime
    , astAlarms
    , astMemoryThreshold
    , astCPUThreshold

    -- ** BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- ** ChefConfiguration
    , ChefConfiguration
    , chefConfiguration
    , ccBerkshelfVersion
    , ccManageBerkshelf

    -- ** CloudWatchLogsConfiguration
    , CloudWatchLogsConfiguration
    , cloudWatchLogsConfiguration
    , cwlcEnabled
    , cwlcLogStreams

    -- ** CloudWatchLogsLogStream
    , CloudWatchLogsLogStream
    , cloudWatchLogsLogStream
    , cwllsBatchCount
    , cwllsFileFingerprintLines
    , cwllsBufferDuration
    , cwllsBatchSize
    , cwllsLogGroupName
    , cwllsMultiLineStartPattern
    , cwllsInitialPosition
    , cwllsDatetimeFormat
    , cwllsEncoding
    , cwllsTimeZone
    , cwllsFile

    -- ** Command
    , Command
    , command
    , cDeploymentId
    , cInstanceId
    , cStatus
    , cLogURL
    , cCreatedAt
    , cCommandId
    , cExitCode
    , cType
    , cCompletedAt
    , cAcknowledgedAt

    -- ** DataSource
    , DataSource
    , dataSource
    , dsARN
    , dsDatabaseName
    , dsType

    -- ** Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dStatus
    , dCommand
    , dCreatedAt
    , dCustomJSON
    , dIAMUserARN
    , dAppId
    , dInstanceIds
    , dCompletedAt
    , dStackId
    , dComment
    , dDuration

    -- ** DeploymentCommand
    , DeploymentCommand
    , deploymentCommand
    , dcArgs
    , dcName

    -- ** EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdVolumeSize
    , ebdIOPS
    , ebdVolumeType
    , ebdSnapshotId

    -- ** EcsCluster
    , EcsCluster
    , ecsCluster
    , ecEcsClusterARN
    , ecEcsClusterName
    , ecRegisteredAt
    , ecStackId

    -- ** ElasticIP
    , ElasticIP
    , elasticIP
    , eiInstanceId
    , eiDomain
    , eiIP
    , eiName
    , eiRegion

    -- ** ElasticLoadBalancer
    , ElasticLoadBalancer
    , elasticLoadBalancer
    , elbSubnetIds
    , elbVPCId
    , elbAvailabilityZones
    , elbRegion
    , elbElasticLoadBalancerName
    , elbStackId
    , elbEC2InstanceIds
    , elbLayerId
    , elbDNSName

    -- ** EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evSecure
    , evKey
    , evValue

    -- ** Instance
    , Instance
    , instance'
    , iPrivateDNS
    , iReportedAgentVersion
    , iInstanceId
    , iStatus
    , iPrivateIP
    , iInstallUpdatesOnBoot
    , iVirtualizationType
    , iInstanceProfileARN
    , iPlatform
    , iHostname
    , iSSHHostRsaKeyFingerprint
    , iSecurityGroupIds
    , iEcsClusterARN
    , iARN
    , iCreatedAt
    , iEC2InstanceId
    , iSSHKeyName
    , iAgentVersion
    , iRootDeviceVolumeId
    , iSubnetId
    , iInfrastructureClass
    , iSSHHostDsaKeyFingerprint
    , iInstanceType
    , iEBSOptimized
    , iElasticIP
    , iOS
    , iAvailabilityZone
    , iLastServiceErrorId
    , iTenancy
    , iAutoScalingType
    , iLayerIds
    , iArchitecture
    , iPublicDNS
    , iAMIId
    , iPublicIP
    , iReportedOS
    , iRegisteredBy
    , iStackId
    , iRootDeviceType
    , iEcsContainerInstanceARN
    , iBlockDeviceMappings

    -- ** InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- ** InstancesCount
    , InstancesCount
    , instancesCount
    , icTerminating
    , icPending
    , icOnline
    , icUnassigning
    , icDeregistering
    , icRunningSetup
    , icRequested
    , icStopFailed
    , icBooting
    , icStopped
    , icRebooting
    , icAssigning
    , icShuttingDown
    , icSetupFailed
    , icConnectionLost
    , icTerminated
    , icStopping
    , icRegistered
    , icStartFailed
    , icRegistering

    -- ** Layer
    , Layer
    , layer
    , lCustomInstanceProfileARN
    , lCustomSecurityGroupIds
    , lInstallUpdatesOnBoot
    , lCloudWatchLogsConfiguration
    , lLifecycleEventConfiguration
    , lARN
    , lCreatedAt
    , lShortname
    , lDefaultRecipes
    , lCustomRecipes
    , lCustomJSON
    , lVolumeConfigurations
    , lEnableAutoHealing
    , lPackages
    , lAttributes
    , lName
    , lAutoAssignPublicIPs
    , lType
    , lUseEBSOptimizedInstances
    , lStackId
    , lLayerId
    , lDefaultSecurityGroupNames
    , lAutoAssignElasticIPs

    -- ** LifecycleEventConfiguration
    , LifecycleEventConfiguration
    , lifecycleEventConfiguration
    , lecShutdown

    -- ** LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , loadBasedAutoScalingConfiguration
    , lbascUpScaling
    , lbascEnable
    , lbascDownScaling
    , lbascLayerId

    -- ** OperatingSystem
    , OperatingSystem
    , operatingSystem
    , osReportedVersion
    , osSupported
    , osName
    , osId
    , osConfigurationManagers
    , osType
    , osReportedName

    -- ** OperatingSystemConfigurationManager
    , OperatingSystemConfigurationManager
    , operatingSystemConfigurationManager
    , oscmName
    , oscmVersion

    -- ** Permission
    , Permission
    , permission
    , pIAMUserARN
    , pAllowSudo
    , pStackId
    , pLevel
    , pAllowSSH

    -- ** RAIdArray
    , RAIdArray
    , rAIdArray
    , raiaInstanceId
    , raiaSize
    , raiaIOPS
    , raiaCreatedAt
    , raiaRAIdLevel
    , raiaDevice
    , raiaNumberOfDisks
    , raiaAvailabilityZone
    , raiaName
    , raiaRAIdArrayId
    , raiaVolumeType
    , raiaStackId
    , raiaMountPoint

    -- ** RDSDBInstance
    , RDSDBInstance
    , rdsDBInstance
    , rdiRDSDBInstanceARN
    , rdiDBUser
    , rdiMissingOnRDS
    , rdiEngine
    , rdiAddress
    , rdiDBInstanceIdentifier
    , rdiRegion
    , rdiStackId
    , rdiDBPassword

    -- ** Recipes
    , Recipes
    , recipes
    , rSetup
    , rShutdown
    , rUndeploy
    , rConfigure
    , rDeploy

    -- ** ReportedOS
    , ReportedOS
    , reportedOS
    , roFamily
    , roName
    , roVersion

    -- ** SSLConfiguration
    , SSLConfiguration
    , sslConfiguration
    , scPrivateKey
    , scCertificate
    , scChain

    -- ** SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supSSHPublicKey
    , supSSHUsername
    , supIAMUserARN
    , supName

    -- ** ServiceError'
    , ServiceError'
    , serviceError'
    , seInstanceId
    , seCreatedAt
    , seServiceErrorId
    , seType
    , seStackId
    , seMessage

    -- ** ShutdownEventConfiguration
    , ShutdownEventConfiguration
    , shutdownEventConfiguration
    , secExecutionTimeout
    , secDelayUntilElbConnectionsDrained

    -- ** Source
    , Source
    , source
    , sURL
    , sUsername
    , sSSHKey
    , sPassword
    , sType
    , sRevision

    -- ** Stack
    , Stack
    , stack
    , sDefaultInstanceProfileARN
    , sServiceRoleARN
    , sDefaultRootDeviceType
    , sARN
    , sCreatedAt
    , sVPCId
    , sChefConfiguration
    , sAgentVersion
    , sDefaultSSHKeyName
    , sCustomJSON
    , sCustomCookbooksSource
    , sDefaultAvailabilityZone
    , sAttributes
    , sName
    , sDefaultOS
    , sUseOpsworksSecurityGroups
    , sUseCustomCookbooks
    , sDefaultSubnetId
    , sRegion
    , sConfigurationManager
    , sStackId
    , sHostnameTheme

    -- ** StackConfigurationManager
    , StackConfigurationManager
    , stackConfigurationManager
    , scmName
    , scmVersion

    -- ** StackSummary
    , StackSummary
    , stackSummary
    , ssARN
    , ssAppsCount
    , ssName
    , ssStackId
    , ssLayersCount
    , ssInstancesCount

    -- ** TemporaryCredential
    , TemporaryCredential
    , temporaryCredential
    , tcInstanceId
    , tcUsername
    , tcPassword
    , tcValidForInMinutes

    -- ** TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , timeBasedAutoScalingConfiguration
    , tbascInstanceId
    , tbascAutoScalingSchedule

    -- ** UserProfile
    , UserProfile
    , userProfile
    , upAllowSelfManagement
    , upSSHPublicKey
    , upSSHUsername
    , upIAMUserARN
    , upName

    -- ** Volume
    , Volume
    , volume
    , vInstanceId
    , vStatus
    , vSize
    , vIOPS
    , vDevice
    , vEncrypted
    , vAvailabilityZone
    , vName
    , vRAIdArrayId
    , vVolumeId
    , vRegion
    , vVolumeType
    , vEC2VolumeId
    , vMountPoint

    -- ** VolumeConfiguration
    , VolumeConfiguration
    , volumeConfiguration
    , vcIOPS
    , vcRAIdLevel
    , vcEncrypted
    , vcVolumeType
    , vcMountPoint
    , vcNumberOfDisks
    , vcSize

    -- ** WeeklyAutoScalingSchedule
    , WeeklyAutoScalingSchedule
    , weeklyAutoScalingSchedule
    , wassThursday
    , wassWednesday
    , wassSaturday
    , wassMonday
    , wassFriday
    , wassSunday
    , wassTuesday
    ) where

import Network.AWS.OpsWorks.AssignInstance
import Network.AWS.OpsWorks.AssignVolume
import Network.AWS.OpsWorks.AssociateElasticIP
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
import Network.AWS.OpsWorks.DeregisterElasticIP
import Network.AWS.OpsWorks.DeregisterInstance
import Network.AWS.OpsWorks.DeregisterRDSDBInstance
import Network.AWS.OpsWorks.DeregisterVolume
import Network.AWS.OpsWorks.DescribeAgentVersions
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeCommands
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeEcsClusters
import Network.AWS.OpsWorks.DescribeElasticIPs
import Network.AWS.OpsWorks.DescribeElasticLoadBalancers
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeLayers
import Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
import Network.AWS.OpsWorks.DescribeMyUserProfile
import Network.AWS.OpsWorks.DescribeOperatingSystems
import Network.AWS.OpsWorks.DescribePermissions
import Network.AWS.OpsWorks.DescribeRAIdArrays
import Network.AWS.OpsWorks.DescribeRDSDBInstances
import Network.AWS.OpsWorks.DescribeServiceErrors
import Network.AWS.OpsWorks.DescribeStackProvisioningParameters
import Network.AWS.OpsWorks.DescribeStacks
import Network.AWS.OpsWorks.DescribeStackSummary
import Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
import Network.AWS.OpsWorks.DescribeUserProfiles
import Network.AWS.OpsWorks.DescribeVolumes
import Network.AWS.OpsWorks.DetachElasticLoadBalancer
import Network.AWS.OpsWorks.DisassociateElasticIP
import Network.AWS.OpsWorks.GetHostnameSuggestion
import Network.AWS.OpsWorks.GrantAccess
import Network.AWS.OpsWorks.ListTags
import Network.AWS.OpsWorks.RebootInstance
import Network.AWS.OpsWorks.RegisterEcsCluster
import Network.AWS.OpsWorks.RegisterElasticIP
import Network.AWS.OpsWorks.RegisterInstance
import Network.AWS.OpsWorks.RegisterRDSDBInstance
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
import Network.AWS.OpsWorks.UpdateElasticIP
import Network.AWS.OpsWorks.UpdateInstance
import Network.AWS.OpsWorks.UpdateLayer
import Network.AWS.OpsWorks.UpdateMyUserProfile
import Network.AWS.OpsWorks.UpdateRDSDBInstance
import Network.AWS.OpsWorks.UpdateStack
import Network.AWS.OpsWorks.UpdateUserProfile
import Network.AWS.OpsWorks.UpdateVolume
import Network.AWS.OpsWorks.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'OpsWorks'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
