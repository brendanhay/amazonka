{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS OpsWorks
--
-- Welcome to the /AWS OpsWorks API Reference/. This guide provides
-- descriptions, syntax, and usage examples about AWS OpsWorks actions and
-- data types, including common parameters and error codes.
--
-- AWS OpsWorks is an application management service that provides an
-- integrated experience for overseeing the complete application lifecycle.
-- For information about this product, go to the
-- <http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.
--
-- __SDKs and CLI__
--
-- The most common way to use the AWS OpsWorks API is by using the AWS
-- Command Line Interface (CLI) or by using one of the AWS SDKs to
-- implement applications in your preferred language. For more information,
-- see:
--
-- -   <http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
-- -   <http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
-- -   <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
-- -   <http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
-- -   <http://docs.aws.amazon.com/AWSRubySDK/latest/AWS/OpsWorks/Client.html AWS SDK for Ruby>
-- -   <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
-- -   <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>
--
-- __Endpoints__
--
-- AWS OpsWorks supports only one endpoint,
-- opsworks.us-east-1.amazonaws.com (HTTPS), so you must connect to that
-- endpoint. You can then use the API to direct AWS OpsWorks to create
-- stacks in any AWS Region.
--
-- __Chef Versions__
--
-- When you call CreateStack, CloneStack, or UpdateStack we recommend you
-- use the @ConfigurationManager@ parameter to specify the Chef version.
-- The recommended value for Linux stacks, which is also the default value,
-- is currently 11.10. Windows stacks use Chef 12.2. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions>.
--
-- You can also specify Chef 11.4 or Chef 0.9 for your Linux stack.
-- However, Chef 0.9 has been deprecated. We do not recommend using Chef
-- 0.9 for new stacks, and we recommend migrating your existing Chef 0.9
-- stacks to Chef 11.10 as soon as possible.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.OpsWorks
    (
    -- * Service Description
      OpsWorks

    -- * Error Matchers
    -- $errors
    , _ValidationException
    , _ResourceNotFoundException

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

    -- ** UnassignVolume
    , module Network.AWS.OpsWorks.UnassignVolume

    -- ** DeregisterRDSDBInstance
    , module Network.AWS.OpsWorks.DeregisterRDSDBInstance

    -- ** CreateInstance
    , module Network.AWS.OpsWorks.CreateInstance

    -- ** RegisterElasticIP
    , module Network.AWS.OpsWorks.RegisterElasticIP

    -- ** DescribeAgentVersions
    , module Network.AWS.OpsWorks.DescribeAgentVersions

    -- ** DescribeLayers
    , module Network.AWS.OpsWorks.DescribeLayers

    -- ** CreateDeployment
    , module Network.AWS.OpsWorks.CreateDeployment

    -- ** DeleteApp
    , module Network.AWS.OpsWorks.DeleteApp

    -- ** UpdateApp
    , module Network.AWS.OpsWorks.UpdateApp

    -- ** DeleteInstance
    , module Network.AWS.OpsWorks.DeleteInstance

    -- ** UpdateInstance
    , module Network.AWS.OpsWorks.UpdateInstance

    -- ** DescribeStacks
    , module Network.AWS.OpsWorks.DescribeStacks

    -- ** DeregisterVolume
    , module Network.AWS.OpsWorks.DeregisterVolume

    -- ** AssignInstance
    , module Network.AWS.OpsWorks.AssignInstance

    -- ** RebootInstance
    , module Network.AWS.OpsWorks.RebootInstance

    -- ** DescribeTimeBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling

    -- ** UpdateRDSDBInstance
    , module Network.AWS.OpsWorks.UpdateRDSDBInstance

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

    -- ** DeregisterElasticIP
    , module Network.AWS.OpsWorks.DeregisterElasticIP

    -- ** AttachElasticLoadBalancer
    , module Network.AWS.OpsWorks.AttachElasticLoadBalancer

    -- ** DescribeUserProfiles
    , module Network.AWS.OpsWorks.DescribeUserProfiles

    -- ** DescribeStackSummary
    , module Network.AWS.OpsWorks.DescribeStackSummary

    -- ** DeregisterEcsCluster
    , module Network.AWS.OpsWorks.DeregisterEcsCluster

    -- ** DescribeApps
    , module Network.AWS.OpsWorks.DescribeApps

    -- ** UpdateMyUserProfile
    , module Network.AWS.OpsWorks.UpdateMyUserProfile

    -- ** DescribeInstances
    , module Network.AWS.OpsWorks.DescribeInstances

    -- ** DescribeDeployments
    , module Network.AWS.OpsWorks.DescribeDeployments

    -- ** CreateStack
    , module Network.AWS.OpsWorks.CreateStack

    -- ** GrantAccess
    , module Network.AWS.OpsWorks.GrantAccess

    -- ** DescribeElasticIPs
    , module Network.AWS.OpsWorks.DescribeElasticIPs

    -- ** DeleteLayer
    , module Network.AWS.OpsWorks.DeleteLayer

    -- ** UpdateLayer
    , module Network.AWS.OpsWorks.UpdateLayer

    -- ** CloneStack
    , module Network.AWS.OpsWorks.CloneStack

    -- ** GetHostnameSuggestion
    , module Network.AWS.OpsWorks.GetHostnameSuggestion

    -- ** CreateApp
    , module Network.AWS.OpsWorks.CreateApp

    -- ** DescribePermissions
    , module Network.AWS.OpsWorks.DescribePermissions

    -- ** UpdateElasticIP
    , module Network.AWS.OpsWorks.UpdateElasticIP

    -- ** DescribeLoadBasedAutoScaling
    , module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling

    -- ** RegisterInstance
    , module Network.AWS.OpsWorks.RegisterInstance

    -- ** AssociateElasticIP
    , module Network.AWS.OpsWorks.AssociateElasticIP

    -- ** DetachElasticLoadBalancer
    , module Network.AWS.OpsWorks.DetachElasticLoadBalancer

    -- ** DescribeStackProvisioningParameters
    , module Network.AWS.OpsWorks.DescribeStackProvisioningParameters

    -- ** DescribeMyUserProfile
    , module Network.AWS.OpsWorks.DescribeMyUserProfile

    -- ** UnassignInstance
    , module Network.AWS.OpsWorks.UnassignInstance

    -- ** RegisterRDSDBInstance
    , module Network.AWS.OpsWorks.RegisterRDSDBInstance

    -- ** DeleteUserProfile
    , module Network.AWS.OpsWorks.DeleteUserProfile

    -- ** UpdateUserProfile
    , module Network.AWS.OpsWorks.UpdateUserProfile

    -- ** DescribeServiceErrors
    , module Network.AWS.OpsWorks.DescribeServiceErrors

    -- ** StartStack
    , module Network.AWS.OpsWorks.StartStack

    -- ** CreateUserProfile
    , module Network.AWS.OpsWorks.CreateUserProfile

    -- ** DescribeCommands
    , module Network.AWS.OpsWorks.DescribeCommands

    -- ** DescribeEcsClusters
    , module Network.AWS.OpsWorks.DescribeEcsClusters

    -- ** DescribeElasticLoadBalancers
    , module Network.AWS.OpsWorks.DescribeElasticLoadBalancers

    -- ** DeregisterInstance
    , module Network.AWS.OpsWorks.DeregisterInstance

    -- ** DescribeRAIdArrays
    , module Network.AWS.OpsWorks.DescribeRAIdArrays

    -- ** SetPermission
    , module Network.AWS.OpsWorks.SetPermission

    -- ** UpdateVolume
    , module Network.AWS.OpsWorks.UpdateVolume

    -- ** AssignVolume
    , module Network.AWS.OpsWorks.AssignVolume

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
    , appShortname
    , appEnableSSL
    , appCreatedAt
    , appEnvironment
    , appDataSources
    , appAppId
    , appAppSource
    , appName
    , appAttributes
    , appType
    , appDomains
    , appStackId
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

    -- ** Command
    , Command
    , command
    , cInstanceId
    , cDeploymentId
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
    , elbEC2InstanceIds
    , elbStackId
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
    , iInstanceId
    , iPrivateIP
    , iInstallUpdatesOnBoot
    , iReportedAgentVersion
    , iStatus
    , iPrivateDNS
    , iVirtualizationType
    , iSecurityGroupIds
    , iSSHHostRsaKeyFingerprint
    , iInstanceProfileARN
    , iPlatform
    , iHostname
    , iEcsClusterARN
    , iCreatedAt
    , iSSHKeyName
    , iEC2InstanceId
    , iAgentVersion
    , iRootDeviceVolumeId
    , iSubnetId
    , iInstanceType
    , iInfrastructureClass
    , iEBSOptimized
    , iSSHHostDsaKeyFingerprint
    , iElasticIP
    , iOS
    , iAvailabilityZone
    , iLastServiceErrorId
    , iAutoScalingType
    , iLayerIds
    , iArchitecture
    , iPublicDNS
    , iPublicIP
    , iAMIId
    , iReportedOS
    , iStackId
    , iRegisteredBy
    , iBlockDeviceMappings
    , iEcsContainerInstanceARN
    , iRootDeviceType

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
    , icRequested
    , icRunningSetup
    , icDeregistering
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
    , lInstallUpdatesOnBoot
    , lCustomSecurityGroupIds
    , lLifecycleEventConfiguration
    , lShortname
    , lCreatedAt
    , lDefaultRecipes
    , lCustomRecipes
    , lVolumeConfigurations
    , lCustomJSON
    , lEnableAutoHealing
    , lPackages
    , lName
    , lAttributes
    , lAutoAssignPublicIPs
    , lUseEBSOptimizedInstances
    , lType
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
    , raiaCreatedAt
    , raiaIOPS
    , raiaRAIdLevel
    , raiaDevice
    , raiaNumberOfDisks
    , raiaName
    , raiaAvailabilityZone
    , raiaRAIdArrayId
    , raiaVolumeType
    , raiaStackId
    , raiaMountPoint

    -- ** RDSDBInstance
    , RDSDBInstance
    , rdsDBInstance
    , rdiDBUser
    , rdiRDSDBInstanceARN
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
    , rUndeploy
    , rShutdown
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
    , scChain
    , scCertificate
    , scPrivateKey

    -- ** SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supSSHUsername
    , supSSHPublicKey
    , supIAMUserARN
    , supName

    -- ** ServiceError'
    , ServiceError'
    , serviceError'
    , seInstanceId
    , seCreatedAt
    , seServiceErrorId
    , seType
    , seMessage
    , seStackId

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
    , sARN
    , sDefaultRootDeviceType
    , sCreatedAt
    , sChefConfiguration
    , sVPCId
    , sAgentVersion
    , sDefaultSSHKeyName
    , sCustomJSON
    , sCustomCookbooksSource
    , sDefaultAvailabilityZone
    , sName
    , sUseOpsworksSecurityGroups
    , sDefaultOS
    , sAttributes
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
    , upSSHUsername
    , upSSHPublicKey
    , upAllowSelfManagement
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
    , vName
    , vAvailabilityZone
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

import           Network.AWS.OpsWorks.AssignInstance
import           Network.AWS.OpsWorks.AssignVolume
import           Network.AWS.OpsWorks.AssociateElasticIP
import           Network.AWS.OpsWorks.AttachElasticLoadBalancer
import           Network.AWS.OpsWorks.CloneStack
import           Network.AWS.OpsWorks.CreateApp
import           Network.AWS.OpsWorks.CreateDeployment
import           Network.AWS.OpsWorks.CreateInstance
import           Network.AWS.OpsWorks.CreateLayer
import           Network.AWS.OpsWorks.CreateStack
import           Network.AWS.OpsWorks.CreateUserProfile
import           Network.AWS.OpsWorks.DeleteApp
import           Network.AWS.OpsWorks.DeleteInstance
import           Network.AWS.OpsWorks.DeleteLayer
import           Network.AWS.OpsWorks.DeleteStack
import           Network.AWS.OpsWorks.DeleteUserProfile
import           Network.AWS.OpsWorks.DeregisterEcsCluster
import           Network.AWS.OpsWorks.DeregisterElasticIP
import           Network.AWS.OpsWorks.DeregisterInstance
import           Network.AWS.OpsWorks.DeregisterRDSDBInstance
import           Network.AWS.OpsWorks.DeregisterVolume
import           Network.AWS.OpsWorks.DescribeAgentVersions
import           Network.AWS.OpsWorks.DescribeApps
import           Network.AWS.OpsWorks.DescribeCommands
import           Network.AWS.OpsWorks.DescribeDeployments
import           Network.AWS.OpsWorks.DescribeEcsClusters
import           Network.AWS.OpsWorks.DescribeElasticIPs
import           Network.AWS.OpsWorks.DescribeElasticLoadBalancers
import           Network.AWS.OpsWorks.DescribeInstances
import           Network.AWS.OpsWorks.DescribeLayers
import           Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
import           Network.AWS.OpsWorks.DescribeMyUserProfile
import           Network.AWS.OpsWorks.DescribePermissions
import           Network.AWS.OpsWorks.DescribeRAIdArrays
import           Network.AWS.OpsWorks.DescribeRDSDBInstances
import           Network.AWS.OpsWorks.DescribeServiceErrors
import           Network.AWS.OpsWorks.DescribeStackProvisioningParameters
import           Network.AWS.OpsWorks.DescribeStacks
import           Network.AWS.OpsWorks.DescribeStackSummary
import           Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
import           Network.AWS.OpsWorks.DescribeUserProfiles
import           Network.AWS.OpsWorks.DescribeVolumes
import           Network.AWS.OpsWorks.DetachElasticLoadBalancer
import           Network.AWS.OpsWorks.DisassociateElasticIP
import           Network.AWS.OpsWorks.GetHostnameSuggestion
import           Network.AWS.OpsWorks.GrantAccess
import           Network.AWS.OpsWorks.RebootInstance
import           Network.AWS.OpsWorks.RegisterEcsCluster
import           Network.AWS.OpsWorks.RegisterElasticIP
import           Network.AWS.OpsWorks.RegisterInstance
import           Network.AWS.OpsWorks.RegisterRDSDBInstance
import           Network.AWS.OpsWorks.RegisterVolume
import           Network.AWS.OpsWorks.SetLoadBasedAutoScaling
import           Network.AWS.OpsWorks.SetPermission
import           Network.AWS.OpsWorks.SetTimeBasedAutoScaling
import           Network.AWS.OpsWorks.StartInstance
import           Network.AWS.OpsWorks.StartStack
import           Network.AWS.OpsWorks.StopInstance
import           Network.AWS.OpsWorks.StopStack
import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.UnassignInstance
import           Network.AWS.OpsWorks.UnassignVolume
import           Network.AWS.OpsWorks.UpdateApp
import           Network.AWS.OpsWorks.UpdateElasticIP
import           Network.AWS.OpsWorks.UpdateInstance
import           Network.AWS.OpsWorks.UpdateLayer
import           Network.AWS.OpsWorks.UpdateMyUserProfile
import           Network.AWS.OpsWorks.UpdateRDSDBInstance
import           Network.AWS.OpsWorks.UpdateStack
import           Network.AWS.OpsWorks.UpdateUserProfile
import           Network.AWS.OpsWorks.UpdateVolume
import           Network.AWS.OpsWorks.Waiters

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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
