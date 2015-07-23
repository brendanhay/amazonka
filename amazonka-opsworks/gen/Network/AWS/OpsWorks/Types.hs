{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types
    (
    -- * Service
      OpsWorks

    -- * Errors
    , _ValidationException
    , _ResourceNotFoundException

    -- * AppAttributesKeys
    , AppAttributesKeys (..)

    -- * AppType
    , AppType (..)

    -- * Architecture
    , Architecture (..)

    -- * AutoScalingType
    , AutoScalingType (..)

    -- * DeploymentCommandName
    , DeploymentCommandName (..)

    -- * LayerAttributesKeys
    , LayerAttributesKeys (..)

    -- * LayerType
    , LayerType (..)

    -- * RootDeviceType
    , RootDeviceType (..)

    -- * SourceType
    , SourceType (..)

    -- * StackAttributesKeys
    , StackAttributesKeys (..)

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * VolumeType
    , VolumeType (..)

    -- * AgentVersion
    , AgentVersion
    , agentVersion
    , avVersion
    , avConfigurationManager

    -- * App
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

    -- * AutoScalingThresholds
    , AutoScalingThresholds
    , autoScalingThresholds
    , astInstanceCount
    , astIgnoreMetricsTime
    , astLoadThreshold
    , astThresholdsWaitTime
    , astAlarms
    , astMemoryThreshold
    , astCPUThreshold

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- * ChefConfiguration
    , ChefConfiguration
    , chefConfiguration
    , ccBerkshelfVersion
    , ccManageBerkshelf

    -- * Command
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

    -- * DataSource
    , DataSource
    , dataSource
    , dsARN
    , dsDatabaseName
    , dsType

    -- * Deployment
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

    -- * DeploymentCommand
    , DeploymentCommand
    , deploymentCommand
    , dcArgs
    , dcName

    -- * EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdVolumeSize
    , ebdIOPS
    , ebdVolumeType
    , ebdSnapshotId

    -- * ElasticIP
    , ElasticIP
    , elasticIP
    , eiInstanceId
    , eiDomain
    , eiIP
    , eiName
    , eiRegion

    -- * ElasticLoadBalancer
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

    -- * EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evSecure
    , evKey
    , evValue

    -- * Instance
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
    , iRootDeviceType

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- * InstancesCount
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

    -- * Layer
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

    -- * LifecycleEventConfiguration
    , LifecycleEventConfiguration
    , lifecycleEventConfiguration
    , lecShutdown

    -- * LoadBasedAutoScalingConfiguration
    , LoadBasedAutoScalingConfiguration
    , loadBasedAutoScalingConfiguration
    , lbascUpScaling
    , lbascEnable
    , lbascDownScaling
    , lbascLayerId

    -- * Permission
    , Permission
    , permission
    , pIAMUserARN
    , pAllowSudo
    , pStackId
    , pLevel
    , pAllowSSH

    -- * RAIdArray
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

    -- * RDSDBInstance
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

    -- * Recipes
    , Recipes
    , recipes
    , rSetup
    , rUndeploy
    , rShutdown
    , rConfigure
    , rDeploy

    -- * ReportedOS
    , ReportedOS
    , reportedOS
    , roFamily
    , roName
    , roVersion

    -- * SSLConfiguration
    , SSLConfiguration
    , sslConfiguration
    , scChain
    , scCertificate
    , scPrivateKey

    -- * SelfUserProfile
    , SelfUserProfile
    , selfUserProfile
    , supSSHUsername
    , supSSHPublicKey
    , supIAMUserARN
    , supName

    -- * ServiceError'
    , ServiceError'
    , serviceError'
    , seInstanceId
    , seCreatedAt
    , seServiceErrorId
    , seType
    , seMessage
    , seStackId

    -- * ShutdownEventConfiguration
    , ShutdownEventConfiguration
    , shutdownEventConfiguration
    , secExecutionTimeout
    , secDelayUntilElbConnectionsDrained

    -- * Source
    , Source
    , source
    , sURL
    , sUsername
    , sSSHKey
    , sPassword
    , sType
    , sRevision

    -- * Stack
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

    -- * StackConfigurationManager
    , StackConfigurationManager
    , stackConfigurationManager
    , scmName
    , scmVersion

    -- * StackSummary
    , StackSummary
    , stackSummary
    , ssARN
    , ssAppsCount
    , ssName
    , ssStackId
    , ssLayersCount
    , ssInstancesCount

    -- * TemporaryCredential
    , TemporaryCredential
    , temporaryCredential
    , tcInstanceId
    , tcUsername
    , tcPassword
    , tcValidForInMinutes

    -- * TimeBasedAutoScalingConfiguration
    , TimeBasedAutoScalingConfiguration
    , timeBasedAutoScalingConfiguration
    , tbascInstanceId
    , tbascAutoScalingSchedule

    -- * UserProfile
    , UserProfile
    , userProfile
    , upSSHUsername
    , upSSHPublicKey
    , upAllowSelfManagement
    , upIAMUserARN
    , upName

    -- * Volume
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

    -- * VolumeConfiguration
    , VolumeConfiguration
    , volumeConfiguration
    , vcIOPS
    , vcRAIdLevel
    , vcVolumeType
    , vcMountPoint
    , vcNumberOfDisks
    , vcSize

    -- * WeeklyAutoScalingSchedule
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

import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.OpsWorks.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-02-18@ of the Amazon OpsWorks SDK.
data OpsWorks

instance AWSService OpsWorks where
    type Sg OpsWorks = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "OpsWorks"
            , _svcPrefix = "opsworks"
            , _svcVersion = "2013-02-18"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Indicates that a request was invalid.
_ValidationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | Indicates that a resource was not found.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"
