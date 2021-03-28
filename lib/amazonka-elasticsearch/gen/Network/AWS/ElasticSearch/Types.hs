-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ValidationException
    , _AccessDeniedException
    , _ResourceAlreadyExistsException
    , _ConflictException
    , _BaseException
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _ResourceNotFoundException
    , _InvalidPaginationTokenException
    , _LimitExceededException

    -- * AdvancedSecurityOptionsStatus
    , AdvancedSecurityOptionsStatus (..)
    , mkAdvancedSecurityOptionsStatus
    , asosOptions
    , asosStatus

    -- * EBSOptions
    , EBSOptions (..)
    , mkEBSOptions
    , ebsoEBSEnabled
    , ebsoIops
    , ebsoVolumeSize
    , ebsoVolumeType

    -- * SAMLOptionsOutput
    , SAMLOptionsOutput (..)
    , mkSAMLOptionsOutput
    , samlooEnabled
    , samlooIdp
    , samlooRolesKey
    , samlooSessionTimeoutMinutes
    , samlooSubjectKey

    -- * UpgradeStatus
    , UpgradeStatus (..)

    -- * NodeToNodeEncryptionOptions
    , NodeToNodeEncryptionOptions (..)
    , mkNodeToNodeEncryptionOptions
    , ntneoEnabled

    -- * AdditionalLimit
    , AdditionalLimit (..)
    , mkAdditionalLimit
    , alLimitName
    , alLimitValues

    -- * OptionState
    , OptionState (..)

    -- * DescribePackagesFilter
    , DescribePackagesFilter (..)
    , mkDescribePackagesFilter
    , dpfName
    , dpfValue

    -- * PackageSource
    , PackageSource (..)
    , mkPackageSource
    , psS3BucketName
    , psS3Key

    -- * PolicyDocument
    , PolicyDocument (..)

    -- * UpgradeStepItem
    , UpgradeStepItem (..)
    , mkUpgradeStepItem
    , usiIssues
    , usiProgressPercent
    , usiUpgradeStep
    , usiUpgradeStepStatus

    -- * InstanceLimits
    , InstanceLimits (..)
    , mkInstanceLimits
    , ilInstanceCountLimits

    -- * ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus (..)
    , mkElasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- * DescribePackagesFilterValue
    , DescribePackagesFilterValue (..)

    -- * PackageID
    , PackageID (..)

    -- * ReservedElasticsearchInstancePaymentOption
    , ReservedElasticsearchInstancePaymentOption (..)

    -- * IdentityPoolId
    , IdentityPoolId (..)

    -- * S3Key
    , S3Key (..)

    -- * PackageType
    , PackageType (..)

    -- * ServiceSoftwareOptions
    , ServiceSoftwareOptions (..)
    , mkServiceSoftwareOptions
    , ssoAutomatedUpdateDate
    , ssoCancellable
    , ssoCurrentVersion
    , ssoDescription
    , ssoNewVersion
    , ssoOptionalDeployment
    , ssoUpdateAvailable
    , ssoUpdateStatus

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * PackageVersionHistory
    , PackageVersionHistory (..)
    , mkPackageVersionHistory
    , pvhCommitMessage
    , pvhCreatedAt
    , pvhPackageVersion

    -- * LimitName
    , LimitName (..)

    -- * AdvancedOptionsStatus
    , AdvancedOptionsStatus (..)
    , mkAdvancedOptionsStatus
    , aosOptions
    , aosStatus

    -- * UpgradeHistory
    , UpgradeHistory (..)
    , mkUpgradeHistory
    , uhStartTimestamp
    , uhStepsList
    , uhUpgradeName
    , uhUpgradeStatus

    -- * InstanceRole
    , InstanceRole (..)

    -- * ARN
    , ARN (..)

    -- * ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- * PackageName
    , PackageName (..)

    -- * PackageVersion
    , PackageVersion (..)

    -- * PackageDetails
    , PackageDetails (..)
    , mkPackageDetails
    , pdAvailablePackageVersion
    , pdCreatedAt
    , pdErrorDetails
    , pdLastUpdatedAt
    , pdPackageDescription
    , pdPackageID
    , pdPackageName
    , pdPackageStatus
    , pdPackageType

    -- * EBSOptionsStatus
    , EBSOptionsStatus (..)
    , mkEBSOptionsStatus
    , ebsosOptions
    , ebsosStatus

    -- * UserPoolId
    , UserPoolId (..)

    -- * InboundCrossClusterSearchConnectionStatusCode
    , InboundCrossClusterSearchConnectionStatusCode (..)

    -- * ConnectionAlias
    , ConnectionAlias (..)

    -- * BackendRole
    , BackendRole (..)

    -- * AdvancedSecurityOptions
    , AdvancedSecurityOptions (..)
    , mkAdvancedSecurityOptions
    , asoEnabled
    , asoInternalUserDatabaseEnabled
    , asoSAMLOptions

    -- * NodeToNodeEncryptionOptionsStatus
    , NodeToNodeEncryptionOptionsStatus (..)
    , mkNodeToNodeEncryptionOptionsStatus
    , ntneosOptions
    , ntneosStatus

    -- * DomainPackageDetails
    , DomainPackageDetails (..)
    , mkDomainPackageDetails
    , dpdDomainName
    , dpdDomainPackageStatus
    , dpdErrorDetails
    , dpdLastUpdated
    , dpdPackageID
    , dpdPackageName
    , dpdPackageType
    , dpdPackageVersion
    , dpdReferencePath

    -- * CrossClusterSearchConnectionStatusMessage
    , CrossClusterSearchConnectionStatusMessage (..)

    -- * ReservationToken
    , ReservationToken (..)

    -- * ElasticsearchClusterConfig
    , ElasticsearchClusterConfig (..)
    , mkElasticsearchClusterConfig
    , eccDedicatedMasterCount
    , eccDedicatedMasterEnabled
    , eccDedicatedMasterType
    , eccInstanceCount
    , eccInstanceType
    , eccWarmCount
    , eccWarmEnabled
    , eccWarmType
    , eccZoneAwarenessConfig
    , eccZoneAwarenessEnabled

    -- * CompatibleVersionsMap
    , CompatibleVersionsMap (..)
    , mkCompatibleVersionsMap
    , cvmSourceVersion
    , cvmTargetVersions

    -- * DomainPackageStatus
    , DomainPackageStatus (..)

    -- * ServiceUrl
    , ServiceUrl (..)

    -- * ReservedElasticsearchInstanceOffering
    , ReservedElasticsearchInstanceOffering (..)
    , mkReservedElasticsearchInstanceOffering
    , reioCurrencyCode
    , reioDuration
    , reioElasticsearchInstanceType
    , reioFixedPrice
    , reioPaymentOption
    , reioRecurringCharges
    , reioReservedElasticsearchInstanceOfferingId
    , reioUsagePrice

    -- * Limits
    , Limits (..)
    , mkLimits
    , lAdditionalLimits
    , lInstanceLimits
    , lStorageTypes

    -- * LogPublishingOption
    , LogPublishingOption (..)
    , mkLogPublishingOption
    , lpoCloudWatchLogsLogGroupArn
    , lpoEnabled

    -- * EncryptionAtRestOptionsStatus
    , EncryptionAtRestOptionsStatus (..)
    , mkEncryptionAtRestOptionsStatus
    , earosOptions
    , earosStatus

    -- * Username
    , Username (..)

    -- * ReservedElasticsearchInstance
    , ReservedElasticsearchInstance (..)
    , mkReservedElasticsearchInstance
    , reiCurrencyCode
    , reiDuration
    , reiElasticsearchInstanceCount
    , reiElasticsearchInstanceType
    , reiFixedPrice
    , reiPaymentOption
    , reiRecurringCharges
    , reiReservationName
    , reiReservedElasticsearchInstanceId
    , reiReservedElasticsearchInstanceOfferingId
    , reiStartTime
    , reiState
    , reiUsagePrice

    -- * AdvancedSecurityOptionsInput
    , AdvancedSecurityOptionsInput (..)
    , mkAdvancedSecurityOptionsInput
    , asoiEnabled
    , asoiInternalUserDatabaseEnabled
    , asoiMasterUserOptions
    , asoiSAMLOptions

    -- * PackageStatus
    , PackageStatus (..)

    -- * VPCDerivedInfo
    , VPCDerivedInfo (..)
    , mkVPCDerivedInfo
    , vpcdiAvailabilityZones
    , vpcdiSecurityGroupIds
    , vpcdiSubnetIds
    , vpcdiVPCId

    -- * CloudWatchLogsLogGroupArn
    , CloudWatchLogsLogGroupArn (..)

    -- * CrossClusterSearchConnectionId
    , CrossClusterSearchConnectionId (..)

    -- * LogPublishingOptionsStatus
    , LogPublishingOptionsStatus (..)
    , mkLogPublishingOptionsStatus
    , lposOptions
    , lposStatus

    -- * OutboundCrossClusterSearchConnectionStatusCode
    , OutboundCrossClusterSearchConnectionStatusCode (..)

    -- * LogType
    , LogType (..)

    -- * DomainInformation
    , DomainInformation (..)
    , mkDomainInformation
    , dDomainName
    , dOwnerId
    , dRegion

    -- * PackageDescription
    , PackageDescription (..)

    -- * OwnerId
    , OwnerId (..)

    -- * NextToken
    , NextToken (..)

    -- * StorageTypeLimit
    , StorageTypeLimit (..)
    , mkStorageTypeLimit
    , stlLimitName
    , stlLimitValues

    -- * InstanceCountLimits
    , InstanceCountLimits (..)
    , mkInstanceCountLimits
    , iclMaximumInstanceCount
    , iclMinimumInstanceCount

    -- * ESWarmPartitionInstanceType
    , ESWarmPartitionInstanceType (..)

    -- * SnapshotOptions
    , SnapshotOptions (..)
    , mkSnapshotOptions
    , soAutomatedSnapshotStartHour

    -- * ElasticsearchVersionString
    , ElasticsearchVersionString (..)

    -- * OutboundCrossClusterSearchConnectionStatus
    , OutboundCrossClusterSearchConnectionStatus (..)
    , mkOutboundCrossClusterSearchConnectionStatus
    , occscsMessage
    , occscsStatusCode

    -- * NonEmptyString
    , NonEmptyString (..)

    -- * UpgradeName
    , UpgradeName (..)

    -- * KmsKeyId
    , KmsKeyId (..)

    -- * DomainName
    , DomainName (..)

    -- * OptionStatus
    , OptionStatus (..)
    , mkOptionStatus
    , osCreationDate
    , osUpdateDate
    , osState
    , osPendingDeletion
    , osUpdateVersion

    -- * ElasticsearchDomainStatus
    , ElasticsearchDomainStatus (..)
    , mkElasticsearchDomainStatus
    , edsDomainId
    , edsDomainName
    , edsARN
    , edsElasticsearchClusterConfig
    , edsAccessPolicies
    , edsAdvancedOptions
    , edsAdvancedSecurityOptions
    , edsCognitoOptions
    , edsCreated
    , edsDeleted
    , edsDomainEndpointOptions
    , edsEBSOptions
    , edsElasticsearchVersion
    , edsEncryptionAtRestOptions
    , edsEndpoint
    , edsEndpoints
    , edsLogPublishingOptions
    , edsNodeToNodeEncryptionOptions
    , edsProcessing
    , edsServiceSoftwareOptions
    , edsSnapshotOptions
    , edsUpgradeProcessing
    , edsVPCOptions

    -- * CognitoOptions
    , CognitoOptions (..)
    , mkCognitoOptions
    , coEnabled
    , coIdentityPoolId
    , coRoleArn
    , coUserPoolId

    -- * Password
    , Password (..)

    -- * DomainEndpointOptionsStatus
    , DomainEndpointOptionsStatus (..)
    , mkDomainEndpointOptionsStatus
    , deosOptions
    , deosStatus

    -- * ZoneAwarenessConfig
    , ZoneAwarenessConfig (..)
    , mkZoneAwarenessConfig
    , zacAvailabilityZoneCount

    -- * CommitMessage
    , CommitMessage (..)

    -- * StorageSubTypeName
    , StorageSubTypeName (..)

    -- * EncryptionAtRestOptions
    , EncryptionAtRestOptions (..)
    , mkEncryptionAtRestOptions
    , earoEnabled
    , earoKmsKeyId

    -- * DomainNameFqdn
    , DomainNameFqdn (..)

    -- * UpgradeStep
    , UpgradeStep (..)

    -- * VPCDerivedInfoStatus
    , VPCDerivedInfoStatus (..)
    , mkVPCDerivedInfoStatus
    , vpcdisOptions
    , vpcdisStatus

    -- * ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , edErrorMessage
    , edErrorType

    -- * MasterUserOptions
    , MasterUserOptions (..)
    , mkMasterUserOptions
    , muoMasterUserARN
    , muoMasterUserName
    , muoMasterUserPassword

    -- * ElasticsearchVersionStatus
    , ElasticsearchVersionStatus (..)
    , mkElasticsearchVersionStatus
    , evsOptions
    , evsStatus

    -- * SAMLEntityId
    , SAMLEntityId (..)

    -- * TLSSecurityPolicy
    , TLSSecurityPolicy (..)

    -- * SAMLOptionsInput
    , SAMLOptionsInput (..)
    , mkSAMLOptionsInput
    , samloiEnabled
    , samloiIdp
    , samloiMasterBackendRole
    , samloiMasterUserName
    , samloiRolesKey
    , samloiSessionTimeoutMinutes
    , samloiSubjectKey

    -- * VPCOptions
    , VPCOptions (..)
    , mkVPCOptions
    , vpcoSecurityGroupIds
    , vpcoSubnetIds

    -- * DomainId
    , DomainId (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * InboundCrossClusterSearchConnectionStatus
    , InboundCrossClusterSearchConnectionStatus (..)
    , mkInboundCrossClusterSearchConnectionStatus
    , iccscsMessage
    , iccscsStatusCode

    -- * Region
    , Region (..)

    -- * OutboundCrossClusterSearchConnection
    , OutboundCrossClusterSearchConnection (..)
    , mkOutboundCrossClusterSearchConnection
    , occscConnectionAlias
    , occscConnectionStatus
    , occscCrossClusterSearchConnectionId
    , occscDestinationDomainInfo
    , occscSourceDomainInfo

    -- * SnapshotOptionsStatus
    , SnapshotOptionsStatus (..)
    , mkSnapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- * Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- * VolumeType
    , VolumeType (..)

    -- * ErrorType
    , ErrorType (..)

    -- * DescribePackagesFilterName
    , DescribePackagesFilterName (..)

    -- * RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * CognitoOptionsStatus
    , CognitoOptionsStatus (..)
    , mkCognitoOptionsStatus
    , cosOptions
    , cosStatus

    -- * DomainEndpointOptions
    , DomainEndpointOptions (..)
    , mkDomainEndpointOptions
    , deoCustomEndpoint
    , deoCustomEndpointCertificateArn
    , deoCustomEndpointEnabled
    , deoEnforceHTTPS
    , deoTLSSecurityPolicy

    -- * DomainInfo
    , DomainInfo (..)
    , mkDomainInfo
    , diDomainName

    -- * ReferencePath
    , ReferencePath (..)

    -- * SAMLMetadata
    , SAMLMetadata (..)

    -- * StorageTypeName
    , StorageTypeName (..)

    -- * LimitValue
    , LimitValue (..)

    -- * SAMLIdp
    , SAMLIdp (..)
    , mkSAMLIdp
    , samliMetadataContent
    , samliEntityId

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus (..)
    , mkAccessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * S3BucketName
    , S3BucketName (..)

    -- * ElasticsearchDomainConfig
    , ElasticsearchDomainConfig (..)
    , mkElasticsearchDomainConfig
    , edcAccessPolicies
    , edcAdvancedOptions
    , edcAdvancedSecurityOptions
    , edcCognitoOptions
    , edcDomainEndpointOptions
    , edcEBSOptions
    , edcElasticsearchClusterConfig
    , edcElasticsearchVersion
    , edcEncryptionAtRestOptions
    , edcLogPublishingOptions
    , edcNodeToNodeEncryptionOptions
    , edcSnapshotOptions
    , edcVPCOptions

    -- * Issue
    , Issue (..)

    -- * InboundCrossClusterSearchConnection
    , InboundCrossClusterSearchConnection (..)
    , mkInboundCrossClusterSearchConnection
    , iccscConnectionStatus
    , iccscCrossClusterSearchConnectionId
    , iccscDestinationDomainInfo
    , iccscSourceDomainInfo

    -- * StorageType
    , StorageType (..)
    , mkStorageType
    , stStorageSubTypeName
    , stStorageTypeLimits
    , stStorageTypeName

    -- * RoleArn
    , RoleArn (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ElasticsearchVersion
    , ElasticsearchVersion (..)

    -- * SourceVersion
    , SourceVersion (..)

    -- * ReservedElasticsearchInstanceId
    , ReservedElasticsearchInstanceId (..)

    -- * ReservedElasticsearchInstanceOfferingId
    , ReservedElasticsearchInstanceOfferingId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
  
import Network.AWS.ElasticSearch.Types.EBSOptions
  
import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
  
import Network.AWS.ElasticSearch.Types.UpgradeStatus
  
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
  
import Network.AWS.ElasticSearch.Types.AdditionalLimit
  
  
  
import Network.AWS.ElasticSearch.Types.OptionState
  
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
  
import Network.AWS.ElasticSearch.Types.PackageSource
  
import Network.AWS.ElasticSearch.Types.PolicyDocument
  
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
  
import Network.AWS.ElasticSearch.Types.InstanceLimits
  
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
  
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterValue
  
import Network.AWS.ElasticSearch.Types.PackageID
  
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
  
import Network.AWS.ElasticSearch.Types.IdentityPoolId
  
import Network.AWS.ElasticSearch.Types.S3Key
  
import Network.AWS.ElasticSearch.Types.PackageType
  
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
  
import Network.AWS.ElasticSearch.Types.Tag
  
import Network.AWS.ElasticSearch.Types.PackageVersionHistory
  
import Network.AWS.ElasticSearch.Types.LimitName
  
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
  
import Network.AWS.ElasticSearch.Types.UpgradeHistory
  
import Network.AWS.ElasticSearch.Types.InstanceRole
  
import Network.AWS.ElasticSearch.Types.ARN
  
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
  
import Network.AWS.ElasticSearch.Types.PackageName
  
import Network.AWS.ElasticSearch.Types.PackageVersion
  
import Network.AWS.ElasticSearch.Types.PackageDetails
  
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
  
import Network.AWS.ElasticSearch.Types.UserPoolId
  
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
  
import Network.AWS.ElasticSearch.Types.ConnectionAlias
  
import Network.AWS.ElasticSearch.Types.BackendRole
  
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
  
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
  
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
  
import Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionStatusMessage
  
  
import Network.AWS.ElasticSearch.Types.ReservationToken
  
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
  
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
  
import Network.AWS.ElasticSearch.Types.DomainPackageStatus
  
import Network.AWS.ElasticSearch.Types.ServiceUrl
  
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
  
import Network.AWS.ElasticSearch.Types.Limits
  
import Network.AWS.ElasticSearch.Types.LogPublishingOption
  
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
  
import Network.AWS.ElasticSearch.Types.Username
  
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
  
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
  
import Network.AWS.ElasticSearch.Types.PackageStatus
  
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
  
import Network.AWS.ElasticSearch.Types.CloudWatchLogsLogGroupArn
  
  
import Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionId
  
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
  
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
  
import Network.AWS.ElasticSearch.Types.LogType
  
import Network.AWS.ElasticSearch.Types.DomainInformation
  
import Network.AWS.ElasticSearch.Types.PackageDescription
  
import Network.AWS.ElasticSearch.Types.OwnerId
  
import Network.AWS.ElasticSearch.Types.NextToken
  
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
  
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
  
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
  
import Network.AWS.ElasticSearch.Types.SnapshotOptions
  
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionString
  
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
  
import Network.AWS.ElasticSearch.Types.NonEmptyString
  
import Network.AWS.ElasticSearch.Types.UpgradeName
  
import Network.AWS.ElasticSearch.Types.KmsKeyId
  
import Network.AWS.ElasticSearch.Types.DomainName
  
import Network.AWS.ElasticSearch.Types.OptionStatus
  
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
  
import Network.AWS.ElasticSearch.Types.CognitoOptions
  
import Network.AWS.ElasticSearch.Types.Password
  
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
  
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
  
import Network.AWS.ElasticSearch.Types.CommitMessage
  
import Network.AWS.ElasticSearch.Types.StorageSubTypeName
  
  
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
  
import Network.AWS.ElasticSearch.Types.DomainNameFqdn
  
import Network.AWS.ElasticSearch.Types.UpgradeStep
  
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
  
import Network.AWS.ElasticSearch.Types.ErrorDetails
  
  
import Network.AWS.ElasticSearch.Types.MasterUserOptions
  
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
  
import Network.AWS.ElasticSearch.Types.SAMLEntityId
  
import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
  
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
  
import Network.AWS.ElasticSearch.Types.VPCOptions
  
import Network.AWS.ElasticSearch.Types.DomainId
  
import Network.AWS.ElasticSearch.Types.DeploymentStatus
  
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
  
import Network.AWS.ElasticSearch.Types.Region
  
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
  
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
  
import Network.AWS.ElasticSearch.Types.Filter
  
  
import Network.AWS.ElasticSearch.Types.VolumeType
  
import Network.AWS.ElasticSearch.Types.ErrorType
  
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
  
import Network.AWS.ElasticSearch.Types.RecurringCharge
  
import Network.AWS.ElasticSearch.Types.ErrorMessage
  
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
  
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
  
import Network.AWS.ElasticSearch.Types.DomainInfo
  
import Network.AWS.ElasticSearch.Types.ReferencePath
  
import Network.AWS.ElasticSearch.Types.SAMLMetadata
  
import Network.AWS.ElasticSearch.Types.StorageTypeName
  
import Network.AWS.ElasticSearch.Types.LimitValue
  
  
import Network.AWS.ElasticSearch.Types.SAMLIdp
  
import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
  
  
import Network.AWS.ElasticSearch.Types.S3BucketName
  
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
  
import Network.AWS.ElasticSearch.Types.Issue
  
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
  
import Network.AWS.ElasticSearch.Types.StorageType
  
  
  
import Network.AWS.ElasticSearch.Types.RoleArn
  
import Network.AWS.ElasticSearch.Types.Key
  
import Network.AWS.ElasticSearch.Types.Value
  
import Network.AWS.ElasticSearch.Types.ElasticsearchVersion
  
import Network.AWS.ElasticSearch.Types.SourceVersion
  
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceId
  
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOfferingId
  

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ElasticSearch",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "es",
                 Core._svcVersion = "2015-01-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ElasticSearch",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | An exception for missing / invalid input fields. Gives http status code of 400.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred because user does not have permissions to access the resource. Returns HTTP status code 403.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | An exception for creating a resource that already exists. Gives http status code of 400.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceAlreadyExistsException #-}
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred because the client attempts to remove a resource that is currently in use. Returns HTTP status code 409.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BaseException
  = Core._MatchServiceError mkServiceConfig "BaseException"
{-# INLINEABLE _BaseException #-}
{-# DEPRECATED _BaseException "Use generic-lens or generic-optics instead"  #-}

-- | An error occured because the client wanted to access a not supported operation. Gives http status code of 409.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException
  = Core._MatchServiceError mkServiceConfig
      "DisabledOperationException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DisabledOperationException #-}
{-# DEPRECATED _DisabledOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException
  = Core._MatchServiceError mkServiceConfig "InternalException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalException #-}
{-# DEPRECATED _InternalException "Use generic-lens or generic-optics instead"  #-}

-- | An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException
  = Core._MatchServiceError mkServiceConfig "InvalidTypeException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _InvalidTypeException #-}
{-# DEPRECATED _InvalidTypeException "Use generic-lens or generic-optics instead"  #-}

-- | An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The request processing has failed because of invalid pagination token provided by customer. Returns an HTTP status code of 400. 
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPaginationTokenException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidPaginationTokenException #-}
{-# DEPRECATED _InvalidPaginationTokenException "Use generic-lens or generic-optics instead"  #-}

-- | An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
