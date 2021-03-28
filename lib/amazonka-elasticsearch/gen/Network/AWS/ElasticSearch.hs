{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elasticsearch Configuration Service__ 
--
-- Use the Amazon Elasticsearch Configuration API to create, configure, and manage Elasticsearch domains.
-- For sample code that uses the Configuration API, see the <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-configuration-samples.html Amazon Elasticsearch Service Developer Guide> . The guide also contains <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-request-signing.html sample code for sending signed HTTP requests to the Elasticsearch APIs> .
-- The endpoint for configuration service requests is region-specific: es./region/ .amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions Regions and Endpoints> .
module Network.AWS.ElasticSearch
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ConflictException
    , _ConflictException

    -- ** BaseException
    , _BaseException

    -- ** DisabledOperationException
    , _DisabledOperationException

    -- ** InternalException
    , _InternalException

    -- ** InvalidTypeException
    , _InvalidTypeException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateOutboundCrossClusterSearchConnection 
    , module Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection

    -- ** DescribeInboundCrossClusterSearchConnections 
    , module Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections

    -- ** CreateElasticsearchDomain 
    , module Network.AWS.ElasticSearch.CreateElasticsearchDomain

    -- ** RemoveTags 
    , module Network.AWS.ElasticSearch.RemoveTags

    -- ** GetCompatibleElasticsearchVersions 
    , module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions

    -- ** DescribeElasticsearchDomains 
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomains

    -- ** ListDomainsForPackage 
    , module Network.AWS.ElasticSearch.ListDomainsForPackage

    -- ** ListPackagesForDomain 
    , module Network.AWS.ElasticSearch.ListPackagesForDomain

    -- ** StartElasticsearchServiceSoftwareUpdate 
    , module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate

    -- ** ListElasticsearchInstanceTypes (Paginated)
    , module Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes

    -- ** DeleteElasticsearchServiceRole 
    , module Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole

    -- ** DescribeElasticsearchDomain 
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomain

    -- ** ListDomainNames 
    , module Network.AWS.ElasticSearch.ListDomainNames

    -- ** AssociatePackage 
    , module Network.AWS.ElasticSearch.AssociatePackage

    -- ** DeleteOutboundCrossClusterSearchConnection 
    , module Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection

    -- ** DescribeElasticsearchInstanceTypeLimits 
    , module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits

    -- ** GetPackageVersionHistory 
    , module Network.AWS.ElasticSearch.GetPackageVersionHistory

    -- ** GetUpgradeHistory (Paginated)
    , module Network.AWS.ElasticSearch.GetUpgradeHistory

    -- ** DescribePackages 
    , module Network.AWS.ElasticSearch.DescribePackages

    -- ** DescribeElasticsearchDomainConfig 
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig

    -- ** GetUpgradeStatus 
    , module Network.AWS.ElasticSearch.GetUpgradeStatus

    -- ** DeleteElasticsearchDomain 
    , module Network.AWS.ElasticSearch.DeleteElasticsearchDomain

    -- ** DissociatePackage 
    , module Network.AWS.ElasticSearch.DissociatePackage

    -- ** PurchaseReservedElasticsearchInstanceOffering 
    , module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering

    -- ** DescribeReservedElasticsearchInstances (Paginated)
    , module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances

    -- ** UpdateElasticsearchDomainConfig 
    , module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig

    -- ** ListElasticsearchVersions (Paginated)
    , module Network.AWS.ElasticSearch.ListElasticsearchVersions

    -- ** AddTags 
    , module Network.AWS.ElasticSearch.AddTags

    -- ** DeleteInboundCrossClusterSearchConnection 
    , module Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection

    -- ** DescribeReservedElasticsearchInstanceOfferings (Paginated)
    , module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings

    -- ** UpgradeElasticsearchDomain 
    , module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain

    -- ** ListTags 
    , module Network.AWS.ElasticSearch.ListTags

    -- ** DeletePackage 
    , module Network.AWS.ElasticSearch.DeletePackage

    -- ** UpdatePackage 
    , module Network.AWS.ElasticSearch.UpdatePackage

    -- ** CancelElasticsearchServiceSoftwareUpdate 
    , module Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate

    -- ** CreatePackage 
    , module Network.AWS.ElasticSearch.CreatePackage

    -- ** RejectInboundCrossClusterSearchConnection 
    , module Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection

    -- ** DescribeOutboundCrossClusterSearchConnections 
    , module Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections

    -- ** AcceptInboundCrossClusterSearchConnection 
    , module Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection

    -- * Types

    -- ** AdvancedSecurityOptionsStatus
    , AdvancedSecurityOptionsStatus (..)
    , mkAdvancedSecurityOptionsStatus
    , asosOptions
    , asosStatus

    -- ** EBSOptions
    , EBSOptions (..)
    , mkEBSOptions
    , ebsoEBSEnabled
    , ebsoIops
    , ebsoVolumeSize
    , ebsoVolumeType

    -- ** SAMLOptionsOutput
    , SAMLOptionsOutput (..)
    , mkSAMLOptionsOutput
    , samlooEnabled
    , samlooIdp
    , samlooRolesKey
    , samlooSessionTimeoutMinutes
    , samlooSubjectKey

    -- ** UpgradeStatus
    , UpgradeStatus (..)

    -- ** NodeToNodeEncryptionOptions
    , NodeToNodeEncryptionOptions (..)
    , mkNodeToNodeEncryptionOptions
    , ntneoEnabled

    -- ** AdditionalLimit
    , AdditionalLimit (..)
    , mkAdditionalLimit
    , alLimitName
    , alLimitValues

    -- ** OptionState
    , OptionState (..)

    -- ** DescribePackagesFilter
    , DescribePackagesFilter (..)
    , mkDescribePackagesFilter
    , dpfName
    , dpfValue

    -- ** PackageSource
    , PackageSource (..)
    , mkPackageSource
    , psS3BucketName
    , psS3Key

    -- ** PolicyDocument
    , PolicyDocument (..)

    -- ** UpgradeStepItem
    , UpgradeStepItem (..)
    , mkUpgradeStepItem
    , usiIssues
    , usiProgressPercent
    , usiUpgradeStep
    , usiUpgradeStepStatus

    -- ** InstanceLimits
    , InstanceLimits (..)
    , mkInstanceLimits
    , ilInstanceCountLimits

    -- ** ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus (..)
    , mkElasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- ** DescribePackagesFilterValue
    , DescribePackagesFilterValue (..)

    -- ** PackageID
    , PackageID (..)

    -- ** ReservedElasticsearchInstancePaymentOption
    , ReservedElasticsearchInstancePaymentOption (..)

    -- ** IdentityPoolId
    , IdentityPoolId (..)

    -- ** S3Key
    , S3Key (..)

    -- ** PackageType
    , PackageType (..)

    -- ** ServiceSoftwareOptions
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

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** PackageVersionHistory
    , PackageVersionHistory (..)
    , mkPackageVersionHistory
    , pvhCommitMessage
    , pvhCreatedAt
    , pvhPackageVersion

    -- ** LimitName
    , LimitName (..)

    -- ** AdvancedOptionsStatus
    , AdvancedOptionsStatus (..)
    , mkAdvancedOptionsStatus
    , aosOptions
    , aosStatus

    -- ** UpgradeHistory
    , UpgradeHistory (..)
    , mkUpgradeHistory
    , uhStartTimestamp
    , uhStepsList
    , uhUpgradeName
    , uhUpgradeStatus

    -- ** InstanceRole
    , InstanceRole (..)

    -- ** ARN
    , ARN (..)

    -- ** ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- ** PackageName
    , PackageName (..)

    -- ** PackageVersion
    , PackageVersion (..)

    -- ** PackageDetails
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

    -- ** EBSOptionsStatus
    , EBSOptionsStatus (..)
    , mkEBSOptionsStatus
    , ebsosOptions
    , ebsosStatus

    -- ** UserPoolId
    , UserPoolId (..)

    -- ** InboundCrossClusterSearchConnectionStatusCode
    , InboundCrossClusterSearchConnectionStatusCode (..)

    -- ** ConnectionAlias
    , ConnectionAlias (..)

    -- ** BackendRole
    , BackendRole (..)

    -- ** AdvancedSecurityOptions
    , AdvancedSecurityOptions (..)
    , mkAdvancedSecurityOptions
    , asoEnabled
    , asoInternalUserDatabaseEnabled
    , asoSAMLOptions

    -- ** NodeToNodeEncryptionOptionsStatus
    , NodeToNodeEncryptionOptionsStatus (..)
    , mkNodeToNodeEncryptionOptionsStatus
    , ntneosOptions
    , ntneosStatus

    -- ** DomainPackageDetails
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

    -- ** CrossClusterSearchConnectionStatusMessage
    , CrossClusterSearchConnectionStatusMessage (..)

    -- ** ReservationToken
    , ReservationToken (..)

    -- ** ElasticsearchClusterConfig
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

    -- ** CompatibleVersionsMap
    , CompatibleVersionsMap (..)
    , mkCompatibleVersionsMap
    , cvmSourceVersion
    , cvmTargetVersions

    -- ** DomainPackageStatus
    , DomainPackageStatus (..)

    -- ** ServiceUrl
    , ServiceUrl (..)

    -- ** ReservedElasticsearchInstanceOffering
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

    -- ** Limits
    , Limits (..)
    , mkLimits
    , lAdditionalLimits
    , lInstanceLimits
    , lStorageTypes

    -- ** LogPublishingOption
    , LogPublishingOption (..)
    , mkLogPublishingOption
    , lpoCloudWatchLogsLogGroupArn
    , lpoEnabled

    -- ** EncryptionAtRestOptionsStatus
    , EncryptionAtRestOptionsStatus (..)
    , mkEncryptionAtRestOptionsStatus
    , earosOptions
    , earosStatus

    -- ** Username
    , Username (..)

    -- ** ReservedElasticsearchInstance
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

    -- ** AdvancedSecurityOptionsInput
    , AdvancedSecurityOptionsInput (..)
    , mkAdvancedSecurityOptionsInput
    , asoiEnabled
    , asoiInternalUserDatabaseEnabled
    , asoiMasterUserOptions
    , asoiSAMLOptions

    -- ** PackageStatus
    , PackageStatus (..)

    -- ** VPCDerivedInfo
    , VPCDerivedInfo (..)
    , mkVPCDerivedInfo
    , vpcdiAvailabilityZones
    , vpcdiSecurityGroupIds
    , vpcdiSubnetIds
    , vpcdiVPCId

    -- ** CloudWatchLogsLogGroupArn
    , CloudWatchLogsLogGroupArn (..)

    -- ** CrossClusterSearchConnectionId
    , CrossClusterSearchConnectionId (..)

    -- ** LogPublishingOptionsStatus
    , LogPublishingOptionsStatus (..)
    , mkLogPublishingOptionsStatus
    , lposOptions
    , lposStatus

    -- ** OutboundCrossClusterSearchConnectionStatusCode
    , OutboundCrossClusterSearchConnectionStatusCode (..)

    -- ** LogType
    , LogType (..)

    -- ** DomainInformation
    , DomainInformation (..)
    , mkDomainInformation
    , dDomainName
    , dOwnerId
    , dRegion

    -- ** PackageDescription
    , PackageDescription (..)

    -- ** OwnerId
    , OwnerId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** StorageTypeLimit
    , StorageTypeLimit (..)
    , mkStorageTypeLimit
    , stlLimitName
    , stlLimitValues

    -- ** InstanceCountLimits
    , InstanceCountLimits (..)
    , mkInstanceCountLimits
    , iclMaximumInstanceCount
    , iclMinimumInstanceCount

    -- ** ESWarmPartitionInstanceType
    , ESWarmPartitionInstanceType (..)

    -- ** SnapshotOptions
    , SnapshotOptions (..)
    , mkSnapshotOptions
    , soAutomatedSnapshotStartHour

    -- ** ElasticsearchVersionString
    , ElasticsearchVersionString (..)

    -- ** OutboundCrossClusterSearchConnectionStatus
    , OutboundCrossClusterSearchConnectionStatus (..)
    , mkOutboundCrossClusterSearchConnectionStatus
    , occscsMessage
    , occscsStatusCode

    -- ** NonEmptyString
    , NonEmptyString (..)

    -- ** UpgradeName
    , UpgradeName (..)

    -- ** KmsKeyId
    , KmsKeyId (..)

    -- ** DomainName
    , DomainName (..)

    -- ** OptionStatus
    , OptionStatus (..)
    , mkOptionStatus
    , osCreationDate
    , osUpdateDate
    , osState
    , osPendingDeletion
    , osUpdateVersion

    -- ** ElasticsearchDomainStatus
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

    -- ** CognitoOptions
    , CognitoOptions (..)
    , mkCognitoOptions
    , coEnabled
    , coIdentityPoolId
    , coRoleArn
    , coUserPoolId

    -- ** Password
    , Password (..)

    -- ** DomainEndpointOptionsStatus
    , DomainEndpointOptionsStatus (..)
    , mkDomainEndpointOptionsStatus
    , deosOptions
    , deosStatus

    -- ** ZoneAwarenessConfig
    , ZoneAwarenessConfig (..)
    , mkZoneAwarenessConfig
    , zacAvailabilityZoneCount

    -- ** CommitMessage
    , CommitMessage (..)

    -- ** StorageSubTypeName
    , StorageSubTypeName (..)

    -- ** EncryptionAtRestOptions
    , EncryptionAtRestOptions (..)
    , mkEncryptionAtRestOptions
    , earoEnabled
    , earoKmsKeyId

    -- ** DomainNameFqdn
    , DomainNameFqdn (..)

    -- ** UpgradeStep
    , UpgradeStep (..)

    -- ** VPCDerivedInfoStatus
    , VPCDerivedInfoStatus (..)
    , mkVPCDerivedInfoStatus
    , vpcdisOptions
    , vpcdisStatus

    -- ** ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , edErrorMessage
    , edErrorType

    -- ** MasterUserOptions
    , MasterUserOptions (..)
    , mkMasterUserOptions
    , muoMasterUserARN
    , muoMasterUserName
    , muoMasterUserPassword

    -- ** ElasticsearchVersionStatus
    , ElasticsearchVersionStatus (..)
    , mkElasticsearchVersionStatus
    , evsOptions
    , evsStatus

    -- ** SAMLEntityId
    , SAMLEntityId (..)

    -- ** TLSSecurityPolicy
    , TLSSecurityPolicy (..)

    -- ** SAMLOptionsInput
    , SAMLOptionsInput (..)
    , mkSAMLOptionsInput
    , samloiEnabled
    , samloiIdp
    , samloiMasterBackendRole
    , samloiMasterUserName
    , samloiRolesKey
    , samloiSessionTimeoutMinutes
    , samloiSubjectKey

    -- ** VPCOptions
    , VPCOptions (..)
    , mkVPCOptions
    , vpcoSecurityGroupIds
    , vpcoSubnetIds

    -- ** DomainId
    , DomainId (..)

    -- ** DeploymentStatus
    , DeploymentStatus (..)

    -- ** InboundCrossClusterSearchConnectionStatus
    , InboundCrossClusterSearchConnectionStatus (..)
    , mkInboundCrossClusterSearchConnectionStatus
    , iccscsMessage
    , iccscsStatusCode

    -- ** Region
    , Region (..)

    -- ** OutboundCrossClusterSearchConnection
    , OutboundCrossClusterSearchConnection (..)
    , mkOutboundCrossClusterSearchConnection
    , occscConnectionAlias
    , occscConnectionStatus
    , occscCrossClusterSearchConnectionId
    , occscDestinationDomainInfo
    , occscSourceDomainInfo

    -- ** SnapshotOptionsStatus
    , SnapshotOptionsStatus (..)
    , mkSnapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- ** Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- ** VolumeType
    , VolumeType (..)

    -- ** ErrorType
    , ErrorType (..)

    -- ** DescribePackagesFilterName
    , DescribePackagesFilterName (..)

    -- ** RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** CognitoOptionsStatus
    , CognitoOptionsStatus (..)
    , mkCognitoOptionsStatus
    , cosOptions
    , cosStatus

    -- ** DomainEndpointOptions
    , DomainEndpointOptions (..)
    , mkDomainEndpointOptions
    , deoCustomEndpoint
    , deoCustomEndpointCertificateArn
    , deoCustomEndpointEnabled
    , deoEnforceHTTPS
    , deoTLSSecurityPolicy

    -- ** DomainInfo
    , DomainInfo (..)
    , mkDomainInfo
    , diDomainName

    -- ** ReferencePath
    , ReferencePath (..)

    -- ** SAMLMetadata
    , SAMLMetadata (..)

    -- ** StorageTypeName
    , StorageTypeName (..)

    -- ** LimitValue
    , LimitValue (..)

    -- ** SAMLIdp
    , SAMLIdp (..)
    , mkSAMLIdp
    , samliMetadataContent
    , samliEntityId

    -- ** AccessPoliciesStatus
    , AccessPoliciesStatus (..)
    , mkAccessPoliciesStatus
    , apsOptions
    , apsStatus

    -- ** S3BucketName
    , S3BucketName (..)

    -- ** ElasticsearchDomainConfig
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

    -- ** Issue
    , Issue (..)

    -- ** InboundCrossClusterSearchConnection
    , InboundCrossClusterSearchConnection (..)
    , mkInboundCrossClusterSearchConnection
    , iccscConnectionStatus
    , iccscCrossClusterSearchConnectionId
    , iccscDestinationDomainInfo
    , iccscSourceDomainInfo

    -- ** StorageType
    , StorageType (..)
    , mkStorageType
    , stStorageSubTypeName
    , stStorageTypeLimits
    , stStorageTypeName

    -- ** RoleArn
    , RoleArn (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** ElasticsearchVersion
    , ElasticsearchVersion (..)

    -- ** SourceVersion
    , SourceVersion (..)

    -- ** ReservedElasticsearchInstanceId
    , ReservedElasticsearchInstanceId (..)

    -- ** ReservedElasticsearchInstanceOfferingId
    , ReservedElasticsearchInstanceOfferingId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Waiters
import Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.CreateElasticsearchDomain
import Network.AWS.ElasticSearch.RemoveTags
import Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
import Network.AWS.ElasticSearch.DescribeElasticsearchDomains
import Network.AWS.ElasticSearch.ListDomainsForPackage
import Network.AWS.ElasticSearch.ListPackagesForDomain
import Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
import Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
import Network.AWS.ElasticSearch.DescribeElasticsearchDomain
import Network.AWS.ElasticSearch.ListDomainNames
import Network.AWS.ElasticSearch.AssociatePackage
import Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Network.AWS.ElasticSearch.GetPackageVersionHistory
import Network.AWS.ElasticSearch.GetUpgradeHistory
import Network.AWS.ElasticSearch.DescribePackages
import Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
import Network.AWS.ElasticSearch.GetUpgradeStatus
import Network.AWS.ElasticSearch.DeleteElasticsearchDomain
import Network.AWS.ElasticSearch.DissociatePackage
import Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
import Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
import Network.AWS.ElasticSearch.ListElasticsearchVersions
import Network.AWS.ElasticSearch.AddTags
import Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
import Network.AWS.ElasticSearch.ListTags
import Network.AWS.ElasticSearch.DeletePackage
import Network.AWS.ElasticSearch.UpdatePackage
import Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.CreatePackage
import Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElasticSearch'.
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
