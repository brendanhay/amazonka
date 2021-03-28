-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _AccessDeniedException
    , _AccountSetupInProgressException
    , _NotFoundException
    , _OperationFailureException
    , _ServiceException
    , _UnauthenticatedException
    , _InvalidInputException

    -- * AlarmState
    , AlarmState (..)

    -- * HeaderEnum
    , HeaderEnum (..)

    -- * DomainEntryType
    , DomainEntryType (..)

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction (..)
    , mkPendingMaintenanceAction
    , pmaAction
    , pmaCurrentApplyDate
    , pmaDescription

    -- * LoadBalancerProtocol
    , LoadBalancerProtocol (..)

    -- * Origin
    , Origin (..)
    , mkOrigin
    , oName
    , oProtocolPolicy
    , oRegionName
    , oResourceType

    -- * CloudFormationStackRecordSourceType
    , CloudFormationStackRecordSourceType (..)

    -- * ContainerServiceDeploymentState
    , ContainerServiceDeploymentState (..)

    -- * IpV6Address
    , IpV6Address (..)

    -- * AutoSnapshotStatus
    , AutoSnapshotStatus (..)

    -- * BehaviorEnum
    , BehaviorEnum (..)

    -- * Blueprint
    , Blueprint (..)
    , mkBlueprint
    , bBlueprintId
    , bDescription
    , bGroup
    , bIsActive
    , bLicenseUrl
    , bMinPower
    , bName
    , bPlatform
    , bProductUrl
    , bType
    , bVersion
    , bVersionCode

    -- * InstancePortState
    , InstancePortState (..)
    , mkInstancePortState
    , ipsCidrListAliases
    , ipsCidrs
    , ipsFromPort
    , ipsProtocol
    , ipsState
    , ipsToPort

    -- * InstanceNetworking
    , InstanceNetworking (..)
    , mkInstanceNetworking
    , inMonthlyTransfer
    , inPorts

    -- * IpAddress
    , IpAddress (..)

    -- * ResourceRecord
    , ResourceRecord (..)
    , mkResourceRecord
    , rrName
    , rrType
    , rrValue

    -- * RegionName
    , RegionName (..)

    -- * DiskSnapshotInfo
    , DiskSnapshotInfo (..)
    , mkDiskSnapshotInfo
    , dsiSizeInGb

    -- * InstanceSnapshotState
    , InstanceSnapshotState (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TreatMissingData
    , TreatMissingData (..)

    -- * ContainerService
    , ContainerService (..)
    , mkContainerService
    , csArn
    , csContainerServiceName
    , csCreatedAt
    , csCurrentDeployment
    , csIsDisabled
    , csLocation
    , csNextDeployment
    , csPower
    , csPowerId
    , csPrincipalArn
    , csPrivateDomainName
    , csPublicDomainNames
    , csResourceType
    , csScale
    , csState
    , csTags
    , csUrl

    -- * CertificateName
    , CertificateName (..)

    -- * ContainerServiceHealthCheckConfig
    , ContainerServiceHealthCheckConfig (..)
    , mkContainerServiceHealthCheckConfig
    , cshccHealthyThreshold
    , cshccIntervalSeconds
    , cshccPath
    , cshccSuccessCodes
    , cshccTimeoutSeconds
    , cshccUnhealthyThreshold

    -- * RecordState
    , RecordState (..)

    -- * RelationalDatabaseEvent
    , RelationalDatabaseEvent (..)
    , mkRelationalDatabaseEvent
    , rdeCreatedAt
    , rdeEventCategories
    , rdeMessage
    , rdeResource

    -- * AutoSnapshotAddOnRequest
    , AutoSnapshotAddOnRequest (..)
    , mkAutoSnapshotAddOnRequest
    , asaorSnapshotTimeOfDay

    -- * HeaderObject
    , HeaderObject (..)
    , mkHeaderObject
    , hoHeadersAllowList
    , hoOption

    -- * ResourceType
    , ResourceType (..)

    -- * DestinationInfo
    , DestinationInfo (..)
    , mkDestinationInfo
    , diId
    , diService

    -- * QueryStringObject
    , QueryStringObject (..)
    , mkQueryStringObject
    , qsoOption
    , qsoQueryStringsAllowList

    -- * RelationalDatabaseSnapshot
    , RelationalDatabaseSnapshot (..)
    , mkRelationalDatabaseSnapshot
    , rdsArn
    , rdsCreatedAt
    , rdsEngine
    , rdsEngineVersion
    , rdsFromRelationalDatabaseArn
    , rdsFromRelationalDatabaseBlueprintId
    , rdsFromRelationalDatabaseBundleId
    , rdsFromRelationalDatabaseName
    , rdsLocation
    , rdsName
    , rdsResourceType
    , rdsSizeInGb
    , rdsState
    , rdsSupportCode
    , rdsTags

    -- * ContainerServiceLogEvent
    , ContainerServiceLogEvent (..)
    , mkContainerServiceLogEvent
    , csleCreatedAt
    , csleMessage

    -- * ContainerServicePowerName
    , ContainerServicePowerName (..)

    -- * PortAccessType
    , PortAccessType (..)

    -- * Disk
    , Disk (..)
    , mkDisk
    , dAddOns
    , dArn
    , dAttachedTo
    , dAttachmentState
    , dCreatedAt
    , dGbInUse
    , dIops
    , dIsAttached
    , dIsSystemDisk
    , dLocation
    , dName
    , dPath
    , dResourceType
    , dSizeInGb
    , dState
    , dSupportCode
    , dTags

    -- * EligibleToRenew
    , EligibleToRenew (..)

    -- * ContainerServiceState
    , ContainerServiceState (..)

    -- * ResourceName
    , ResourceName (..)

    -- * RelationalDatabaseMetricName
    , RelationalDatabaseMetricName (..)

    -- * KeyPair
    , KeyPair (..)
    , mkKeyPair
    , kpArn
    , kpCreatedAt
    , kpFingerprint
    , kpLocation
    , kpName
    , kpResourceType
    , kpSupportCode
    , kpTags

    -- * RequestFailureReason
    , RequestFailureReason (..)

    -- * Operation
    , Operation (..)
    , mkOperation
    , ofCreatedAt
    , ofErrorCode
    , ofErrorDetails
    , ofId
    , ofIsTerminal
    , ofLocation
    , ofOperationDetails
    , ofOperationType
    , ofResourceName
    , ofResourceType
    , ofStatus
    , ofStatusChangedAt

    -- * ContainerName
    , ContainerName (..)

    -- * MetricDatapoint
    , MetricDatapoint (..)
    , mkMetricDatapoint
    , mdAverage
    , mdMaximum
    , mdMinimum
    , mdSampleCount
    , mdSum
    , mdTimestamp
    , mdUnit

    -- * LogEvent
    , LogEvent (..)
    , mkLogEvent
    , leCreatedAt
    , leMessage

    -- * RelationalDatabaseHardware
    , RelationalDatabaseHardware (..)
    , mkRelationalDatabaseHardware
    , rdhCpuCount
    , rdhDiskSizeInGb
    , rdhRamSizeInGb

    -- * NetworkProtocol
    , NetworkProtocol (..)

    -- * PendingModifiedRelationalDatabaseValues
    , PendingModifiedRelationalDatabaseValues (..)
    , mkPendingModifiedRelationalDatabaseValues
    , pmrdvBackupRetentionEnabled
    , pmrdvEngineVersion
    , pmrdvMasterUserPassword

    -- * LoadBalancerTlsCertificateRevocationReason
    , LoadBalancerTlsCertificateRevocationReason (..)

    -- * Domain
    , Domain (..)
    , mkDomain
    , dfArn
    , dfCreatedAt
    , dfDomainEntries
    , dfLocation
    , dfName
    , dfResourceType
    , dfSupportCode
    , dfTags

    -- * ContainerImage
    , ContainerImage (..)
    , mkContainerImage
    , ciCreatedAt
    , ciDigest
    , ciImage

    -- * AddOnType
    , AddOnType (..)

    -- * CacheBehavior
    , CacheBehavior (..)
    , mkCacheBehavior
    , cbBehavior

    -- * MetricName
    , MetricName (..)

    -- * DiskInfo
    , DiskInfo (..)
    , mkDiskInfo
    , diIsSystemDisk
    , diName
    , diPath
    , diSizeInGb

    -- * LightsailDistribution
    , LightsailDistribution (..)
    , mkLightsailDistribution
    , ldAbleToUpdateBundle
    , ldAlternativeDomainNames
    , ldArn
    , ldBundleId
    , ldCacheBehaviorSettings
    , ldCacheBehaviors
    , ldCertificateName
    , ldCreatedAt
    , ldDefaultCacheBehavior
    , ldDomainName
    , ldIsEnabled
    , ldLocation
    , ldName
    , ldOrigin
    , ldOriginPublicDNS
    , ldResourceType
    , ldStatus
    , ldSupportCode
    , ldTags

    -- * ForwardValues
    , ForwardValues (..)

    -- * InstanceAccessProtocol
    , InstanceAccessProtocol (..)

    -- * DiskSnapshotState
    , DiskSnapshotState (..)

    -- * InstancePlatform
    , InstancePlatform (..)

    -- * ContactProtocol
    , ContactProtocol (..)

    -- * DistributionBundle
    , DistributionBundle (..)
    , mkDistributionBundle
    , dbBundleId
    , dbIsActive
    , dbName
    , dbPrice
    , dbTransferPerMonthInGb

    -- * MonthlyTransfer
    , MonthlyTransfer (..)
    , mkMonthlyTransfer
    , mtGbPerMonthAllocated

    -- * LoadBalancerAttributeName
    , LoadBalancerAttributeName (..)

    -- * ContainerServiceMetricName
    , ContainerServiceMetricName (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * DistributionMetricName
    , DistributionMetricName (..)

    -- * AutoSnapshotDetails
    , AutoSnapshotDetails (..)
    , mkAutoSnapshotDetails
    , asdCreatedAt
    , asdDate
    , asdFromAttachedDisks
    , asdStatus

    -- * DiskMap
    , DiskMap (..)
    , mkDiskMap
    , dmNewDiskName
    , dmOriginalDiskPath

    -- * RelationalDatabaseBlueprint
    , RelationalDatabaseBlueprint (..)
    , mkRelationalDatabaseBlueprint
    , rdbBlueprintId
    , rdbEngine
    , rdbEngineDescription
    , rdbEngineVersion
    , rdbEngineVersionDescription
    , rdbIsEngineDefault

    -- * AddOnRequest
    , AddOnRequest (..)
    , mkAddOnRequest
    , aorAddOnType
    , aorAutoSnapshotAddOnRequest

    -- * DiskSnapshot
    , DiskSnapshot (..)
    , mkDiskSnapshot
    , dsArn
    , dsCreatedAt
    , dsFromDiskArn
    , dsFromDiskName
    , dsFromInstanceArn
    , dsFromInstanceName
    , dsIsFromAutoSnapshot
    , dsLocation
    , dsName
    , dsProgress
    , dsResourceType
    , dsSizeInGb
    , dsState
    , dsSupportCode
    , dsTags

    -- * StaticIp
    , StaticIp (..)
    , mkStaticIp
    , siArn
    , siAttachedTo
    , siCreatedAt
    , siIpAddress
    , siIsAttached
    , siLocation
    , siName
    , siResourceType
    , siSupportCode

    -- * SensitiveString
    , SensitiveString (..)

    -- * CertificateSummary
    , CertificateSummary (..)
    , mkCertificateSummary
    , cCertificateArn
    , cCertificateDetail
    , cCertificateName
    , cDomainName
    , cTags

    -- * CloudFormationStackRecord
    , CloudFormationStackRecord (..)
    , mkCloudFormationStackRecord
    , cfsrArn
    , cfsrCreatedAt
    , cfsrDestinationInfo
    , cfsrLocation
    , cfsrName
    , cfsrResourceType
    , cfsrSourceInfo
    , cfsrState

    -- * AccessDirection
    , AccessDirection (..)

    -- * ContainerServiceRegistryLogin
    , ContainerServiceRegistryLogin (..)
    , mkContainerServiceRegistryLogin
    , csrlExpiresAt
    , csrlPassword
    , csrlRegistry
    , csrlUsername

    -- * LoadBalancerTlsCertificateDomainValidationOption
    , LoadBalancerTlsCertificateDomainValidationOption (..)
    , mkLoadBalancerTlsCertificateDomainValidationOption
    , lbtcdvoDomainName
    , lbtcdvoValidationStatus

    -- * DomainEntry
    , DomainEntry (..)
    , mkDomainEntry
    , deId
    , deIsAlias
    , deName
    , deOptions
    , deTarget
    , deType

    -- * ContainerServiceProtocol
    , ContainerServiceProtocol (..)

    -- * Bundle
    , Bundle (..)
    , mkBundle
    , bfBundleId
    , bfCpuCount
    , bfDiskSizeInGb
    , bfInstanceType
    , bfIsActive
    , bfName
    , bfPower
    , bfPrice
    , bfRamSizeInGb
    , bfSupportedPlatforms
    , bfTransferPerMonthInGb

    -- * RevocationReason
    , RevocationReason (..)

    -- * LoadBalancerTlsCertificate
    , LoadBalancerTlsCertificate (..)
    , mkLoadBalancerTlsCertificate
    , lbtcArn
    , lbtcCreatedAt
    , lbtcDomainName
    , lbtcDomainValidationRecords
    , lbtcFailureReason
    , lbtcIsAttached
    , lbtcIssuedAt
    , lbtcIssuer
    , lbtcKeyAlgorithm
    , lbtcLoadBalancerName
    , lbtcLocation
    , lbtcName
    , lbtcNotAfter
    , lbtcNotBefore
    , lbtcRenewalSummary
    , lbtcResourceType
    , lbtcRevocationReason
    , lbtcRevokedAt
    , lbtcSerial
    , lbtcSignatureAlgorithm
    , lbtcStatus
    , lbtcSubject
    , lbtcSubjectAlternativeNames
    , lbtcSupportCode
    , lbtcTags

    -- * NonEmptyString
    , NonEmptyString (..)

    -- * PortInfo
    , PortInfo (..)
    , mkPortInfo
    , piCidrListAliases
    , piCidrs
    , piFromPort
    , piProtocol
    , piToPort

    -- * ResourceArn
    , ResourceArn (..)

    -- * DomainName
    , DomainName (..)

    -- * InstanceHardware
    , InstanceHardware (..)
    , mkInstanceHardware
    , ihCpuCount
    , ihDisks
    , ihRamSizeInGb

    -- * AttachedDisk
    , AttachedDisk (..)
    , mkAttachedDisk
    , adPath
    , adSizeInGb

    -- * RelationalDatabaseParameter
    , RelationalDatabaseParameter (..)
    , mkRelationalDatabaseParameter
    , rdpAllowedValues
    , rdpApplyMethod
    , rdpApplyType
    , rdpDataType
    , rdpDescription
    , rdpIsModifiable
    , rdpParameterName
    , rdpParameterValue

    -- * ResourceLocation
    , ResourceLocation (..)
    , mkResourceLocation
    , rlAvailabilityZone
    , rlRegionName

    -- * PasswordData
    , PasswordData (..)
    , mkPasswordData
    , pdCiphertext
    , pdKeyPairName

    -- * Base64
    , Base64 (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azState
    , azZoneName

    -- * InstanceAccessDetails
    , InstanceAccessDetails (..)
    , mkInstanceAccessDetails
    , iadCertKey
    , iadExpiresAt
    , iadHostKeys
    , iadInstanceName
    , iadIpAddress
    , iadPassword
    , iadPasswordData
    , iadPrivateKey
    , iadProtocol
    , iadUsername

    -- * ContactMethod
    , ContactMethod (..)
    , mkContactMethod
    , cmArn
    , cmContactEndpoint
    , cmCreatedAt
    , cmLocation
    , cmName
    , cmProtocol
    , cmResourceType
    , cmStatus
    , cmSupportCode

    -- * BlueprintType
    , BlueprintType (..)

    -- * Container
    , Container (..)
    , mkContainer
    , cCommand
    , cEnvironment
    , cImage
    , cPorts

    -- * MetricUnit
    , MetricUnit (..)

    -- * LoadBalancer
    , LoadBalancer (..)
    , mkLoadBalancer
    , lbArn
    , lbConfigurationOptions
    , lbCreatedAt
    , lbDnsName
    , lbHealthCheckPath
    , lbInstanceHealthSummary
    , lbInstancePort
    , lbLocation
    , lbName
    , lbProtocol
    , lbPublicPorts
    , lbResourceType
    , lbState
    , lbSupportCode
    , lbTags
    , lbTlsCertificateSummaries

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * DiskState
    , DiskState (..)

    -- * InstanceMetricName
    , InstanceMetricName (..)

    -- * EndpointRequest
    , EndpointRequest (..)
    , mkEndpointRequest
    , erContainerName
    , erContainerPort
    , erHealthCheck

    -- * RenewalStatus
    , RenewalStatus (..)

    -- * PortInfoSourceType
    , PortInfoSourceType (..)

    -- * RelationalDatabaseBundle
    , RelationalDatabaseBundle (..)
    , mkRelationalDatabaseBundle
    , rdbBundleId
    , rdbCpuCount
    , rdbDiskSizeInGb
    , rdbIsActive
    , rdbIsEncrypted
    , rdbName
    , rdbPrice
    , rdbRamSizeInGb
    , rdbTransferPerMonthInGb

    -- * CacheBehaviorPerPath
    , CacheBehaviorPerPath (..)
    , mkCacheBehaviorPerPath
    , cbppBehavior
    , cbppPath

    -- * InputOrigin
    , InputOrigin (..)
    , mkInputOrigin
    , ioName
    , ioProtocolPolicy
    , ioRegionName

    -- * RenewalSummary
    , RenewalSummary (..)
    , mkRenewalSummary
    , rsDomainValidationRecords
    , rsRenewalStatus
    , rsRenewalStatusReason
    , rsUpdatedAt

    -- * ContainerServiceDeploymentRequest
    , ContainerServiceDeploymentRequest (..)
    , mkContainerServiceDeploymentRequest
    , csdrContainers
    , csdrPublicEndpoint

    -- * InstanceHealthState
    , InstanceHealthState (..)

    -- * LoadBalancerTlsCertificateDomainValidationRecord
    , LoadBalancerTlsCertificateDomainValidationRecord (..)
    , mkLoadBalancerTlsCertificateDomainValidationRecord
    , lbtcdvrDomainName
    , lbtcdvrName
    , lbtcdvrType
    , lbtcdvrValidationStatus
    , lbtcdvrValue

    -- * CloudFormationStackRecordSourceInfo
    , CloudFormationStackRecordSourceInfo (..)
    , mkCloudFormationStackRecordSourceInfo
    , cfsrsiArn
    , cfsrsiName
    , cfsrsiResourceType

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cfArn
    , cfCreatedAt
    , cfDomainName
    , cfDomainValidationRecords
    , cfEligibleToRenew
    , cfInUseResourceCount
    , cfIssuedAt
    , cfIssuerCA
    , cfKeyAlgorithm
    , cfName
    , cfNotAfter
    , cfNotBefore
    , cfRenewalSummary
    , cfRequestFailureReason
    , cfRevocationReason
    , cfRevokedAt
    , cfSerialNumber
    , cfStatus
    , cfSubjectAlternativeNames
    , cfSupportCode
    , cfTags

    -- * InstanceEntry
    , InstanceEntry (..)
    , mkInstanceEntry
    , ieSourceName
    , ieInstanceType
    , iePortInfoSource
    , ieAvailabilityZone
    , ieUserData

    -- * LoadBalancerState
    , LoadBalancerState (..)

    -- * OperationType
    , OperationType (..)

    -- * ContainerServiceName
    , ContainerServiceName (..)

    -- * ExportSnapshotRecord
    , ExportSnapshotRecord (..)
    , mkExportSnapshotRecord
    , esrArn
    , esrCreatedAt
    , esrDestinationInfo
    , esrLocation
    , esrName
    , esrResourceType
    , esrSourceInfo
    , esrState

    -- * InstanceSnapshot
    , InstanceSnapshot (..)
    , mkInstanceSnapshot
    , isArn
    , isCreatedAt
    , isFromAttachedDisks
    , isFromBlueprintId
    , isFromBundleId
    , isFromInstanceArn
    , isFromInstanceName
    , isIsFromAutoSnapshot
    , isLocation
    , isName
    , isProgress
    , isResourceType
    , isSizeInGb
    , isState
    , isSupportCode
    , isTags

    -- * ContainerServicePower
    , ContainerServicePower (..)
    , mkContainerServicePower
    , cspCpuCount
    , cspIsActive
    , cspName
    , cspPowerId
    , cspPrice
    , cspRamSizeInGb

    -- * RelationalDatabaseEndpoint
    , RelationalDatabaseEndpoint (..)
    , mkRelationalDatabaseEndpoint
    , rdeAddress
    , rdePort

    -- * TagKey
    , TagKey (..)

    -- * RegionInfo
    , RegionInfo (..)
    , mkRegionInfo
    , riAvailabilityZones
    , riContinentCode
    , riDescription
    , riDisplayName
    , riName
    , riRelationalDatabaseAvailabilityZones

    -- * ContactMethodVerificationProtocol
    , ContactMethodVerificationProtocol (..)

    -- * HostKeyAttributes
    , HostKeyAttributes (..)
    , mkHostKeyAttributes
    , hkaAlgorithm
    , hkaFingerprintSHA1
    , hkaFingerprintSHA256
    , hkaNotValidAfter
    , hkaNotValidBefore
    , hkaPublicKey
    , hkaWitnessedAt

    -- * DomainEntryOptionsKeys
    , DomainEntryOptionsKeys (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * RelationalDatabaseEngine
    , RelationalDatabaseEngine (..)

    -- * LoadBalancerTlsCertificateDomainStatus
    , LoadBalancerTlsCertificateDomainStatus (..)

    -- * LoadBalancerTlsCertificateSummary
    , LoadBalancerTlsCertificateSummary (..)
    , mkLoadBalancerTlsCertificateSummary
    , lbtcsIsAttached
    , lbtcsName

    -- * LoadBalancerTlsCertificateFailureReason
    , LoadBalancerTlsCertificateFailureReason (..)

    -- * ExportSnapshotRecordSourceType
    , ExportSnapshotRecordSourceType (..)

    -- * TimeOfDay
    , TimeOfDay (..)

    -- * LoadBalancerTlsCertificateStatus
    , LoadBalancerTlsCertificateStatus (..)

    -- * AddOn
    , AddOn (..)
    , mkAddOn
    , aoName
    , aoNextSnapshotTimeOfDay
    , aoSnapshotTimeOfDay
    , aoStatus

    -- * ContainerServiceDeployment
    , ContainerServiceDeployment (..)
    , mkContainerServiceDeployment
    , csdContainers
    , csdCreatedAt
    , csdPublicEndpoint
    , csdState
    , csdVersion

    -- * InstanceSnapshotInfo
    , InstanceSnapshotInfo (..)
    , mkInstanceSnapshotInfo
    , isiFromBlueprintId
    , isiFromBundleId
    , isiFromDiskInfo

    -- * SerialNumber
    , SerialNumber (..)

    -- * ContactMethodStatus
    , ContactMethodStatus (..)

    -- * Alarm
    , Alarm (..)
    , mkAlarm
    , aArn
    , aComparisonOperator
    , aContactProtocols
    , aCreatedAt
    , aDatapointsToAlarm
    , aEvaluationPeriods
    , aLocation
    , aMetricName
    , aMonitoredResourceInfo
    , aName
    , aNotificationEnabled
    , aNotificationTriggers
    , aPeriod
    , aResourceType
    , aState
    , aStatistic
    , aSupportCode
    , aThreshold
    , aTreatMissingData
    , aUnit

    -- * RelationalDatabasePasswordVersion
    , RelationalDatabasePasswordVersion (..)

    -- * PortState
    , PortState (..)

    -- * RenewalStatusReason
    , RenewalStatusReason (..)

    -- * InstancePortInfo
    , InstancePortInfo (..)
    , mkInstancePortInfo
    , ipiAccessDirection
    , ipiAccessFrom
    , ipiAccessType
    , ipiCidrListAliases
    , ipiCidrs
    , ipiCommonName
    , ipiFromPort
    , ipiProtocol
    , ipiToPort

    -- * DomainValidationRecord
    , DomainValidationRecord (..)
    , mkDomainValidationRecord
    , dvrDomainName
    , dvrResourceRecord

    -- * RelationalDatabase
    , RelationalDatabase (..)
    , mkRelationalDatabase
    , rdArn
    , rdBackupRetentionEnabled
    , rdCaCertificateIdentifier
    , rdCreatedAt
    , rdEngine
    , rdEngineVersion
    , rdHardware
    , rdLatestRestorableTime
    , rdLocation
    , rdMasterDatabaseName
    , rdMasterEndpoint
    , rdMasterUsername
    , rdName
    , rdParameterApplyStatus
    , rdPendingMaintenanceActions
    , rdPendingModifiedValues
    , rdPreferredBackupWindow
    , rdPreferredMaintenanceWindow
    , rdPubliclyAccessible
    , rdRelationalDatabaseBlueprintId
    , rdRelationalDatabaseBundleId
    , rdResourceType
    , rdSecondaryAvailabilityZone
    , rdState
    , rdSupportCode
    , rdTags

    -- * LoadBalancerTlsCertificateRenewalSummary
    , LoadBalancerTlsCertificateRenewalSummary (..)
    , mkLoadBalancerTlsCertificateRenewalSummary
    , lbtcrsDomainValidationOptions
    , lbtcrsRenewalStatus

    -- * ExportSnapshotRecordSourceInfo
    , ExportSnapshotRecordSourceInfo (..)
    , mkExportSnapshotRecordSourceInfo
    , esrsiArn
    , esrsiCreatedAt
    , esrsiDiskSnapshotInfo
    , esrsiFromResourceArn
    , esrsiFromResourceName
    , esrsiInstanceSnapshotInfo
    , esrsiName
    , esrsiResourceType

    -- * LoadBalancerTlsCertificateRenewalStatus
    , LoadBalancerTlsCertificateRenewalStatus (..)

    -- * InstanceState
    , InstanceState (..)
    , mkInstanceState
    , isfCode
    , isfName

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * IssuerCA
    , IssuerCA (..)

    -- * CacheSettings
    , CacheSettings (..)
    , mkCacheSettings
    , csAllowedHTTPMethods
    , csCachedHTTPMethods
    , csDefaultTTL
    , csForwardedCookies
    , csForwardedHeaders
    , csForwardedQueryStrings
    , csMaximumTTL
    , csMinimumTTL

    -- * InstanceHealthReason
    , InstanceHealthReason (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iAddOns
    , iArn
    , iBlueprintId
    , iBlueprintName
    , iBundleId
    , iCreatedAt
    , iHardware
    , iIpv6Address
    , iIsStaticIp
    , iLocation
    , iName
    , iNetworking
    , iPrivateIpAddress
    , iPublicIpAddress
    , iResourceType
    , iSshKeyName
    , iState
    , iSupportCode
    , iTags
    , iUsername

    -- * MonitoredResourceInfo
    , MonitoredResourceInfo (..)
    , mkMonitoredResourceInfo
    , mriArn
    , mriName
    , mriResourceType

    -- * ContainerServiceEndpoint
    , ContainerServiceEndpoint (..)
    , mkContainerServiceEndpoint
    , cseContainerName
    , cseContainerPort
    , cseHealthCheck

    -- * OriginProtocolPolicyEnum
    , OriginProtocolPolicyEnum (..)

    -- * CookieObject
    , CookieObject (..)
    , mkCookieObject
    , coCookiesAllowList
    , coOption

    -- * LoadBalancerMetricName
    , LoadBalancerMetricName (..)

    -- * InstanceHealthSummary
    , InstanceHealthSummary (..)
    , mkInstanceHealthSummary
    , ihsInstanceHealth
    , ihsInstanceHealthReason
    , ihsInstanceName

    -- * LoadBalancerName
    , LoadBalancerName (..)

    -- * ServiceName
    , ServiceName (..)

    -- * Action
    , Action (..)

    -- * Description
    , Description (..)

    -- * Name
    , Name (..)

    -- * RelationalDatabaseName
    , RelationalDatabaseName (..)

    -- * AlarmName
    , AlarmName (..)

    -- * DistributionName
    , DistributionName (..)

    -- * RelationalDatabaseSnapshotName
    , RelationalDatabaseSnapshotName (..)

    -- * BlueprintId
    , BlueprintId (..)

    -- * Group
    , Group (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * DiskName
    , DiskName (..)

    -- * DiskSnapshotName
    , DiskSnapshotName (..)

    -- * Arn
    , Arn (..)

    -- * InstanceSnapshotName
    , InstanceSnapshotName (..)

    -- * Resource
    , Resource (..)

    -- * SnapshotTimeOfDay
    , SnapshotTimeOfDay (..)

    -- * Id
    , Id (..)

    -- * Service
    , Service (..)

    -- * Engine
    , Engine (..)

    -- * EngineVersion
    , EngineVersion (..)

    -- * FromRelationalDatabaseArn
    , FromRelationalDatabaseArn (..)

    -- * FromRelationalDatabaseName
    , FromRelationalDatabaseName (..)

    -- * State
    , State (..)

    -- * ContactEndpoint
    , ContactEndpoint (..)

    -- * AttachedTo
    , AttachedTo (..)

    -- * Fingerprint
    , Fingerprint (..)

    -- * BundleId
    , BundleId (..)

    -- * Label
    , Label (..)

    -- * MasterUserPassword
    , MasterUserPassword (..)

    -- * Path
    , Path (..)

    -- * Date
    , Date (..)

    -- * PrivateKeyBase64
    , PrivateKeyBase64 (..)

    -- * PublicKeyBase64
    , PublicKeyBase64 (..)

    -- * AttributeValue
    , AttributeValue (..)

    -- * OriginalDiskPath
    , OriginalDiskPath (..)

    -- * FromDiskArn
    , FromDiskArn (..)

    -- * FromInstanceArn
    , FromInstanceArn (..)

    -- * CertificateArn
    , CertificateArn (..)

    -- * Issuer
    , Issuer (..)

    -- * Serial
    , Serial (..)

    -- * SignatureAlgorithm
    , SignatureAlgorithm (..)

    -- * Subject
    , Subject (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Lightsail.Types.AlarmState
  
import Network.AWS.Lightsail.Types.HeaderEnum
  
import Network.AWS.Lightsail.Types.DomainEntryType
  
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
  
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
  
import Network.AWS.Lightsail.Types.Origin
  
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
  
  
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
  
import Network.AWS.Lightsail.Types.IpV6Address
  
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
  
import Network.AWS.Lightsail.Types.BehaviorEnum
  
import Network.AWS.Lightsail.Types.Blueprint
  
import Network.AWS.Lightsail.Types.InstancePortState
  
import Network.AWS.Lightsail.Types.InstanceNetworking
  
import Network.AWS.Lightsail.Types.IpAddress
  
import Network.AWS.Lightsail.Types.ResourceRecord
  
import Network.AWS.Lightsail.Types.RegionName
  
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
  
import Network.AWS.Lightsail.Types.InstanceSnapshotState
  
import Network.AWS.Lightsail.Types.Tag
  
import Network.AWS.Lightsail.Types.TreatMissingData
  
import Network.AWS.Lightsail.Types.ContainerService
  
import Network.AWS.Lightsail.Types.CertificateName
  
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
  
import Network.AWS.Lightsail.Types.RecordState
  
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
  
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
  
import Network.AWS.Lightsail.Types.HeaderObject
  
import Network.AWS.Lightsail.Types.ResourceType
  
import Network.AWS.Lightsail.Types.DestinationInfo
  
import Network.AWS.Lightsail.Types.QueryStringObject
  
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
  
import Network.AWS.Lightsail.Types.ContainerServiceLogEvent
  
import Network.AWS.Lightsail.Types.ContainerServicePowerName
  
import Network.AWS.Lightsail.Types.PortAccessType
  
import Network.AWS.Lightsail.Types.Disk
  
import Network.AWS.Lightsail.Types.EligibleToRenew
  
import Network.AWS.Lightsail.Types.ContainerServiceState
  
import Network.AWS.Lightsail.Types.ResourceName
  
import Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
  
import Network.AWS.Lightsail.Types.KeyPair
  
import Network.AWS.Lightsail.Types.RequestFailureReason
  
import Network.AWS.Lightsail.Types.Operation
  
import Network.AWS.Lightsail.Types.ContainerName
  
import Network.AWS.Lightsail.Types.MetricDatapoint
  
import Network.AWS.Lightsail.Types.LogEvent
  
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
  
import Network.AWS.Lightsail.Types.NetworkProtocol
  
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
  
import Network.AWS.Lightsail.Types.Domain
  
import Network.AWS.Lightsail.Types.ContainerImage
  
import Network.AWS.Lightsail.Types.AddOnType
  
import Network.AWS.Lightsail.Types.CacheBehavior
  
import Network.AWS.Lightsail.Types.MetricName
  
import Network.AWS.Lightsail.Types.DiskInfo
  
import Network.AWS.Lightsail.Types.LightsailDistribution
  
  
import Network.AWS.Lightsail.Types.ForwardValues
  
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
  
import Network.AWS.Lightsail.Types.DiskSnapshotState
  
import Network.AWS.Lightsail.Types.InstancePlatform
  
import Network.AWS.Lightsail.Types.ContactProtocol
  
import Network.AWS.Lightsail.Types.DistributionBundle
  
import Network.AWS.Lightsail.Types.MonthlyTransfer
  
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
  
import Network.AWS.Lightsail.Types.ContainerServiceMetricName
  
import Network.AWS.Lightsail.Types.OperationStatus
  
import Network.AWS.Lightsail.Types.ComparisonOperator
  
import Network.AWS.Lightsail.Types.DistributionMetricName
  
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
  
import Network.AWS.Lightsail.Types.DiskMap
  
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
  
import Network.AWS.Lightsail.Types.AddOnRequest
  
import Network.AWS.Lightsail.Types.DiskSnapshot
  
import Network.AWS.Lightsail.Types.StaticIp
  
import Network.AWS.Lightsail.Types.SensitiveString
  
import Network.AWS.Lightsail.Types.CertificateSummary
  
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
  
import Network.AWS.Lightsail.Types.AccessDirection
  
import Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
  
import Network.AWS.Lightsail.Types.DomainEntry
  
import Network.AWS.Lightsail.Types.ContainerServiceProtocol
  
  
import Network.AWS.Lightsail.Types.Bundle
  
import Network.AWS.Lightsail.Types.RevocationReason
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
  
import Network.AWS.Lightsail.Types.NonEmptyString
  
import Network.AWS.Lightsail.Types.PortInfo
  
import Network.AWS.Lightsail.Types.ResourceArn
  
import Network.AWS.Lightsail.Types.DomainName
  
import Network.AWS.Lightsail.Types.InstanceHardware
  
import Network.AWS.Lightsail.Types.AttachedDisk
  
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
  
import Network.AWS.Lightsail.Types.ResourceLocation
  
import Network.AWS.Lightsail.Types.PasswordData
  
import Network.AWS.Lightsail.Types.Base64
  
import Network.AWS.Lightsail.Types.AvailabilityZone
  
import Network.AWS.Lightsail.Types.InstanceAccessDetails
  
import Network.AWS.Lightsail.Types.ContactMethod
  
import Network.AWS.Lightsail.Types.BlueprintType
  
import Network.AWS.Lightsail.Types.Container
  
import Network.AWS.Lightsail.Types.MetricUnit
  
import Network.AWS.Lightsail.Types.LoadBalancer
  
import Network.AWS.Lightsail.Types.MetricStatistic
  
import Network.AWS.Lightsail.Types.DiskState
  
import Network.AWS.Lightsail.Types.InstanceMetricName
  
import Network.AWS.Lightsail.Types.EndpointRequest
  
import Network.AWS.Lightsail.Types.RenewalStatus
  
  
import Network.AWS.Lightsail.Types.PortInfoSourceType
  
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
  
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
  
import Network.AWS.Lightsail.Types.InputOrigin
  
import Network.AWS.Lightsail.Types.RenewalSummary
  
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
  
  
import Network.AWS.Lightsail.Types.InstanceHealthState
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
  
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
  
import Network.AWS.Lightsail.Types.Certificate
  
import Network.AWS.Lightsail.Types.InstanceEntry
  
import Network.AWS.Lightsail.Types.LoadBalancerState
  
import Network.AWS.Lightsail.Types.OperationType
  
import Network.AWS.Lightsail.Types.ContainerServiceName
  
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
  
import Network.AWS.Lightsail.Types.InstanceSnapshot
  
import Network.AWS.Lightsail.Types.ContainerServicePower
  
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
  
import Network.AWS.Lightsail.Types.TagKey
  
  
import Network.AWS.Lightsail.Types.RegionInfo
  
import Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
  
import Network.AWS.Lightsail.Types.HostKeyAttributes
  
import Network.AWS.Lightsail.Types.DomainEntryOptionsKeys
  
import Network.AWS.Lightsail.Types.KeyAlgorithm
  
  
import Network.AWS.Lightsail.Types.RelationalDatabaseEngine
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
  
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
  
import Network.AWS.Lightsail.Types.TimeOfDay
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
  
import Network.AWS.Lightsail.Types.AddOn
  
import Network.AWS.Lightsail.Types.ContainerServiceDeployment
  
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
  
import Network.AWS.Lightsail.Types.SerialNumber
  
import Network.AWS.Lightsail.Types.ContactMethodStatus
  
import Network.AWS.Lightsail.Types.Alarm
  
import Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
  
import Network.AWS.Lightsail.Types.PortState
  
import Network.AWS.Lightsail.Types.RenewalStatusReason
  
import Network.AWS.Lightsail.Types.InstancePortInfo
  
import Network.AWS.Lightsail.Types.DomainValidationRecord
  
import Network.AWS.Lightsail.Types.RelationalDatabase
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
  
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
  
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
  
import Network.AWS.Lightsail.Types.InstanceState
  
import Network.AWS.Lightsail.Types.CertificateStatus
  
import Network.AWS.Lightsail.Types.IssuerCA
  
import Network.AWS.Lightsail.Types.CacheSettings
  
import Network.AWS.Lightsail.Types.InstanceHealthReason
  
import Network.AWS.Lightsail.Types.Instance
  
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
  
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
  
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
  
import Network.AWS.Lightsail.Types.CookieObject
  
import Network.AWS.Lightsail.Types.LoadBalancerMetricName
  
import Network.AWS.Lightsail.Types.InstanceHealthSummary
  
import Network.AWS.Lightsail.Types.LoadBalancerName
  
import Network.AWS.Lightsail.Types.ServiceName
  
import Network.AWS.Lightsail.Types.Action
  
import Network.AWS.Lightsail.Types.Description
  
import Network.AWS.Lightsail.Types.Name
  
import Network.AWS.Lightsail.Types.RelationalDatabaseName
  
import Network.AWS.Lightsail.Types.AlarmName
  
import Network.AWS.Lightsail.Types.DistributionName
  
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshotName
  
import Network.AWS.Lightsail.Types.BlueprintId
  
import Network.AWS.Lightsail.Types.Group
  
import Network.AWS.Lightsail.Types.Key
  
import Network.AWS.Lightsail.Types.Value
  
import Network.AWS.Lightsail.Types.DiskName
  
import Network.AWS.Lightsail.Types.DiskSnapshotName
  
import Network.AWS.Lightsail.Types.Arn
  
import Network.AWS.Lightsail.Types.InstanceSnapshotName
  
import Network.AWS.Lightsail.Types.Resource
  
import Network.AWS.Lightsail.Types.SnapshotTimeOfDay
  
import Network.AWS.Lightsail.Types.Id
  
import Network.AWS.Lightsail.Types.Service
  
import Network.AWS.Lightsail.Types.Engine
  
import Network.AWS.Lightsail.Types.EngineVersion
  
import Network.AWS.Lightsail.Types.FromRelationalDatabaseArn
  
import Network.AWS.Lightsail.Types.FromRelationalDatabaseName
  
import Network.AWS.Lightsail.Types.State
  
import Network.AWS.Lightsail.Types.ContactEndpoint
  
import Network.AWS.Lightsail.Types.AttachedTo
  
import Network.AWS.Lightsail.Types.Fingerprint
  
import Network.AWS.Lightsail.Types.BundleId
  
import Network.AWS.Lightsail.Types.Label
  
import Network.AWS.Lightsail.Types.MasterUserPassword
  
import Network.AWS.Lightsail.Types.Path
  
import Network.AWS.Lightsail.Types.Date
  
import Network.AWS.Lightsail.Types.PrivateKeyBase64
  
import Network.AWS.Lightsail.Types.PublicKeyBase64
  
import Network.AWS.Lightsail.Types.AttributeValue
  
import Network.AWS.Lightsail.Types.OriginalDiskPath
  
import Network.AWS.Lightsail.Types.FromDiskArn
  
import Network.AWS.Lightsail.Types.FromInstanceArn
  
import Network.AWS.Lightsail.Types.CertificateArn
  
import Network.AWS.Lightsail.Types.Issuer
  
import Network.AWS.Lightsail.Types.Serial
  
import Network.AWS.Lightsail.Types.SignatureAlgorithm
  
import Network.AWS.Lightsail.Types.Subject
  

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Lightsail",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "lightsail",
                 Core._svcVersion = "2016-11-28", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Lightsail",
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

-- | Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | Lightsail throws this exception when an account is still in the setup in progress state.
_AccountSetupInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountSetupInProgressException
  = Core._MatchServiceError mkServiceConfig
      "AccountSetupInProgressException"
{-# INLINEABLE _AccountSetupInProgressException #-}
{-# DEPRECATED _AccountSetupInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | Lightsail throws this exception when it cannot find a resource.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Lightsail throws this exception when an operation fails to execute.
_OperationFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationFailureException
  = Core._MatchServiceError mkServiceConfig
      "OperationFailureException"
{-# INLINEABLE _OperationFailureException #-}
{-# DEPRECATED _OperationFailureException "Use generic-lens or generic-optics instead"  #-}

-- | A general service exception.
_ServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceException
  = Core._MatchServiceError mkServiceConfig "ServiceException"
{-# INLINEABLE _ServiceException #-}
{-# DEPRECATED _ServiceException "Use generic-lens or generic-optics instead"  #-}

-- | Lightsail throws this exception when the user has not been authenticated.
_UnauthenticatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthenticatedException
  = Core._MatchServiceError mkServiceConfig
      "UnauthenticatedException"
{-# INLINEABLE _UnauthenticatedException #-}
{-# DEPRECATED _UnauthenticatedException "Use generic-lens or generic-optics instead"  #-}

-- | Lightsail throws this exception when user input does not conform to the validation rules of an input field.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}
