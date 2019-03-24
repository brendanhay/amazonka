{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, a managed database, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).
--
--
-- For more information about Lightsail concepts and tasks, see the <https://lightsail.aws.amazon.com/ls/docs/all Lightsail Dev Guide> .
--
-- To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Lightsail Dev Guide> .
--
module Network.AWS.Lightsail
    (
    -- * Service Configuration
      lightsail

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** AccountSetupInProgressException
    , _AccountSetupInProgressException

    -- ** NotFoundException
    , _NotFoundException

    -- ** OperationFailureException
    , _OperationFailureException

    -- ** ServiceException
    , _ServiceException

    -- ** UnauthenticatedException
    , _UnauthenticatedException

    -- ** InvalidInputException
    , _InvalidInputException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloseInstancePublicPorts
    , module Network.AWS.Lightsail.CloseInstancePublicPorts

    -- ** GetRelationalDatabaseMetricData
    , module Network.AWS.Lightsail.GetRelationalDatabaseMetricData

    -- ** AllocateStaticIP
    , module Network.AWS.Lightsail.AllocateStaticIP

    -- ** DeleteKeyPair
    , module Network.AWS.Lightsail.DeleteKeyPair

    -- ** DeleteInstanceSnapshot
    , module Network.AWS.Lightsail.DeleteInstanceSnapshot

    -- ** GetInstances (Paginated)
    , module Network.AWS.Lightsail.GetInstances

    -- ** GetLoadBalancer
    , module Network.AWS.Lightsail.GetLoadBalancer

    -- ** GetInstance
    , module Network.AWS.Lightsail.GetInstance

    -- ** GetRelationalDatabaseEvents (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabaseEvents

    -- ** GetRelationalDatabaseSnapshots (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots

    -- ** AttachStaticIP
    , module Network.AWS.Lightsail.AttachStaticIP

    -- ** GetRelationalDatabaseParameters (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabaseParameters

    -- ** DetachDisk
    , module Network.AWS.Lightsail.DetachDisk

    -- ** DownloadDefaultKeyPair
    , module Network.AWS.Lightsail.DownloadDefaultKeyPair

    -- ** DeleteLoadBalancerTLSCertificate
    , module Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate

    -- ** GetDomains (Paginated)
    , module Network.AWS.Lightsail.GetDomains

    -- ** UpdateRelationalDatabaseParameters
    , module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters

    -- ** CreateLoadBalancerTLSCertificate
    , module Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate

    -- ** CreateDomainEntry
    , module Network.AWS.Lightsail.CreateDomainEntry

    -- ** ImportKeyPair
    , module Network.AWS.Lightsail.ImportKeyPair

    -- ** GetInstanceSnapshots (Paginated)
    , module Network.AWS.Lightsail.GetInstanceSnapshots

    -- ** ExportSnapshot
    , module Network.AWS.Lightsail.ExportSnapshot

    -- ** CreateRelationalDatabaseFromSnapshot
    , module Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot

    -- ** CreateCloudFormationStack
    , module Network.AWS.Lightsail.CreateCloudFormationStack

    -- ** GetExportSnapshotRecords (Paginated)
    , module Network.AWS.Lightsail.GetExportSnapshotRecords

    -- ** ReleaseStaticIP
    , module Network.AWS.Lightsail.ReleaseStaticIP

    -- ** DeleteInstance
    , module Network.AWS.Lightsail.DeleteInstance

    -- ** RebootInstance
    , module Network.AWS.Lightsail.RebootInstance

    -- ** DeleteLoadBalancer
    , module Network.AWS.Lightsail.DeleteLoadBalancer

    -- ** CreateDiskFromSnapshot
    , module Network.AWS.Lightsail.CreateDiskFromSnapshot

    -- ** GetRelationalDatabases (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabases

    -- ** GetInstanceSnapshot
    , module Network.AWS.Lightsail.GetInstanceSnapshot

    -- ** GetRelationalDatabaseLogEvents
    , module Network.AWS.Lightsail.GetRelationalDatabaseLogEvents

    -- ** GetRelationalDatabaseLogStreams
    , module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams

    -- ** GetDomain
    , module Network.AWS.Lightsail.GetDomain

    -- ** GetActiveNames (Paginated)
    , module Network.AWS.Lightsail.GetActiveNames

    -- ** StopRelationalDatabase
    , module Network.AWS.Lightsail.StopRelationalDatabase

    -- ** CreateRelationalDatabaseSnapshot
    , module Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot

    -- ** GetInstanceAccessDetails
    , module Network.AWS.Lightsail.GetInstanceAccessDetails

    -- ** StopInstance
    , module Network.AWS.Lightsail.StopInstance

    -- ** DetachInstancesFromLoadBalancer
    , module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer

    -- ** CreateInstanceSnapshot
    , module Network.AWS.Lightsail.CreateInstanceSnapshot

    -- ** CopySnapshot
    , module Network.AWS.Lightsail.CopySnapshot

    -- ** GetRelationalDatabaseSnapshot
    , module Network.AWS.Lightsail.GetRelationalDatabaseSnapshot

    -- ** IsVPCPeered
    , module Network.AWS.Lightsail.IsVPCPeered

    -- ** GetStaticIPs (Paginated)
    , module Network.AWS.Lightsail.GetStaticIPs

    -- ** UnpeerVPC
    , module Network.AWS.Lightsail.UnpeerVPC

    -- ** DeleteDisk
    , module Network.AWS.Lightsail.DeleteDisk

    -- ** CreateInstancesFromSnapshot
    , module Network.AWS.Lightsail.CreateInstancesFromSnapshot

    -- ** GetCloudFormationStackRecords (Paginated)
    , module Network.AWS.Lightsail.GetCloudFormationStackRecords

    -- ** CreateDomain
    , module Network.AWS.Lightsail.CreateDomain

    -- ** GetRelationalDatabaseBlueprints (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints

    -- ** GetDiskSnapshots (Paginated)
    , module Network.AWS.Lightsail.GetDiskSnapshots

    -- ** PeerVPC
    , module Network.AWS.Lightsail.PeerVPC

    -- ** GetRelationalDatabaseBundles (Paginated)
    , module Network.AWS.Lightsail.GetRelationalDatabaseBundles

    -- ** GetLoadBalancers (Paginated)
    , module Network.AWS.Lightsail.GetLoadBalancers

    -- ** RebootRelationalDatabase
    , module Network.AWS.Lightsail.RebootRelationalDatabase

    -- ** AttachLoadBalancerTLSCertificate
    , module Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate

    -- ** UpdateLoadBalancerAttribute
    , module Network.AWS.Lightsail.UpdateLoadBalancerAttribute

    -- ** DeleteRelationalDatabase
    , module Network.AWS.Lightsail.DeleteRelationalDatabase

    -- ** GetDiskSnapshot
    , module Network.AWS.Lightsail.GetDiskSnapshot

    -- ** UpdateRelationalDatabase
    , module Network.AWS.Lightsail.UpdateRelationalDatabase

    -- ** GetStaticIP
    , module Network.AWS.Lightsail.GetStaticIP

    -- ** GetRelationalDatabaseMasterUserPassword
    , module Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword

    -- ** GetBlueprints (Paginated)
    , module Network.AWS.Lightsail.GetBlueprints

    -- ** GetInstancePortStates
    , module Network.AWS.Lightsail.GetInstancePortStates

    -- ** CreateRelationalDatabase
    , module Network.AWS.Lightsail.CreateRelationalDatabase

    -- ** CreateDiskSnapshot
    , module Network.AWS.Lightsail.CreateDiskSnapshot

    -- ** DeleteDomainEntry
    , module Network.AWS.Lightsail.DeleteDomainEntry

    -- ** UpdateDomainEntry
    , module Network.AWS.Lightsail.UpdateDomainEntry

    -- ** GetRegions
    , module Network.AWS.Lightsail.GetRegions

    -- ** DeleteDiskSnapshot
    , module Network.AWS.Lightsail.DeleteDiskSnapshot

    -- ** GetLoadBalancerMetricData
    , module Network.AWS.Lightsail.GetLoadBalancerMetricData

    -- ** GetInstanceState
    , module Network.AWS.Lightsail.GetInstanceState

    -- ** GetKeyPairs (Paginated)
    , module Network.AWS.Lightsail.GetKeyPairs

    -- ** GetOperations (Paginated)
    , module Network.AWS.Lightsail.GetOperations

    -- ** GetDisks (Paginated)
    , module Network.AWS.Lightsail.GetDisks

    -- ** GetRelationalDatabase
    , module Network.AWS.Lightsail.GetRelationalDatabase

    -- ** AttachInstancesToLoadBalancer
    , module Network.AWS.Lightsail.AttachInstancesToLoadBalancer

    -- ** TagResource
    , module Network.AWS.Lightsail.TagResource

    -- ** GetOperation
    , module Network.AWS.Lightsail.GetOperation

    -- ** DeleteRelationalDatabaseSnapshot
    , module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot

    -- ** GetInstanceMetricData
    , module Network.AWS.Lightsail.GetInstanceMetricData

    -- ** GetKeyPair
    , module Network.AWS.Lightsail.GetKeyPair

    -- ** UntagResource
    , module Network.AWS.Lightsail.UntagResource

    -- ** PutInstancePublicPorts
    , module Network.AWS.Lightsail.PutInstancePublicPorts

    -- ** GetDisk
    , module Network.AWS.Lightsail.GetDisk

    -- ** CreateLoadBalancer
    , module Network.AWS.Lightsail.CreateLoadBalancer

    -- ** DeleteKnownHostKeys
    , module Network.AWS.Lightsail.DeleteKnownHostKeys

    -- ** AttachDisk
    , module Network.AWS.Lightsail.AttachDisk

    -- ** DetachStaticIP
    , module Network.AWS.Lightsail.DetachStaticIP

    -- ** CreateInstances
    , module Network.AWS.Lightsail.CreateInstances

    -- ** OpenInstancePublicPorts
    , module Network.AWS.Lightsail.OpenInstancePublicPorts

    -- ** StartRelationalDatabase
    , module Network.AWS.Lightsail.StartRelationalDatabase

    -- ** GetBundles (Paginated)
    , module Network.AWS.Lightsail.GetBundles

    -- ** DeleteDomain
    , module Network.AWS.Lightsail.DeleteDomain

    -- ** GetLoadBalancerTLSCertificates
    , module Network.AWS.Lightsail.GetLoadBalancerTLSCertificates

    -- ** CreateDisk
    , module Network.AWS.Lightsail.CreateDisk

    -- ** GetOperationsForResource
    , module Network.AWS.Lightsail.GetOperationsForResource

    -- ** CreateKeyPair
    , module Network.AWS.Lightsail.CreateKeyPair

    -- ** StartInstance
    , module Network.AWS.Lightsail.StartInstance

    -- * Types

    -- ** AccessDirection
    , AccessDirection (..)

    -- ** BlueprintType
    , BlueprintType (..)

    -- ** CloudFormationStackRecordSourceType
    , CloudFormationStackRecordSourceType (..)

    -- ** DiskSnapshotState
    , DiskSnapshotState (..)

    -- ** DiskState
    , DiskState (..)

    -- ** ExportSnapshotRecordSourceType
    , ExportSnapshotRecordSourceType (..)

    -- ** InstanceAccessProtocol
    , InstanceAccessProtocol (..)

    -- ** InstanceHealthReason
    , InstanceHealthReason (..)

    -- ** InstanceHealthState
    , InstanceHealthState (..)

    -- ** InstanceMetricName
    , InstanceMetricName (..)

    -- ** InstancePlatform
    , InstancePlatform (..)

    -- ** InstanceSnapshotState
    , InstanceSnapshotState (..)

    -- ** LoadBalancerAttributeName
    , LoadBalancerAttributeName (..)

    -- ** LoadBalancerMetricName
    , LoadBalancerMetricName (..)

    -- ** LoadBalancerProtocol
    , LoadBalancerProtocol (..)

    -- ** LoadBalancerState
    , LoadBalancerState (..)

    -- ** LoadBalancerTLSCertificateDomainStatus
    , LoadBalancerTLSCertificateDomainStatus (..)

    -- ** LoadBalancerTLSCertificateFailureReason
    , LoadBalancerTLSCertificateFailureReason (..)

    -- ** LoadBalancerTLSCertificateRenewalStatus
    , LoadBalancerTLSCertificateRenewalStatus (..)

    -- ** LoadBalancerTLSCertificateRevocationReason
    , LoadBalancerTLSCertificateRevocationReason (..)

    -- ** LoadBalancerTLSCertificateStatus
    , LoadBalancerTLSCertificateStatus (..)

    -- ** MetricStatistic
    , MetricStatistic (..)

    -- ** MetricUnit
    , MetricUnit (..)

    -- ** NetworkProtocol
    , NetworkProtocol (..)

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** OperationType
    , OperationType (..)

    -- ** PortAccessType
    , PortAccessType (..)

    -- ** PortInfoSourceType
    , PortInfoSourceType (..)

    -- ** PortState
    , PortState (..)

    -- ** RecordState
    , RecordState (..)

    -- ** RegionName
    , RegionName (..)

    -- ** RelationalDatabaseEngine
    , RelationalDatabaseEngine (..)

    -- ** RelationalDatabaseMetricName
    , RelationalDatabaseMetricName (..)

    -- ** RelationalDatabasePasswordVersion
    , RelationalDatabasePasswordVersion (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azState
    , azZoneName

    -- ** Blueprint
    , Blueprint
    , blueprint
    , bVersionCode
    , bPlatform
    , bGroup
    , bMinPower
    , bProductURL
    , bLicenseURL
    , bName
    , bVersion
    , bBlueprintId
    , bType
    , bIsActive
    , bDescription

    -- ** Bundle
    , Bundle
    , bundle
    , bunCpuCount
    , bunTransferPerMonthInGb
    , bunBundleId
    , bunInstanceType
    , bunName
    , bunPower
    , bunDiskSizeInGb
    , bunSupportedPlatforms
    , bunPrice
    , bunIsActive
    , bunRamSizeInGb

    -- ** CloudFormationStackRecord
    , CloudFormationStackRecord
    , cloudFormationStackRecord
    , cfsrState
    , cfsrDestinationInfo
    , cfsrResourceType
    , cfsrArn
    , cfsrCreatedAt
    , cfsrLocation
    , cfsrName
    , cfsrSourceInfo

    -- ** CloudFormationStackRecordSourceInfo
    , CloudFormationStackRecordSourceInfo
    , cloudFormationStackRecordSourceInfo
    , cfsrsiResourceType
    , cfsrsiArn
    , cfsrsiName

    -- ** DestinationInfo
    , DestinationInfo
    , destinationInfo
    , diService
    , diId

    -- ** Disk
    , Disk
    , disk
    , dState
    , dResourceType
    , dArn
    , dPath
    , dCreatedAt
    , dLocation
    , dIops
    , dIsAttached
    , dAttachmentState
    , dName
    , dSizeInGb
    , dSupportCode
    , dIsSystemDisk
    , dAttachedTo
    , dGbInUse
    , dTags

    -- ** DiskInfo
    , DiskInfo
    , diskInfo
    , diPath
    , diName
    , diSizeInGb
    , diIsSystemDisk

    -- ** DiskMap
    , DiskMap
    , diskMap
    , dmNewDiskName
    , dmOriginalDiskPath

    -- ** DiskSnapshot
    , DiskSnapshot
    , diskSnapshot
    , dsFromDiskName
    , dsState
    , dsResourceType
    , dsArn
    , dsCreatedAt
    , dsLocation
    , dsProgress
    , dsName
    , dsSizeInGb
    , dsSupportCode
    , dsFromInstanceARN
    , dsFromInstanceName
    , dsFromDiskARN
    , dsTags

    -- ** DiskSnapshotInfo
    , DiskSnapshotInfo
    , diskSnapshotInfo
    , dsiSizeInGb

    -- ** Domain
    , Domain
    , domain
    , domResourceType
    , domDomainEntries
    , domArn
    , domCreatedAt
    , domLocation
    , domName
    , domSupportCode
    , domTags

    -- ** DomainEntry
    , DomainEntry
    , domainEntry
    , deIsAlias
    , deName
    , deId
    , deOptions
    , deType
    , deTarget

    -- ** ExportSnapshotRecord
    , ExportSnapshotRecord
    , exportSnapshotRecord
    , esrState
    , esrDestinationInfo
    , esrResourceType
    , esrArn
    , esrCreatedAt
    , esrLocation
    , esrName
    , esrSourceInfo

    -- ** ExportSnapshotRecordSourceInfo
    , ExportSnapshotRecordSourceInfo
    , exportSnapshotRecordSourceInfo
    , esrsiDiskSnapshotInfo
    , esrsiResourceType
    , esrsiArn
    , esrsiCreatedAt
    , esrsiFromResourceARN
    , esrsiName
    , esrsiInstanceSnapshotInfo
    , esrsiFromResourceName

    -- ** HostKeyAttributes
    , HostKeyAttributes
    , hostKeyAttributes
    , hkaNotValidAfter
    , hkaNotValidBefore
    , hkaFingerprintSHA1
    , hkaPublicKey
    , hkaAlgorithm
    , hkaWitnessedAt
    , hkaFingerprintSHA256

    -- ** Instance
    , Instance
    , instance'
    , iState
    , iIpv6Address
    , iResourceType
    , iArn
    , iCreatedAt
    , iLocation
    , iSshKeyName
    , iUsername
    , iNetworking
    , iBundleId
    , iName
    , iSupportCode
    , iBlueprintId
    , iPrivateIPAddress
    , iBlueprintName
    , iIsStaticIP
    , iPublicIPAddress
    , iHardware
    , iTags

    -- ** InstanceAccessDetails
    , InstanceAccessDetails
    , instanceAccessDetails
    , iadHostKeys
    , iadCertKey
    , iadIpAddress
    , iadPrivateKey
    , iadExpiresAt
    , iadUsername
    , iadProtocol
    , iadPasswordData
    , iadPassword
    , iadInstanceName

    -- ** InstanceEntry
    , InstanceEntry
    , instanceEntry
    , ieUserData
    , ieSourceName
    , ieInstanceType
    , iePortInfoSource
    , ieAvailabilityZone

    -- ** InstanceHardware
    , InstanceHardware
    , instanceHardware
    , ihCpuCount
    , ihDisks
    , ihRamSizeInGb

    -- ** InstanceHealthSummary
    , InstanceHealthSummary
    , instanceHealthSummary
    , ihsInstanceHealth
    , ihsInstanceName
    , ihsInstanceHealthReason

    -- ** InstanceNetworking
    , InstanceNetworking
    , instanceNetworking
    , inMonthlyTransfer
    , inPorts

    -- ** InstancePortInfo
    , InstancePortInfo
    , instancePortInfo
    , ipiFromPort
    , ipiCommonName
    , ipiProtocol
    , ipiAccessDirection
    , ipiAccessType
    , ipiToPort
    , ipiAccessFrom

    -- ** InstancePortState
    , InstancePortState
    , instancePortState
    , ipsFromPort
    , ipsState
    , ipsProtocol
    , ipsToPort

    -- ** InstanceSnapshot
    , InstanceSnapshot
    , instanceSnapshot
    , insFromBlueprintId
    , insState
    , insResourceType
    , insFromAttachedDisks
    , insArn
    , insCreatedAt
    , insLocation
    , insProgress
    , insName
    , insFromBundleId
    , insSizeInGb
    , insSupportCode
    , insFromInstanceARN
    , insFromInstanceName
    , insTags

    -- ** InstanceSnapshotInfo
    , InstanceSnapshotInfo
    , instanceSnapshotInfo
    , isiFromBlueprintId
    , isiFromBundleId
    , isiFromDiskInfo

    -- ** InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- ** KeyPair
    , KeyPair
    , keyPair
    , kpResourceType
    , kpArn
    , kpCreatedAt
    , kpLocation
    , kpFingerprint
    , kpName
    , kpSupportCode
    , kpTags

    -- ** LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbHealthCheckPath
    , lbState
    , lbResourceType
    , lbArn
    , lbCreatedAt
    , lbLocation
    , lbInstancePort
    , lbConfigurationOptions
    , lbProtocol
    , lbTlsCertificateSummaries
    , lbName
    , lbSupportCode
    , lbPublicPorts
    , lbDnsName
    , lbInstanceHealthSummary
    , lbTags

    -- ** LoadBalancerTLSCertificate
    , LoadBalancerTLSCertificate
    , loadBalancerTLSCertificate
    , lbtcFailureReason
    , lbtcSubject
    , lbtcStatus
    , lbtcSubjectAlternativeNames
    , lbtcResourceType
    , lbtcArn
    , lbtcCreatedAt
    , lbtcLocation
    , lbtcLoadBalancerName
    , lbtcSerial
    , lbtcIsAttached
    , lbtcRevokedAt
    , lbtcNotBefore
    , lbtcRevocationReason
    , lbtcDomainName
    , lbtcName
    , lbtcRenewalSummary
    , lbtcSupportCode
    , lbtcDomainValidationRecords
    , lbtcIssuedAt
    , lbtcKeyAlgorithm
    , lbtcSignatureAlgorithm
    , lbtcIssuer
    , lbtcTags
    , lbtcNotAfter

    -- ** LoadBalancerTLSCertificateDomainValidationOption
    , LoadBalancerTLSCertificateDomainValidationOption
    , loadBalancerTLSCertificateDomainValidationOption
    , lbtcdvoDomainName
    , lbtcdvoValidationStatus

    -- ** LoadBalancerTLSCertificateDomainValidationRecord
    , LoadBalancerTLSCertificateDomainValidationRecord
    , loadBalancerTLSCertificateDomainValidationRecord
    , lbtcdvrValue
    , lbtcdvrDomainName
    , lbtcdvrName
    , lbtcdvrValidationStatus
    , lbtcdvrType

    -- ** LoadBalancerTLSCertificateRenewalSummary
    , LoadBalancerTLSCertificateRenewalSummary
    , loadBalancerTLSCertificateRenewalSummary
    , lbtcrsRenewalStatus
    , lbtcrsDomainValidationOptions

    -- ** LoadBalancerTLSCertificateSummary
    , LoadBalancerTLSCertificateSummary
    , loadBalancerTLSCertificateSummary
    , lbtcsIsAttached
    , lbtcsName

    -- ** LogEvent
    , LogEvent
    , logEvent
    , leCreatedAt
    , leMessage

    -- ** MetricDatapoint
    , MetricDatapoint
    , metricDatapoint
    , mdSampleCount
    , mdMaximum
    , mdAverage
    , mdMinimum
    , mdSum
    , mdTimestamp
    , mdUnit

    -- ** MonthlyTransfer
    , MonthlyTransfer
    , monthlyTransfer
    , mtGbPerMonthAllocated

    -- ** Operation
    , Operation
    , operation
    , oStatus
    , oOperationDetails
    , oResourceType
    , oCreatedAt
    , oResourceName
    , oLocation
    , oStatusChangedAt
    , oErrorDetails
    , oErrorCode
    , oId
    , oOperationType
    , oIsTerminal

    -- ** PasswordData
    , PasswordData
    , passwordData
    , pdKeyPairName
    , pdCiphertext

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAction
    , pmaDescription
    , pmaCurrentApplyDate

    -- ** PendingModifiedRelationalDatabaseValues
    , PendingModifiedRelationalDatabaseValues
    , pendingModifiedRelationalDatabaseValues
    , pmrdvEngineVersion
    , pmrdvMasterUserPassword
    , pmrdvBackupRetentionEnabled

    -- ** PortInfo
    , PortInfo
    , portInfo
    , piFromPort
    , piProtocol
    , piToPort

    -- ** RegionInfo
    , RegionInfo
    , regionInfo
    , riAvailabilityZones
    , riName
    , riRelationalDatabaseAvailabilityZones
    , riDisplayName
    , riContinentCode
    , riDescription

    -- ** RelationalDatabase
    , RelationalDatabase
    , relationalDatabase
    , rdEngineVersion
    , rdRelationalDatabaseBundleId
    , rdMasterEndpoint
    , rdState
    , rdResourceType
    , rdPubliclyAccessible
    , rdMasterUsername
    , rdArn
    , rdCreatedAt
    , rdLocation
    , rdEngine
    , rdLatestRestorableTime
    , rdPreferredMaintenanceWindow
    , rdRelationalDatabaseBlueprintId
    , rdName
    , rdBackupRetentionEnabled
    , rdPreferredBackupWindow
    , rdPendingMaintenanceActions
    , rdSupportCode
    , rdSecondaryAvailabilityZone
    , rdPendingModifiedValues
    , rdMasterDatabaseName
    , rdHardware
    , rdParameterApplyStatus
    , rdTags

    -- ** RelationalDatabaseBlueprint
    , RelationalDatabaseBlueprint
    , relationalDatabaseBlueprint
    , rdbEngineVersion
    , rdbIsEngineDefault
    , rdbEngineVersionDescription
    , rdbEngine
    , rdbBlueprintId
    , rdbEngineDescription

    -- ** RelationalDatabaseBundle
    , RelationalDatabaseBundle
    , relationalDatabaseBundle
    , rdbIsEncrypted
    , rdbCpuCount
    , rdbTransferPerMonthInGb
    , rdbBundleId
    , rdbName
    , rdbDiskSizeInGb
    , rdbPrice
    , rdbIsActive
    , rdbRamSizeInGb

    -- ** RelationalDatabaseEndpoint
    , RelationalDatabaseEndpoint
    , relationalDatabaseEndpoint
    , rdeAddress
    , rdePort

    -- ** RelationalDatabaseEvent
    , RelationalDatabaseEvent
    , relationalDatabaseEvent
    , rdeCreatedAt
    , rdeEventCategories
    , rdeResource
    , rdeMessage

    -- ** RelationalDatabaseHardware
    , RelationalDatabaseHardware
    , relationalDatabaseHardware
    , rdhCpuCount
    , rdhDiskSizeInGb
    , rdhRamSizeInGb

    -- ** RelationalDatabaseParameter
    , RelationalDatabaseParameter
    , relationalDatabaseParameter
    , rdpApplyType
    , rdpParameterValue
    , rdpApplyMethod
    , rdpDataType
    , rdpIsModifiable
    , rdpAllowedValues
    , rdpParameterName
    , rdpDescription

    -- ** RelationalDatabaseSnapshot
    , RelationalDatabaseSnapshot
    , relationalDatabaseSnapshot
    , rdsEngineVersion
    , rdsState
    , rdsFromRelationalDatabaseName
    , rdsResourceType
    , rdsFromRelationalDatabaseBlueprintId
    , rdsArn
    , rdsCreatedAt
    , rdsLocation
    , rdsEngine
    , rdsName
    , rdsSizeInGb
    , rdsSupportCode
    , rdsFromRelationalDatabaseARN
    , rdsFromRelationalDatabaseBundleId
    , rdsTags

    -- ** ResourceLocation
    , ResourceLocation
    , resourceLocation
    , rlRegionName
    , rlAvailabilityZone

    -- ** StaticIP
    , StaticIP
    , staticIP
    , siIpAddress
    , siResourceType
    , siArn
    , siCreatedAt
    , siLocation
    , siIsAttached
    , siName
    , siSupportCode
    , siAttachedTo

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lightsail.AllocateStaticIP
import Network.AWS.Lightsail.AttachDisk
import Network.AWS.Lightsail.AttachInstancesToLoadBalancer
import Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
import Network.AWS.Lightsail.AttachStaticIP
import Network.AWS.Lightsail.CloseInstancePublicPorts
import Network.AWS.Lightsail.CopySnapshot
import Network.AWS.Lightsail.CreateCloudFormationStack
import Network.AWS.Lightsail.CreateDisk
import Network.AWS.Lightsail.CreateDiskFromSnapshot
import Network.AWS.Lightsail.CreateDiskSnapshot
import Network.AWS.Lightsail.CreateDomain
import Network.AWS.Lightsail.CreateDomainEntry
import Network.AWS.Lightsail.CreateInstances
import Network.AWS.Lightsail.CreateInstancesFromSnapshot
import Network.AWS.Lightsail.CreateInstanceSnapshot
import Network.AWS.Lightsail.CreateKeyPair
import Network.AWS.Lightsail.CreateLoadBalancer
import Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
import Network.AWS.Lightsail.CreateRelationalDatabase
import Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
import Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DeleteDisk
import Network.AWS.Lightsail.DeleteDiskSnapshot
import Network.AWS.Lightsail.DeleteDomain
import Network.AWS.Lightsail.DeleteDomainEntry
import Network.AWS.Lightsail.DeleteInstance
import Network.AWS.Lightsail.DeleteInstanceSnapshot
import Network.AWS.Lightsail.DeleteKeyPair
import Network.AWS.Lightsail.DeleteKnownHostKeys
import Network.AWS.Lightsail.DeleteLoadBalancer
import Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
import Network.AWS.Lightsail.DeleteRelationalDatabase
import Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DetachDisk
import Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
import Network.AWS.Lightsail.DetachStaticIP
import Network.AWS.Lightsail.DownloadDefaultKeyPair
import Network.AWS.Lightsail.ExportSnapshot
import Network.AWS.Lightsail.GetActiveNames
import Network.AWS.Lightsail.GetBlueprints
import Network.AWS.Lightsail.GetBundles
import Network.AWS.Lightsail.GetCloudFormationStackRecords
import Network.AWS.Lightsail.GetDisk
import Network.AWS.Lightsail.GetDisks
import Network.AWS.Lightsail.GetDiskSnapshot
import Network.AWS.Lightsail.GetDiskSnapshots
import Network.AWS.Lightsail.GetDomain
import Network.AWS.Lightsail.GetDomains
import Network.AWS.Lightsail.GetExportSnapshotRecords
import Network.AWS.Lightsail.GetInstance
import Network.AWS.Lightsail.GetInstanceAccessDetails
import Network.AWS.Lightsail.GetInstanceMetricData
import Network.AWS.Lightsail.GetInstancePortStates
import Network.AWS.Lightsail.GetInstances
import Network.AWS.Lightsail.GetInstanceSnapshot
import Network.AWS.Lightsail.GetInstanceSnapshots
import Network.AWS.Lightsail.GetInstanceState
import Network.AWS.Lightsail.GetKeyPair
import Network.AWS.Lightsail.GetKeyPairs
import Network.AWS.Lightsail.GetLoadBalancer
import Network.AWS.Lightsail.GetLoadBalancerMetricData
import Network.AWS.Lightsail.GetLoadBalancers
import Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
import Network.AWS.Lightsail.GetOperation
import Network.AWS.Lightsail.GetOperations
import Network.AWS.Lightsail.GetOperationsForResource
import Network.AWS.Lightsail.GetRegions
import Network.AWS.Lightsail.GetRelationalDatabase
import Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
import Network.AWS.Lightsail.GetRelationalDatabaseBundles
import Network.AWS.Lightsail.GetRelationalDatabaseEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
import Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
import Network.AWS.Lightsail.GetRelationalDatabaseMetricData
import Network.AWS.Lightsail.GetRelationalDatabaseParameters
import Network.AWS.Lightsail.GetRelationalDatabases
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
import Network.AWS.Lightsail.GetStaticIP
import Network.AWS.Lightsail.GetStaticIPs
import Network.AWS.Lightsail.ImportKeyPair
import Network.AWS.Lightsail.IsVPCPeered
import Network.AWS.Lightsail.OpenInstancePublicPorts
import Network.AWS.Lightsail.PeerVPC
import Network.AWS.Lightsail.PutInstancePublicPorts
import Network.AWS.Lightsail.RebootInstance
import Network.AWS.Lightsail.RebootRelationalDatabase
import Network.AWS.Lightsail.ReleaseStaticIP
import Network.AWS.Lightsail.StartInstance
import Network.AWS.Lightsail.StartRelationalDatabase
import Network.AWS.Lightsail.StopInstance
import Network.AWS.Lightsail.StopRelationalDatabase
import Network.AWS.Lightsail.TagResource
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.UnpeerVPC
import Network.AWS.Lightsail.UntagResource
import Network.AWS.Lightsail.UpdateDomainEntry
import Network.AWS.Lightsail.UpdateLoadBalancerAttribute
import Network.AWS.Lightsail.UpdateRelationalDatabase
import Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
import Network.AWS.Lightsail.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Lightsail'.
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
