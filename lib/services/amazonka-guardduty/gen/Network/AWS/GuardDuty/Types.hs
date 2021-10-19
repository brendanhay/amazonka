{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerErrorException,
    _BadRequestException,

    -- * AdminStatus
    AdminStatus (..),

    -- * DataSource
    DataSource (..),

    -- * DataSourceStatus
    DataSourceStatus (..),

    -- * DestinationType
    DestinationType (..),

    -- * DetectorStatus
    DetectorStatus (..),

    -- * Feedback
    Feedback (..),

    -- * FilterAction
    FilterAction (..),

    -- * FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- * FindingStatisticType
    FindingStatisticType (..),

    -- * IpSetFormat
    IpSetFormat (..),

    -- * IpSetStatus
    IpSetStatus (..),

    -- * OrderBy
    OrderBy (..),

    -- * PublishingStatus
    PublishingStatus (..),

    -- * ThreatIntelSetFormat
    ThreatIntelSetFormat (..),

    -- * ThreatIntelSetStatus
    ThreatIntelSetStatus (..),

    -- * UsageStatisticType
    UsageStatisticType (..),

    -- * AccessControlList
    AccessControlList (..),
    newAccessControlList,
    accessControlList_allowsPublicWriteAccess,
    accessControlList_allowsPublicReadAccess,

    -- * AccessKeyDetails
    AccessKeyDetails (..),
    newAccessKeyDetails,
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_accessKeyId,
    accessKeyDetails_userType,

    -- * AccountDetail
    AccountDetail (..),
    newAccountDetail,
    accountDetail_accountId,
    accountDetail_email,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    newAccountLevelPermissions,
    accountLevelPermissions_blockPublicAccess,

    -- * Action
    Action (..),
    newAction,
    action_networkConnectionAction,
    action_portProbeAction,
    action_actionType,
    action_dnsRequestAction,
    action_awsApiCallAction,

    -- * AdminAccount
    AdminAccount (..),
    newAdminAccount,
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    newAwsApiCallAction,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_serviceName,
    awsApiCallAction_errorCode,
    awsApiCallAction_api,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_blockPublicPolicy,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    newBucketLevelPermissions,
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- * BucketPolicy
    BucketPolicy (..),
    newBucketPolicy,
    bucketPolicy_allowsPublicWriteAccess,
    bucketPolicy_allowsPublicReadAccess,

    -- * City
    City (..),
    newCity,
    city_cityName,

    -- * CloudTrailConfigurationResult
    CloudTrailConfigurationResult (..),
    newCloudTrailConfigurationResult,
    cloudTrailConfigurationResult_status,

    -- * Condition
    Condition (..),
    newCondition,
    condition_eq,
    condition_lessThan,
    condition_lte,
    condition_greaterThanOrEqual,
    condition_lessThanOrEqual,
    condition_gt,
    condition_equals,
    condition_neq,
    condition_notEquals,
    condition_lt,
    condition_gte,
    condition_greaterThan,

    -- * Country
    Country (..),
    newCountry,
    country_countryName,
    country_countryCode,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    newDNSLogsConfigurationResult,
    dNSLogsConfigurationResult_status,

    -- * DataSourceConfigurations
    DataSourceConfigurations (..),
    newDataSourceConfigurations,
    dataSourceConfigurations_s3Logs,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    newDataSourceConfigurationsResult,
    dataSourceConfigurationsResult_cloudTrail,
    dataSourceConfigurationsResult_dNSLogs,
    dataSourceConfigurationsResult_flowLogs,
    dataSourceConfigurationsResult_s3Logs,

    -- * DefaultServerSideEncryption
    DefaultServerSideEncryption (..),
    newDefaultServerSideEncryption,
    defaultServerSideEncryption_encryptionType,
    defaultServerSideEncryption_kmsMasterKeyArn,

    -- * Destination
    Destination (..),
    newDestination,
    destination_destinationId,
    destination_destinationType,
    destination_status,

    -- * DestinationProperties
    DestinationProperties (..),
    newDestinationProperties,
    destinationProperties_kmsKeyArn,
    destinationProperties_destinationArn,

    -- * DnsRequestAction
    DnsRequestAction (..),
    newDnsRequestAction,
    dnsRequestAction_domain,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domain,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_threatIntelligenceDetails,

    -- * Finding
    Finding (..),
    newFinding,
    finding_service,
    finding_confidence,
    finding_partition,
    finding_title,
    finding_description,
    finding_accountId,
    finding_arn,
    finding_createdAt,
    finding_id,
    finding_region,
    finding_resource,
    finding_schemaVersion,
    finding_severity,
    finding_type,
    finding_updatedAt,

    -- * FindingCriteria
    FindingCriteria (..),
    newFindingCriteria,
    findingCriteria_criterion,

    -- * FindingStatistics
    FindingStatistics (..),
    newFindingStatistics,
    findingStatistics_countBySeverity,

    -- * FlowLogsConfigurationResult
    FlowLogsConfigurationResult (..),
    newFlowLogsConfigurationResult,
    flowLogsConfigurationResult_status,

    -- * GeoLocation
    GeoLocation (..),
    newGeoLocation,
    geoLocation_lat,
    geoLocation_lon,

    -- * IamInstanceProfile
    IamInstanceProfile (..),
    newIamInstanceProfile,
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- * InstanceDetails
    InstanceDetails (..),
    newInstanceDetails,
    instanceDetails_instanceId,
    instanceDetails_platform,
    instanceDetails_launchTime,
    instanceDetails_networkInterfaces,
    instanceDetails_outpostArn,
    instanceDetails_instanceType,
    instanceDetails_availabilityZone,
    instanceDetails_iamInstanceProfile,
    instanceDetails_imageId,
    instanceDetails_productCodes,
    instanceDetails_instanceState,
    instanceDetails_tags,
    instanceDetails_imageDescription,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,
    invitation_accountId,

    -- * LocalIpDetails
    LocalIpDetails (..),
    newLocalIpDetails,
    localIpDetails_ipAddressV4,

    -- * LocalPortDetails
    LocalPortDetails (..),
    newLocalPortDetails,
    localPortDetails_portName,
    localPortDetails_port,

    -- * Master
    Master (..),
    newMaster,
    master_invitedAt,
    master_relationshipStatus,
    master_invitationId,
    master_accountId,

    -- * Member
    Member (..),
    newMember,
    member_invitedAt,
    member_detectorId,
    member_accountId,
    member_masterId,
    member_email,
    member_relationshipStatus,
    member_updatedAt,

    -- * MemberDataSourceConfiguration
    MemberDataSourceConfiguration (..),
    newMemberDataSourceConfiguration,
    memberDataSourceConfiguration_accountId,
    memberDataSourceConfiguration_dataSources,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    newNetworkConnectionAction,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_remotePortDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localPortDetails,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_privateIpAddresses,
    networkInterface_publicDnsName,
    networkInterface_securityGroups,
    networkInterface_vpcId,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_privateIpAddress,
    networkInterface_publicIp,
    networkInterface_privateDnsName,
    networkInterface_ipv6Addresses,

    -- * Organization
    Organization (..),
    newOrganization,
    organization_org,
    organization_asnOrg,
    organization_asn,
    organization_isp,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    newOrganizationDataSourceConfigurations,
    organizationDataSourceConfigurations_s3Logs,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    newOrganizationDataSourceConfigurationsResult,
    organizationDataSourceConfigurationsResult_s3Logs,

    -- * OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (..),
    newOrganizationS3LogsConfiguration,
    organizationS3LogsConfiguration_autoEnable,

    -- * OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (..),
    newOrganizationS3LogsConfigurationResult,
    organizationS3LogsConfigurationResult_autoEnable,

    -- * Owner
    Owner (..),
    newOwner,
    owner_id,

    -- * PermissionConfiguration
    PermissionConfiguration (..),
    newPermissionConfiguration,
    permissionConfiguration_bucketLevelPermissions,
    permissionConfiguration_accountLevelPermissions,

    -- * PortProbeAction
    PortProbeAction (..),
    newPortProbeAction,
    portProbeAction_portProbeDetails,
    portProbeAction_blocked,

    -- * PortProbeDetail
    PortProbeDetail (..),
    newPortProbeDetail,
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,

    -- * PrivateIpAddressDetails
    PrivateIpAddressDetails (..),
    newPrivateIpAddressDetails,
    privateIpAddressDetails_privateIpAddress,
    privateIpAddressDetails_privateDnsName,

    -- * ProductCode
    ProductCode (..),
    newProductCode,
    productCode_productType,
    productCode_code,

    -- * PublicAccess
    PublicAccess (..),
    newPublicAccess,
    publicAccess_permissionConfiguration,
    publicAccess_effectivePermission,

    -- * RemoteIpDetails
    RemoteIpDetails (..),
    newRemoteIpDetails,
    remoteIpDetails_country,
    remoteIpDetails_city,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_geoLocation,
    remoteIpDetails_organization,

    -- * RemotePortDetails
    RemotePortDetails (..),
    newRemotePortDetails,
    remotePortDetails_portName,
    remotePortDetails_port,

    -- * Resource
    Resource (..),
    newResource,
    resource_resourceType,
    resource_s3BucketDetails,
    resource_instanceDetails,
    resource_accessKeyDetails,

    -- * S3BucketDetail
    S3BucketDetail (..),
    newS3BucketDetail,
    s3BucketDetail_arn,
    s3BucketDetail_createdAt,
    s3BucketDetail_owner,
    s3BucketDetail_name,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_publicAccess,
    s3BucketDetail_type,
    s3BucketDetail_tags,

    -- * S3LogsConfiguration
    S3LogsConfiguration (..),
    newS3LogsConfiguration,
    s3LogsConfiguration_enable,

    -- * S3LogsConfigurationResult
    S3LogsConfigurationResult (..),
    newS3LogsConfigurationResult,
    s3LogsConfigurationResult_status,

    -- * SecurityGroup
    SecurityGroup (..),
    newSecurityGroup,
    securityGroup_groupId,
    securityGroup_groupName,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
    serviceInfo_count,
    serviceInfo_eventFirstSeen,
    serviceInfo_action,
    serviceInfo_detectorId,
    serviceInfo_serviceName,
    serviceInfo_userFeedback,
    serviceInfo_evidence,
    serviceInfo_eventLastSeen,
    serviceInfo_resourceRole,
    serviceInfo_archived,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * ThreatIntelligenceDetail
    ThreatIntelligenceDetail (..),
    newThreatIntelligenceDetail,
    threatIntelligenceDetail_threatNames,
    threatIntelligenceDetail_threatListName,

    -- * Total
    Total (..),
    newTotal,
    total_amount,
    total_unit,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_result,

    -- * UsageAccountResult
    UsageAccountResult (..),
    newUsageAccountResult,
    usageAccountResult_accountId,
    usageAccountResult_total,

    -- * UsageCriteria
    UsageCriteria (..),
    newUsageCriteria,
    usageCriteria_accountIds,
    usageCriteria_resources,
    usageCriteria_dataSources,

    -- * UsageDataSourceResult
    UsageDataSourceResult (..),
    newUsageDataSourceResult,
    usageDataSourceResult_total,
    usageDataSourceResult_dataSource,

    -- * UsageResourceResult
    UsageResourceResult (..),
    newUsageResourceResult,
    usageResourceResult_total,
    usageResourceResult_resource,

    -- * UsageStatistics
    UsageStatistics (..),
    newUsageStatistics,
    usageStatistics_topResources,
    usageStatistics_sumByResource,
    usageStatistics_sumByDataSource,
    usageStatistics_sumByAccount,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.AccountDetail
import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.AdminAccount
import Network.AWS.GuardDuty.Types.AdminStatus
import Network.AWS.GuardDuty.Types.AwsApiCallAction
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import Network.AWS.GuardDuty.Types.BucketPolicy
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.DataSourceConfigurations
import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Destination
import Network.AWS.GuardDuty.Types.DestinationProperties
import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.DetectorStatus
import Network.AWS.GuardDuty.Types.DnsRequestAction
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.GuardDuty.Types.Feedback
import Network.AWS.GuardDuty.Types.FilterAction
import Network.AWS.GuardDuty.Types.Finding
import Network.AWS.GuardDuty.Types.FindingCriteria
import Network.AWS.GuardDuty.Types.FindingPublishingFrequency
import Network.AWS.GuardDuty.Types.FindingStatisticType
import Network.AWS.GuardDuty.Types.FindingStatistics
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.IamInstanceProfile
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.Invitation
import Network.AWS.GuardDuty.Types.IpSetFormat
import Network.AWS.GuardDuty.Types.IpSetStatus
import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.Master
import Network.AWS.GuardDuty.Types.Member
import Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.OrderBy
import Network.AWS.GuardDuty.Types.Organization
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.Owner
import Network.AWS.GuardDuty.Types.PermissionConfiguration
import Network.AWS.GuardDuty.Types.PortProbeAction
import Network.AWS.GuardDuty.Types.PortProbeDetail
import Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.PublishingStatus
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.S3BucketDetail
import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.GuardDuty.Types.SortCriteria
import Network.AWS.GuardDuty.Types.Tag
import Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
import Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.GuardDuty.Types.Total
import Network.AWS.GuardDuty.Types.UnprocessedAccount
import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageCriteria
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import Network.AWS.GuardDuty.Types.UsageStatisticType
import Network.AWS.GuardDuty.Types.UsageStatistics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "GuardDuty",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "guardduty",
      Core._serviceSigningName = "guardduty",
      Core._serviceVersion = "2017-11-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "GuardDuty",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An internal server error exception object.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | A bad request exception object.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
