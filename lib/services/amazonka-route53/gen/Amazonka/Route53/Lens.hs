{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Lens
  ( -- * Operations

    -- ** GetHostedZoneLimit
    getHostedZoneLimit_type,
    getHostedZoneLimit_hostedZoneId,
    getHostedZoneLimitResponse_httpStatus,
    getHostedZoneLimitResponse_limit,
    getHostedZoneLimitResponse_count,

    -- ** AssociateVPCWithHostedZone
    associateVPCWithHostedZone_comment,
    associateVPCWithHostedZone_hostedZoneId,
    associateVPCWithHostedZone_vpc,
    associateVPCWithHostedZoneResponse_httpStatus,
    associateVPCWithHostedZoneResponse_changeInfo,

    -- ** DeleteTrafficPolicy
    deleteTrafficPolicy_id,
    deleteTrafficPolicy_version,
    deleteTrafficPolicyResponse_httpStatus,

    -- ** DisableHostedZoneDNSSEC
    disableHostedZoneDNSSEC_hostedZoneId,
    disableHostedZoneDNSSECResponse_httpStatus,
    disableHostedZoneDNSSECResponse_changeInfo,

    -- ** CreateKeySigningKey
    createKeySigningKey_callerReference,
    createKeySigningKey_hostedZoneId,
    createKeySigningKey_keyManagementServiceArn,
    createKeySigningKey_name,
    createKeySigningKey_status,
    createKeySigningKeyResponse_httpStatus,
    createKeySigningKeyResponse_changeInfo,
    createKeySigningKeyResponse_keySigningKey,
    createKeySigningKeyResponse_location,

    -- ** GetCheckerIpRanges
    getCheckerIpRangesResponse_httpStatus,
    getCheckerIpRangesResponse_checkerIpRanges,

    -- ** GetTrafficPolicyInstance
    getTrafficPolicyInstance_id,
    getTrafficPolicyInstanceResponse_httpStatus,
    getTrafficPolicyInstanceResponse_trafficPolicyInstance,

    -- ** GetHealthCheckLastFailureReason
    getHealthCheckLastFailureReason_healthCheckId,
    getHealthCheckLastFailureReasonResponse_httpStatus,
    getHealthCheckLastFailureReasonResponse_healthCheckObservations,

    -- ** DeleteReusableDelegationSet
    deleteReusableDelegationSet_id,
    deleteReusableDelegationSetResponse_httpStatus,

    -- ** ListHostedZonesByName
    listHostedZonesByName_hostedZoneId,
    listHostedZonesByName_maxItems,
    listHostedZonesByName_dNSName,
    listHostedZonesByNameResponse_hostedZoneId,
    listHostedZonesByNameResponse_nextHostedZoneId,
    listHostedZonesByNameResponse_dNSName,
    listHostedZonesByNameResponse_nextDNSName,
    listHostedZonesByNameResponse_httpStatus,
    listHostedZonesByNameResponse_hostedZones,
    listHostedZonesByNameResponse_isTruncated,
    listHostedZonesByNameResponse_maxItems,

    -- ** ActivateKeySigningKey
    activateKeySigningKey_hostedZoneId,
    activateKeySigningKey_name,
    activateKeySigningKeyResponse_httpStatus,
    activateKeySigningKeyResponse_changeInfo,

    -- ** ListReusableDelegationSets
    listReusableDelegationSets_marker,
    listReusableDelegationSets_maxItems,
    listReusableDelegationSetsResponse_nextMarker,
    listReusableDelegationSetsResponse_httpStatus,
    listReusableDelegationSetsResponse_delegationSets,
    listReusableDelegationSetsResponse_marker,
    listReusableDelegationSetsResponse_isTruncated,
    listReusableDelegationSetsResponse_maxItems,

    -- ** ListTagsForResource
    listTagsForResource_resourceType,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_resourceTagSet,

    -- ** ListQueryLoggingConfigs
    listQueryLoggingConfigs_hostedZoneId,
    listQueryLoggingConfigs_nextToken,
    listQueryLoggingConfigs_maxResults,
    listQueryLoggingConfigsResponse_nextToken,
    listQueryLoggingConfigsResponse_httpStatus,
    listQueryLoggingConfigsResponse_queryLoggingConfigs,

    -- ** ListTrafficPolicyInstances
    listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstances_maxItems,
    listTrafficPolicyInstances_hostedZoneIdMarker,
    listTrafficPolicyInstances_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesResponse_httpStatus,
    listTrafficPolicyInstancesResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesResponse_isTruncated,
    listTrafficPolicyInstancesResponse_maxItems,

    -- ** CreateTrafficPolicyInstance
    createTrafficPolicyInstance_hostedZoneId,
    createTrafficPolicyInstance_name,
    createTrafficPolicyInstance_ttl,
    createTrafficPolicyInstance_trafficPolicyId,
    createTrafficPolicyInstance_trafficPolicyVersion,
    createTrafficPolicyInstanceResponse_httpStatus,
    createTrafficPolicyInstanceResponse_trafficPolicyInstance,
    createTrafficPolicyInstanceResponse_location,

    -- ** GetChange
    getChange_id,
    getChangeResponse_httpStatus,
    getChangeResponse_changeInfo,

    -- ** ChangeResourceRecordSets
    changeResourceRecordSets_hostedZoneId,
    changeResourceRecordSets_changeBatch,
    changeResourceRecordSetsResponse_httpStatus,
    changeResourceRecordSetsResponse_changeInfo,

    -- ** DeleteHealthCheck
    deleteHealthCheck_healthCheckId,
    deleteHealthCheckResponse_httpStatus,

    -- ** UpdateHealthCheck
    updateHealthCheck_failureThreshold,
    updateHealthCheck_iPAddress,
    updateHealthCheck_enableSNI,
    updateHealthCheck_disabled,
    updateHealthCheck_resetElements,
    updateHealthCheck_searchString,
    updateHealthCheck_healthThreshold,
    updateHealthCheck_regions,
    updateHealthCheck_resourcePath,
    updateHealthCheck_insufficientDataHealthStatus,
    updateHealthCheck_healthCheckVersion,
    updateHealthCheck_alarmIdentifier,
    updateHealthCheck_inverted,
    updateHealthCheck_fullyQualifiedDomainName,
    updateHealthCheck_childHealthChecks,
    updateHealthCheck_port,
    updateHealthCheck_healthCheckId,
    updateHealthCheckResponse_httpStatus,
    updateHealthCheckResponse_healthCheck,

    -- ** CreateHostedZone
    createHostedZone_delegationSetId,
    createHostedZone_vpc,
    createHostedZone_hostedZoneConfig,
    createHostedZone_name,
    createHostedZone_callerReference,
    createHostedZoneResponse_vpc,
    createHostedZoneResponse_httpStatus,
    createHostedZoneResponse_hostedZone,
    createHostedZoneResponse_changeInfo,
    createHostedZoneResponse_delegationSet,
    createHostedZoneResponse_location,

    -- ** CreateVPCAssociationAuthorization
    createVPCAssociationAuthorization_hostedZoneId,
    createVPCAssociationAuthorization_vpc,
    createVPCAssociationAuthorizationResponse_httpStatus,
    createVPCAssociationAuthorizationResponse_hostedZoneId,
    createVPCAssociationAuthorizationResponse_vpc,

    -- ** ListVPCAssociationAuthorizations
    listVPCAssociationAuthorizations_nextToken,
    listVPCAssociationAuthorizations_maxResults,
    listVPCAssociationAuthorizations_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_nextToken,
    listVPCAssociationAuthorizationsResponse_httpStatus,
    listVPCAssociationAuthorizationsResponse_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_vPCs,

    -- ** ListTrafficPolicyInstancesByPolicy
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicy_maxItems,
    listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyId,
    listTrafficPolicyInstancesByPolicy_trafficPolicyVersion,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicyResponse_httpStatus,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByPolicyResponse_isTruncated,
    listTrafficPolicyInstancesByPolicyResponse_maxItems,

    -- ** DisassociateVPCFromHostedZone
    disassociateVPCFromHostedZone_comment,
    disassociateVPCFromHostedZone_hostedZoneId,
    disassociateVPCFromHostedZone_vpc,
    disassociateVPCFromHostedZoneResponse_httpStatus,
    disassociateVPCFromHostedZoneResponse_changeInfo,

    -- ** CreateHealthCheck
    createHealthCheck_callerReference,
    createHealthCheck_healthCheckConfig,
    createHealthCheckResponse_httpStatus,
    createHealthCheckResponse_healthCheck,
    createHealthCheckResponse_location,

    -- ** DeleteVPCAssociationAuthorization
    deleteVPCAssociationAuthorization_hostedZoneId,
    deleteVPCAssociationAuthorization_vpc,
    deleteVPCAssociationAuthorizationResponse_httpStatus,

    -- ** ChangeTagsForResource
    changeTagsForResource_removeTagKeys,
    changeTagsForResource_addTags,
    changeTagsForResource_resourceType,
    changeTagsForResource_resourceId,
    changeTagsForResourceResponse_httpStatus,

    -- ** ListHostedZones
    listHostedZones_delegationSetId,
    listHostedZones_marker,
    listHostedZones_maxItems,
    listHostedZonesResponse_marker,
    listHostedZonesResponse_nextMarker,
    listHostedZonesResponse_httpStatus,
    listHostedZonesResponse_hostedZones,
    listHostedZonesResponse_isTruncated,
    listHostedZonesResponse_maxItems,

    -- ** GetTrafficPolicyInstanceCount
    getTrafficPolicyInstanceCountResponse_httpStatus,
    getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount,

    -- ** ListGeoLocations
    listGeoLocations_startSubdivisionCode,
    listGeoLocations_maxItems,
    listGeoLocations_startCountryCode,
    listGeoLocations_startContinentCode,
    listGeoLocationsResponse_nextContinentCode,
    listGeoLocationsResponse_nextCountryCode,
    listGeoLocationsResponse_nextSubdivisionCode,
    listGeoLocationsResponse_httpStatus,
    listGeoLocationsResponse_geoLocationDetailsList,
    listGeoLocationsResponse_isTruncated,
    listGeoLocationsResponse_maxItems,

    -- ** GetHostedZone
    getHostedZone_id,
    getHostedZoneResponse_vPCs,
    getHostedZoneResponse_delegationSet,
    getHostedZoneResponse_httpStatus,
    getHostedZoneResponse_hostedZone,

    -- ** GetHealthCheck
    getHealthCheck_healthCheckId,
    getHealthCheckResponse_httpStatus,
    getHealthCheckResponse_healthCheck,

    -- ** ListResourceRecordSets
    listResourceRecordSets_startRecordName,
    listResourceRecordSets_startRecordType,
    listResourceRecordSets_startRecordIdentifier,
    listResourceRecordSets_maxItems,
    listResourceRecordSets_hostedZoneId,
    listResourceRecordSetsResponse_nextRecordType,
    listResourceRecordSetsResponse_nextRecordName,
    listResourceRecordSetsResponse_nextRecordIdentifier,
    listResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_resourceRecordSets,
    listResourceRecordSetsResponse_isTruncated,
    listResourceRecordSetsResponse_maxItems,

    -- ** CreateReusableDelegationSet
    createReusableDelegationSet_hostedZoneId,
    createReusableDelegationSet_callerReference,
    createReusableDelegationSetResponse_httpStatus,
    createReusableDelegationSetResponse_delegationSet,
    createReusableDelegationSetResponse_location,

    -- ** CreateQueryLoggingConfig
    createQueryLoggingConfig_hostedZoneId,
    createQueryLoggingConfig_cloudWatchLogsLogGroupArn,
    createQueryLoggingConfigResponse_httpStatus,
    createQueryLoggingConfigResponse_queryLoggingConfig,
    createQueryLoggingConfigResponse_location,

    -- ** GetHealthCheckCount
    getHealthCheckCountResponse_httpStatus,
    getHealthCheckCountResponse_healthCheckCount,

    -- ** UpdateTrafficPolicyComment
    updateTrafficPolicyComment_id,
    updateTrafficPolicyComment_version,
    updateTrafficPolicyComment_comment,
    updateTrafficPolicyCommentResponse_httpStatus,
    updateTrafficPolicyCommentResponse_trafficPolicy,

    -- ** GetHostedZoneCount
    getHostedZoneCountResponse_httpStatus,
    getHostedZoneCountResponse_hostedZoneCount,

    -- ** DeleteKeySigningKey
    deleteKeySigningKey_hostedZoneId,
    deleteKeySigningKey_name,
    deleteKeySigningKeyResponse_httpStatus,
    deleteKeySigningKeyResponse_changeInfo,

    -- ** GetDNSSEC
    getDNSSEC_hostedZoneId,
    getDNSSECResponse_httpStatus,
    getDNSSECResponse_status,
    getDNSSECResponse_keySigningKeys,

    -- ** GetAccountLimit
    getAccountLimit_type,
    getAccountLimitResponse_httpStatus,
    getAccountLimitResponse_limit,
    getAccountLimitResponse_count,

    -- ** EnableHostedZoneDNSSEC
    enableHostedZoneDNSSEC_hostedZoneId,
    enableHostedZoneDNSSECResponse_httpStatus,
    enableHostedZoneDNSSECResponse_changeInfo,

    -- ** DeleteQueryLoggingConfig
    deleteQueryLoggingConfig_id,
    deleteQueryLoggingConfigResponse_httpStatus,

    -- ** GetQueryLoggingConfig
    getQueryLoggingConfig_id,
    getQueryLoggingConfigResponse_httpStatus,
    getQueryLoggingConfigResponse_queryLoggingConfig,

    -- ** GetReusableDelegationSet
    getReusableDelegationSet_id,
    getReusableDelegationSetResponse_httpStatus,
    getReusableDelegationSetResponse_delegationSet,

    -- ** DeleteTrafficPolicyInstance
    deleteTrafficPolicyInstance_id,
    deleteTrafficPolicyInstanceResponse_httpStatus,

    -- ** UpdateTrafficPolicyInstance
    updateTrafficPolicyInstance_id,
    updateTrafficPolicyInstance_ttl,
    updateTrafficPolicyInstance_trafficPolicyId,
    updateTrafficPolicyInstance_trafficPolicyVersion,
    updateTrafficPolicyInstanceResponse_httpStatus,
    updateTrafficPolicyInstanceResponse_trafficPolicyInstance,

    -- ** UpdateHostedZoneComment
    updateHostedZoneComment_comment,
    updateHostedZoneComment_id,
    updateHostedZoneCommentResponse_httpStatus,
    updateHostedZoneCommentResponse_hostedZone,

    -- ** GetHealthCheckStatus
    getHealthCheckStatus_healthCheckId,
    getHealthCheckStatusResponse_httpStatus,
    getHealthCheckStatusResponse_healthCheckObservations,

    -- ** ListHostedZonesByVPC
    listHostedZonesByVPC_nextToken,
    listHostedZonesByVPC_maxItems,
    listHostedZonesByVPC_vPCId,
    listHostedZonesByVPC_vPCRegion,
    listHostedZonesByVPCResponse_nextToken,
    listHostedZonesByVPCResponse_httpStatus,
    listHostedZonesByVPCResponse_hostedZoneSummaries,
    listHostedZonesByVPCResponse_maxItems,

    -- ** GetReusableDelegationSetLimit
    getReusableDelegationSetLimit_type,
    getReusableDelegationSetLimit_delegationSetId,
    getReusableDelegationSetLimitResponse_httpStatus,
    getReusableDelegationSetLimitResponse_limit,
    getReusableDelegationSetLimitResponse_count,

    -- ** CreateTrafficPolicyVersion
    createTrafficPolicyVersion_comment,
    createTrafficPolicyVersion_id,
    createTrafficPolicyVersion_document,
    createTrafficPolicyVersionResponse_httpStatus,
    createTrafficPolicyVersionResponse_trafficPolicy,
    createTrafficPolicyVersionResponse_location,

    -- ** DeactivateKeySigningKey
    deactivateKeySigningKey_hostedZoneId,
    deactivateKeySigningKey_name,
    deactivateKeySigningKeyResponse_httpStatus,
    deactivateKeySigningKeyResponse_changeInfo,

    -- ** TestDNSAnswer
    testDNSAnswer_resolverIP,
    testDNSAnswer_eDNS0ClientSubnetIP,
    testDNSAnswer_eDNS0ClientSubnetMask,
    testDNSAnswer_hostedZoneId,
    testDNSAnswer_recordName,
    testDNSAnswer_recordType,
    testDNSAnswerResponse_httpStatus,
    testDNSAnswerResponse_nameserver,
    testDNSAnswerResponse_recordName,
    testDNSAnswerResponse_recordType,
    testDNSAnswerResponse_recordData,
    testDNSAnswerResponse_responseCode,
    testDNSAnswerResponse_protocol,

    -- ** ListHealthChecks
    listHealthChecks_marker,
    listHealthChecks_maxItems,
    listHealthChecksResponse_nextMarker,
    listHealthChecksResponse_httpStatus,
    listHealthChecksResponse_healthChecks,
    listHealthChecksResponse_marker,
    listHealthChecksResponse_isTruncated,
    listHealthChecksResponse_maxItems,

    -- ** GetTrafficPolicy
    getTrafficPolicy_id,
    getTrafficPolicy_version,
    getTrafficPolicyResponse_httpStatus,
    getTrafficPolicyResponse_trafficPolicy,

    -- ** ListTrafficPolicyVersions
    listTrafficPolicyVersions_maxItems,
    listTrafficPolicyVersions_trafficPolicyVersionMarker,
    listTrafficPolicyVersions_id,
    listTrafficPolicyVersionsResponse_httpStatus,
    listTrafficPolicyVersionsResponse_trafficPolicies,
    listTrafficPolicyVersionsResponse_isTruncated,
    listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker,
    listTrafficPolicyVersionsResponse_maxItems,

    -- ** DeleteHostedZone
    deleteHostedZone_id,
    deleteHostedZoneResponse_httpStatus,
    deleteHostedZoneResponse_changeInfo,

    -- ** GetGeoLocation
    getGeoLocation_subdivisionCode,
    getGeoLocation_countryCode,
    getGeoLocation_continentCode,
    getGeoLocationResponse_httpStatus,
    getGeoLocationResponse_geoLocationDetails,

    -- ** ListTagsForResources
    listTagsForResources_resourceType,
    listTagsForResources_resourceIds,
    listTagsForResourcesResponse_httpStatus,
    listTagsForResourcesResponse_resourceTagSets,

    -- ** CreateTrafficPolicy
    createTrafficPolicy_comment,
    createTrafficPolicy_name,
    createTrafficPolicy_document,
    createTrafficPolicyResponse_httpStatus,
    createTrafficPolicyResponse_trafficPolicy,
    createTrafficPolicyResponse_location,

    -- ** ListTrafficPolicyInstancesByHostedZone
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZone_maxItems,
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZone_hostedZoneId,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_httpStatus,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByHostedZoneResponse_isTruncated,
    listTrafficPolicyInstancesByHostedZoneResponse_maxItems,

    -- ** ListTrafficPolicies
    listTrafficPolicies_trafficPolicyIdMarker,
    listTrafficPolicies_maxItems,
    listTrafficPoliciesResponse_httpStatus,
    listTrafficPoliciesResponse_trafficPolicySummaries,
    listTrafficPoliciesResponse_isTruncated,
    listTrafficPoliciesResponse_trafficPolicyIdMarker,
    listTrafficPoliciesResponse_maxItems,

    -- * Types

    -- ** AccountLimit
    accountLimit_type,
    accountLimit_value,

    -- ** AlarmIdentifier
    alarmIdentifier_region,
    alarmIdentifier_name,

    -- ** AliasTarget
    aliasTarget_hostedZoneId,
    aliasTarget_dNSName,
    aliasTarget_evaluateTargetHealth,

    -- ** Change
    change_action,
    change_resourceRecordSet,

    -- ** ChangeBatch
    changeBatch_comment,
    changeBatch_changes,

    -- ** ChangeInfo
    changeInfo_comment,
    changeInfo_id,
    changeInfo_status,
    changeInfo_submittedAt,

    -- ** CloudWatchAlarmConfiguration
    cloudWatchAlarmConfiguration_dimensions,
    cloudWatchAlarmConfiguration_evaluationPeriods,
    cloudWatchAlarmConfiguration_threshold,
    cloudWatchAlarmConfiguration_comparisonOperator,
    cloudWatchAlarmConfiguration_period,
    cloudWatchAlarmConfiguration_metricName,
    cloudWatchAlarmConfiguration_namespace,
    cloudWatchAlarmConfiguration_statistic,

    -- ** DNSSECStatus
    dNSSECStatus_serveSignature,
    dNSSECStatus_statusMessage,

    -- ** DelegationSet
    delegationSet_id,
    delegationSet_callerReference,
    delegationSet_nameServers,

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** GeoLocation
    geoLocation_subdivisionCode,
    geoLocation_countryCode,
    geoLocation_continentCode,

    -- ** GeoLocationDetails
    geoLocationDetails_subdivisionName,
    geoLocationDetails_subdivisionCode,
    geoLocationDetails_countryName,
    geoLocationDetails_countryCode,
    geoLocationDetails_continentCode,
    geoLocationDetails_continentName,

    -- ** HealthCheck
    healthCheck_linkedService,
    healthCheck_cloudWatchAlarmConfiguration,
    healthCheck_id,
    healthCheck_callerReference,
    healthCheck_healthCheckConfig,
    healthCheck_healthCheckVersion,

    -- ** HealthCheckConfig
    healthCheckConfig_failureThreshold,
    healthCheckConfig_iPAddress,
    healthCheckConfig_enableSNI,
    healthCheckConfig_disabled,
    healthCheckConfig_searchString,
    healthCheckConfig_healthThreshold,
    healthCheckConfig_regions,
    healthCheckConfig_resourcePath,
    healthCheckConfig_insufficientDataHealthStatus,
    healthCheckConfig_alarmIdentifier,
    healthCheckConfig_measureLatency,
    healthCheckConfig_inverted,
    healthCheckConfig_fullyQualifiedDomainName,
    healthCheckConfig_childHealthChecks,
    healthCheckConfig_routingControlArn,
    healthCheckConfig_requestInterval,
    healthCheckConfig_port,
    healthCheckConfig_type,

    -- ** HealthCheckObservation
    healthCheckObservation_iPAddress,
    healthCheckObservation_statusReport,
    healthCheckObservation_region,

    -- ** HostedZone
    hostedZone_linkedService,
    hostedZone_config,
    hostedZone_resourceRecordSetCount,
    hostedZone_id,
    hostedZone_name,
    hostedZone_callerReference,

    -- ** HostedZoneConfig
    hostedZoneConfig_privateZone,
    hostedZoneConfig_comment,

    -- ** HostedZoneLimit
    hostedZoneLimit_type,
    hostedZoneLimit_value,

    -- ** HostedZoneOwner
    hostedZoneOwner_owningAccount,
    hostedZoneOwner_owningService,

    -- ** HostedZoneSummary
    hostedZoneSummary_hostedZoneId,
    hostedZoneSummary_name,
    hostedZoneSummary_owner,

    -- ** KeySigningKey
    keySigningKey_status,
    keySigningKey_signingAlgorithmMnemonic,
    keySigningKey_digestAlgorithmMnemonic,
    keySigningKey_lastModifiedDate,
    keySigningKey_keyTag,
    keySigningKey_dNSKEYRecord,
    keySigningKey_publicKey,
    keySigningKey_signingAlgorithmType,
    keySigningKey_createdDate,
    keySigningKey_flag,
    keySigningKey_digestAlgorithmType,
    keySigningKey_statusMessage,
    keySigningKey_name,
    keySigningKey_dSRecord,
    keySigningKey_kmsArn,
    keySigningKey_digestValue,

    -- ** LinkedService
    linkedService_servicePrincipal,
    linkedService_description,

    -- ** QueryLoggingConfig
    queryLoggingConfig_id,
    queryLoggingConfig_hostedZoneId,
    queryLoggingConfig_cloudWatchLogsLogGroupArn,

    -- ** ResourceRecord
    resourceRecord_value,

    -- ** ResourceRecordSet
    resourceRecordSet_ttl,
    resourceRecordSet_resourceRecords,
    resourceRecordSet_aliasTarget,
    resourceRecordSet_weight,
    resourceRecordSet_trafficPolicyInstanceId,
    resourceRecordSet_setIdentifier,
    resourceRecordSet_failover,
    resourceRecordSet_healthCheckId,
    resourceRecordSet_region,
    resourceRecordSet_geoLocation,
    resourceRecordSet_multiValueAnswer,
    resourceRecordSet_name,
    resourceRecordSet_type,

    -- ** ResourceTagSet
    resourceTagSet_resourceId,
    resourceTagSet_resourceType,
    resourceTagSet_tags,

    -- ** ReusableDelegationSetLimit
    reusableDelegationSetLimit_type,
    reusableDelegationSetLimit_value,

    -- ** StatusReport
    statusReport_status,
    statusReport_checkedTime,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TrafficPolicy
    trafficPolicy_comment,
    trafficPolicy_id,
    trafficPolicy_version,
    trafficPolicy_name,
    trafficPolicy_type,
    trafficPolicy_document,

    -- ** TrafficPolicyInstance
    trafficPolicyInstance_id,
    trafficPolicyInstance_hostedZoneId,
    trafficPolicyInstance_name,
    trafficPolicyInstance_ttl,
    trafficPolicyInstance_state,
    trafficPolicyInstance_message,
    trafficPolicyInstance_trafficPolicyId,
    trafficPolicyInstance_trafficPolicyVersion,
    trafficPolicyInstance_trafficPolicyType,

    -- ** TrafficPolicySummary
    trafficPolicySummary_id,
    trafficPolicySummary_name,
    trafficPolicySummary_type,
    trafficPolicySummary_latestVersion,
    trafficPolicySummary_trafficPolicyCount,

    -- ** VPC
    vpc_vPCRegion,
    vpc_vPCId,
  )
where

import Amazonka.Route53.ActivateKeySigningKey
import Amazonka.Route53.AssociateVPCWithHostedZone
import Amazonka.Route53.ChangeResourceRecordSets
import Amazonka.Route53.ChangeTagsForResource
import Amazonka.Route53.CreateHealthCheck
import Amazonka.Route53.CreateHostedZone
import Amazonka.Route53.CreateKeySigningKey
import Amazonka.Route53.CreateQueryLoggingConfig
import Amazonka.Route53.CreateReusableDelegationSet
import Amazonka.Route53.CreateTrafficPolicy
import Amazonka.Route53.CreateTrafficPolicyInstance
import Amazonka.Route53.CreateTrafficPolicyVersion
import Amazonka.Route53.CreateVPCAssociationAuthorization
import Amazonka.Route53.DeactivateKeySigningKey
import Amazonka.Route53.DeleteHealthCheck
import Amazonka.Route53.DeleteHostedZone
import Amazonka.Route53.DeleteKeySigningKey
import Amazonka.Route53.DeleteQueryLoggingConfig
import Amazonka.Route53.DeleteReusableDelegationSet
import Amazonka.Route53.DeleteTrafficPolicy
import Amazonka.Route53.DeleteTrafficPolicyInstance
import Amazonka.Route53.DeleteVPCAssociationAuthorization
import Amazonka.Route53.DisableHostedZoneDNSSEC
import Amazonka.Route53.DisassociateVPCFromHostedZone
import Amazonka.Route53.EnableHostedZoneDNSSEC
import Amazonka.Route53.GetAccountLimit
import Amazonka.Route53.GetChange
import Amazonka.Route53.GetCheckerIpRanges
import Amazonka.Route53.GetDNSSEC
import Amazonka.Route53.GetGeoLocation
import Amazonka.Route53.GetHealthCheck
import Amazonka.Route53.GetHealthCheckCount
import Amazonka.Route53.GetHealthCheckLastFailureReason
import Amazonka.Route53.GetHealthCheckStatus
import Amazonka.Route53.GetHostedZone
import Amazonka.Route53.GetHostedZoneCount
import Amazonka.Route53.GetHostedZoneLimit
import Amazonka.Route53.GetQueryLoggingConfig
import Amazonka.Route53.GetReusableDelegationSet
import Amazonka.Route53.GetReusableDelegationSetLimit
import Amazonka.Route53.GetTrafficPolicy
import Amazonka.Route53.GetTrafficPolicyInstance
import Amazonka.Route53.GetTrafficPolicyInstanceCount
import Amazonka.Route53.ListGeoLocations
import Amazonka.Route53.ListHealthChecks
import Amazonka.Route53.ListHostedZones
import Amazonka.Route53.ListHostedZonesByName
import Amazonka.Route53.ListHostedZonesByVPC
import Amazonka.Route53.ListQueryLoggingConfigs
import Amazonka.Route53.ListResourceRecordSets
import Amazonka.Route53.ListReusableDelegationSets
import Amazonka.Route53.ListTagsForResource
import Amazonka.Route53.ListTagsForResources
import Amazonka.Route53.ListTrafficPolicies
import Amazonka.Route53.ListTrafficPolicyInstances
import Amazonka.Route53.ListTrafficPolicyInstancesByHostedZone
import Amazonka.Route53.ListTrafficPolicyInstancesByPolicy
import Amazonka.Route53.ListTrafficPolicyVersions
import Amazonka.Route53.ListVPCAssociationAuthorizations
import Amazonka.Route53.TestDNSAnswer
import Amazonka.Route53.Types.AccountLimit
import Amazonka.Route53.Types.AlarmIdentifier
import Amazonka.Route53.Types.AliasTarget
import Amazonka.Route53.Types.Change
import Amazonka.Route53.Types.ChangeBatch
import Amazonka.Route53.Types.ChangeInfo
import Amazonka.Route53.Types.CloudWatchAlarmConfiguration
import Amazonka.Route53.Types.DNSSECStatus
import Amazonka.Route53.Types.DelegationSet
import Amazonka.Route53.Types.Dimension
import Amazonka.Route53.Types.GeoLocation
import Amazonka.Route53.Types.GeoLocationDetails
import Amazonka.Route53.Types.HealthCheck
import Amazonka.Route53.Types.HealthCheckConfig
import Amazonka.Route53.Types.HealthCheckObservation
import Amazonka.Route53.Types.HostedZone
import Amazonka.Route53.Types.HostedZoneConfig
import Amazonka.Route53.Types.HostedZoneLimit
import Amazonka.Route53.Types.HostedZoneOwner
import Amazonka.Route53.Types.HostedZoneSummary
import Amazonka.Route53.Types.KeySigningKey
import Amazonka.Route53.Types.LinkedService
import Amazonka.Route53.Types.QueryLoggingConfig
import Amazonka.Route53.Types.ResourceRecord
import Amazonka.Route53.Types.ResourceRecordSet
import Amazonka.Route53.Types.ResourceTagSet
import Amazonka.Route53.Types.ReusableDelegationSetLimit
import Amazonka.Route53.Types.StatusReport
import Amazonka.Route53.Types.Tag
import Amazonka.Route53.Types.TrafficPolicy
import Amazonka.Route53.Types.TrafficPolicyInstance
import Amazonka.Route53.Types.TrafficPolicySummary
import Amazonka.Route53.Types.VPC
import Amazonka.Route53.UpdateHealthCheck
import Amazonka.Route53.UpdateHostedZoneComment
import Amazonka.Route53.UpdateTrafficPolicyComment
import Amazonka.Route53.UpdateTrafficPolicyInstance
