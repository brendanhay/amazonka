{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Lens
  ( -- * Operations

    -- ** CreateReusableDelegationSet
    createReusableDelegationSet_hostedZoneId,
    createReusableDelegationSet_callerReference,
    createReusableDelegationSetResponse_httpStatus,
    createReusableDelegationSetResponse_delegationSet,
    createReusableDelegationSetResponse_location,

    -- ** GetHealthCheckCount
    getHealthCheckCountResponse_httpStatus,
    getHealthCheckCountResponse_healthCheckCount,

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

    -- ** CreateTrafficPolicy
    createTrafficPolicy_comment,
    createTrafficPolicy_name,
    createTrafficPolicy_document,
    createTrafficPolicyResponse_httpStatus,
    createTrafficPolicyResponse_trafficPolicy,
    createTrafficPolicyResponse_location,

    -- ** ListGeoLocations
    listGeoLocations_startSubdivisionCode,
    listGeoLocations_startCountryCode,
    listGeoLocations_startContinentCode,
    listGeoLocations_maxItems,
    listGeoLocationsResponse_nextSubdivisionCode,
    listGeoLocationsResponse_nextContinentCode,
    listGeoLocationsResponse_nextCountryCode,
    listGeoLocationsResponse_httpStatus,
    listGeoLocationsResponse_geoLocationDetailsList,
    listGeoLocationsResponse_isTruncated,
    listGeoLocationsResponse_maxItems,

    -- ** ListTrafficPolicies
    listTrafficPolicies_trafficPolicyIdMarker,
    listTrafficPolicies_maxItems,
    listTrafficPoliciesResponse_httpStatus,
    listTrafficPoliciesResponse_trafficPolicySummaries,
    listTrafficPoliciesResponse_isTruncated,
    listTrafficPoliciesResponse_trafficPolicyIdMarker,
    listTrafficPoliciesResponse_maxItems,

    -- ** GetGeoLocation
    getGeoLocation_continentCode,
    getGeoLocation_subdivisionCode,
    getGeoLocation_countryCode,
    getGeoLocationResponse_httpStatus,
    getGeoLocationResponse_geoLocationDetails,

    -- ** ChangeTagsForResource
    changeTagsForResource_addTags,
    changeTagsForResource_removeTagKeys,
    changeTagsForResource_resourceType,
    changeTagsForResource_resourceId,
    changeTagsForResourceResponse_httpStatus,

    -- ** DeleteHostedZone
    deleteHostedZone_id,
    deleteHostedZoneResponse_httpStatus,
    deleteHostedZoneResponse_changeInfo,

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

    -- ** DisassociateVPCFromHostedZone
    disassociateVPCFromHostedZone_comment,
    disassociateVPCFromHostedZone_hostedZoneId,
    disassociateVPCFromHostedZone_vpc,
    disassociateVPCFromHostedZoneResponse_httpStatus,
    disassociateVPCFromHostedZoneResponse_changeInfo,

    -- ** ListHostedZones
    listHostedZones_delegationSetId,
    listHostedZones_maxItems,
    listHostedZones_marker,
    listHostedZonesResponse_nextMarker,
    listHostedZonesResponse_marker,
    listHostedZonesResponse_httpStatus,
    listHostedZonesResponse_hostedZones,
    listHostedZonesResponse_isTruncated,
    listHostedZonesResponse_maxItems,

    -- ** CreateHostedZone
    createHostedZone_hostedZoneConfig,
    createHostedZone_delegationSetId,
    createHostedZone_vpc,
    createHostedZone_name,
    createHostedZone_callerReference,
    createHostedZoneResponse_vpc,
    createHostedZoneResponse_httpStatus,
    createHostedZoneResponse_hostedZone,
    createHostedZoneResponse_changeInfo,
    createHostedZoneResponse_delegationSet,
    createHostedZoneResponse_location,

    -- ** DeactivateKeySigningKey
    deactivateKeySigningKey_hostedZoneId,
    deactivateKeySigningKey_name,
    deactivateKeySigningKeyResponse_httpStatus,
    deactivateKeySigningKeyResponse_changeInfo,

    -- ** TestDNSAnswer
    testDNSAnswer_resolverIP,
    testDNSAnswer_eDNS0ClientSubnetMask,
    testDNSAnswer_eDNS0ClientSubnetIP,
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

    -- ** ChangeResourceRecordSets
    changeResourceRecordSets_hostedZoneId,
    changeResourceRecordSets_changeBatch,
    changeResourceRecordSetsResponse_httpStatus,
    changeResourceRecordSetsResponse_changeInfo,

    -- ** GetReusableDelegationSetLimit
    getReusableDelegationSetLimit_type,
    getReusableDelegationSetLimit_delegationSetId,
    getReusableDelegationSetLimitResponse_httpStatus,
    getReusableDelegationSetLimitResponse_limit,
    getReusableDelegationSetLimitResponse_count,

    -- ** GetReusableDelegationSet
    getReusableDelegationSet_id,
    getReusableDelegationSetResponse_httpStatus,
    getReusableDelegationSetResponse_delegationSet,

    -- ** GetAccountLimit
    getAccountLimit_type,
    getAccountLimitResponse_httpStatus,
    getAccountLimitResponse_limit,
    getAccountLimitResponse_count,

    -- ** GetDNSSEC
    getDNSSEC_hostedZoneId,
    getDNSSECResponse_httpStatus,
    getDNSSECResponse_status,
    getDNSSECResponse_keySigningKeys,

    -- ** GetCheckerIpRanges
    getCheckerIpRangesResponse_httpStatus,
    getCheckerIpRangesResponse_checkerIpRanges,

    -- ** ListReusableDelegationSets
    listReusableDelegationSets_maxItems,
    listReusableDelegationSets_marker,
    listReusableDelegationSetsResponse_nextMarker,
    listReusableDelegationSetsResponse_httpStatus,
    listReusableDelegationSetsResponse_delegationSets,
    listReusableDelegationSetsResponse_marker,
    listReusableDelegationSetsResponse_isTruncated,
    listReusableDelegationSetsResponse_maxItems,

    -- ** DeleteKeySigningKey
    deleteKeySigningKey_hostedZoneId,
    deleteKeySigningKey_name,
    deleteKeySigningKeyResponse_httpStatus,
    deleteKeySigningKeyResponse_changeInfo,

    -- ** GetTrafficPolicyInstance
    getTrafficPolicyInstance_id,
    getTrafficPolicyInstanceResponse_httpStatus,
    getTrafficPolicyInstanceResponse_trafficPolicyInstance,

    -- ** CreateQueryLoggingConfig
    createQueryLoggingConfig_hostedZoneId,
    createQueryLoggingConfig_cloudWatchLogsLogGroupArn,
    createQueryLoggingConfigResponse_httpStatus,
    createQueryLoggingConfigResponse_queryLoggingConfig,
    createQueryLoggingConfigResponse_location,

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

    -- ** UpdateTrafficPolicyComment
    updateTrafficPolicyComment_id,
    updateTrafficPolicyComment_version,
    updateTrafficPolicyComment_comment,
    updateTrafficPolicyCommentResponse_httpStatus,
    updateTrafficPolicyCommentResponse_trafficPolicy,

    -- ** ListResourceRecordSets
    listResourceRecordSets_startRecordIdentifier,
    listResourceRecordSets_startRecordType,
    listResourceRecordSets_maxItems,
    listResourceRecordSets_startRecordName,
    listResourceRecordSets_hostedZoneId,
    listResourceRecordSetsResponse_nextRecordType,
    listResourceRecordSetsResponse_nextRecordIdentifier,
    listResourceRecordSetsResponse_nextRecordName,
    listResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_resourceRecordSets,
    listResourceRecordSetsResponse_isTruncated,
    listResourceRecordSetsResponse_maxItems,

    -- ** GetHealthCheck
    getHealthCheck_healthCheckId,
    getHealthCheckResponse_httpStatus,
    getHealthCheckResponse_healthCheck,

    -- ** DeleteTrafficPolicy
    deleteTrafficPolicy_id,
    deleteTrafficPolicy_version,
    deleteTrafficPolicyResponse_httpStatus,

    -- ** GetHostedZone
    getHostedZone_id,
    getHostedZoneResponse_delegationSet,
    getHostedZoneResponse_vPCs,
    getHostedZoneResponse_httpStatus,
    getHostedZoneResponse_hostedZone,

    -- ** GetTrafficPolicyInstanceCount
    getTrafficPolicyInstanceCountResponse_httpStatus,
    getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount,

    -- ** ListTagsForResources
    listTagsForResources_resourceType,
    listTagsForResources_resourceIds,
    listTagsForResourcesResponse_httpStatus,
    listTagsForResourcesResponse_resourceTagSets,

    -- ** ListTrafficPolicyInstancesByHostedZone
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZone_maxItems,
    listTrafficPolicyInstancesByHostedZone_hostedZoneId,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_httpStatus,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByHostedZoneResponse_isTruncated,
    listTrafficPolicyInstancesByHostedZoneResponse_maxItems,

    -- ** ListVPCAssociationAuthorizations
    listVPCAssociationAuthorizations_nextToken,
    listVPCAssociationAuthorizations_maxResults,
    listVPCAssociationAuthorizations_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_nextToken,
    listVPCAssociationAuthorizationsResponse_httpStatus,
    listVPCAssociationAuthorizationsResponse_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_vPCs,

    -- ** ListTrafficPolicyInstancesByPolicy
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicy_maxItems,
    listTrafficPolicyInstancesByPolicy_trafficPolicyId,
    listTrafficPolicyInstancesByPolicy_trafficPolicyVersion,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicyResponse_httpStatus,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByPolicyResponse_isTruncated,
    listTrafficPolicyInstancesByPolicyResponse_maxItems,

    -- ** ListTrafficPolicyVersions
    listTrafficPolicyVersions_trafficPolicyVersionMarker,
    listTrafficPolicyVersions_maxItems,
    listTrafficPolicyVersions_id,
    listTrafficPolicyVersionsResponse_httpStatus,
    listTrafficPolicyVersionsResponse_trafficPolicies,
    listTrafficPolicyVersionsResponse_isTruncated,
    listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker,
    listTrafficPolicyVersionsResponse_maxItems,

    -- ** CreateVPCAssociationAuthorization
    createVPCAssociationAuthorization_hostedZoneId,
    createVPCAssociationAuthorization_vpc,
    createVPCAssociationAuthorizationResponse_httpStatus,
    createVPCAssociationAuthorizationResponse_hostedZoneId,
    createVPCAssociationAuthorizationResponse_vpc,

    -- ** CreateTrafficPolicyVersion
    createTrafficPolicyVersion_comment,
    createTrafficPolicyVersion_id,
    createTrafficPolicyVersion_document,
    createTrafficPolicyVersionResponse_httpStatus,
    createTrafficPolicyVersionResponse_trafficPolicy,
    createTrafficPolicyVersionResponse_location,

    -- ** ListHealthChecks
    listHealthChecks_maxItems,
    listHealthChecks_marker,
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

    -- ** DeleteHealthCheck
    deleteHealthCheck_healthCheckId,
    deleteHealthCheckResponse_httpStatus,

    -- ** UpdateHealthCheck
    updateHealthCheck_failureThreshold,
    updateHealthCheck_childHealthChecks,
    updateHealthCheck_searchString,
    updateHealthCheck_disabled,
    updateHealthCheck_alarmIdentifier,
    updateHealthCheck_enableSNI,
    updateHealthCheck_insufficientDataHealthStatus,
    updateHealthCheck_resourcePath,
    updateHealthCheck_iPAddress,
    updateHealthCheck_port,
    updateHealthCheck_regions,
    updateHealthCheck_inverted,
    updateHealthCheck_fullyQualifiedDomainName,
    updateHealthCheck_healthThreshold,
    updateHealthCheck_healthCheckVersion,
    updateHealthCheck_resetElements,
    updateHealthCheck_healthCheckId,
    updateHealthCheckResponse_httpStatus,
    updateHealthCheckResponse_healthCheck,

    -- ** CreateTrafficPolicyInstance
    createTrafficPolicyInstance_hostedZoneId,
    createTrafficPolicyInstance_name,
    createTrafficPolicyInstance_ttl,
    createTrafficPolicyInstance_trafficPolicyId,
    createTrafficPolicyInstance_trafficPolicyVersion,
    createTrafficPolicyInstanceResponse_httpStatus,
    createTrafficPolicyInstanceResponse_trafficPolicyInstance,
    createTrafficPolicyInstanceResponse_location,

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

    -- ** GetChange
    getChange_id,
    getChangeResponse_httpStatus,
    getChangeResponse_changeInfo,

    -- ** UpdateHostedZoneComment
    updateHostedZoneComment_comment,
    updateHostedZoneComment_id,
    updateHostedZoneCommentResponse_httpStatus,
    updateHostedZoneCommentResponse_hostedZone,

    -- ** ListTrafficPolicyInstances
    listTrafficPolicyInstances_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstances_hostedZoneIdMarker,
    listTrafficPolicyInstances_maxItems,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesResponse_httpStatus,
    listTrafficPolicyInstancesResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesResponse_isTruncated,
    listTrafficPolicyInstancesResponse_maxItems,

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

    -- ** GetQueryLoggingConfig
    getQueryLoggingConfig_id,
    getQueryLoggingConfigResponse_httpStatus,
    getQueryLoggingConfigResponse_queryLoggingConfig,

    -- ** DeleteReusableDelegationSet
    deleteReusableDelegationSet_id,
    deleteReusableDelegationSetResponse_httpStatus,

    -- ** DeleteQueryLoggingConfig
    deleteQueryLoggingConfig_id,
    deleteQueryLoggingConfigResponse_httpStatus,

    -- ** GetHostedZoneCount
    getHostedZoneCountResponse_httpStatus,
    getHostedZoneCountResponse_hostedZoneCount,

    -- ** EnableHostedZoneDNSSEC
    enableHostedZoneDNSSEC_hostedZoneId,
    enableHostedZoneDNSSECResponse_httpStatus,
    enableHostedZoneDNSSECResponse_changeInfo,

    -- ** ListTagsForResource
    listTagsForResource_resourceType,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_resourceTagSet,

    -- ** GetHealthCheckLastFailureReason
    getHealthCheckLastFailureReason_healthCheckId,
    getHealthCheckLastFailureReasonResponse_httpStatus,
    getHealthCheckLastFailureReasonResponse_healthCheckObservations,

    -- ** ActivateKeySigningKey
    activateKeySigningKey_hostedZoneId,
    activateKeySigningKey_name,
    activateKeySigningKeyResponse_httpStatus,
    activateKeySigningKeyResponse_changeInfo,

    -- ** ListHostedZonesByName
    listHostedZonesByName_hostedZoneId,
    listHostedZonesByName_dNSName,
    listHostedZonesByName_maxItems,
    listHostedZonesByNameResponse_nextDNSName,
    listHostedZonesByNameResponse_hostedZoneId,
    listHostedZonesByNameResponse_dNSName,
    listHostedZonesByNameResponse_nextHostedZoneId,
    listHostedZonesByNameResponse_httpStatus,
    listHostedZonesByNameResponse_hostedZones,
    listHostedZonesByNameResponse_isTruncated,
    listHostedZonesByNameResponse_maxItems,

    -- ** ListQueryLoggingConfigs
    listQueryLoggingConfigs_nextToken,
    listQueryLoggingConfigs_maxResults,
    listQueryLoggingConfigs_hostedZoneId,
    listQueryLoggingConfigsResponse_nextToken,
    listQueryLoggingConfigsResponse_httpStatus,
    listQueryLoggingConfigsResponse_queryLoggingConfigs,

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
    dNSSECStatus_statusMessage,
    dNSSECStatus_serveSignature,

    -- ** DelegationSet
    delegationSet_id,
    delegationSet_callerReference,
    delegationSet_nameServers,

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** GeoLocation
    geoLocation_continentCode,
    geoLocation_subdivisionCode,
    geoLocation_countryCode,

    -- ** GeoLocationDetails
    geoLocationDetails_countryName,
    geoLocationDetails_continentName,
    geoLocationDetails_continentCode,
    geoLocationDetails_subdivisionCode,
    geoLocationDetails_countryCode,
    geoLocationDetails_subdivisionName,

    -- ** HealthCheck
    healthCheck_cloudWatchAlarmConfiguration,
    healthCheck_linkedService,
    healthCheck_id,
    healthCheck_callerReference,
    healthCheck_healthCheckConfig,
    healthCheck_healthCheckVersion,

    -- ** HealthCheckConfig
    healthCheckConfig_failureThreshold,
    healthCheckConfig_childHealthChecks,
    healthCheckConfig_searchString,
    healthCheckConfig_disabled,
    healthCheckConfig_alarmIdentifier,
    healthCheckConfig_enableSNI,
    healthCheckConfig_insufficientDataHealthStatus,
    healthCheckConfig_resourcePath,
    healthCheckConfig_iPAddress,
    healthCheckConfig_port,
    healthCheckConfig_routingControlArn,
    healthCheckConfig_requestInterval,
    healthCheckConfig_regions,
    healthCheckConfig_inverted,
    healthCheckConfig_fullyQualifiedDomainName,
    healthCheckConfig_healthThreshold,
    healthCheckConfig_measureLatency,
    healthCheckConfig_type,

    -- ** HealthCheckObservation
    healthCheckObservation_iPAddress,
    healthCheckObservation_region,
    healthCheckObservation_statusReport,

    -- ** HostedZone
    hostedZone_resourceRecordSetCount,
    hostedZone_config,
    hostedZone_linkedService,
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
    keySigningKey_digestAlgorithmType,
    keySigningKey_lastModifiedDate,
    keySigningKey_statusMessage,
    keySigningKey_createdDate,
    keySigningKey_signingAlgorithmMnemonic,
    keySigningKey_status,
    keySigningKey_publicKey,
    keySigningKey_digestValue,
    keySigningKey_dNSKEYRecord,
    keySigningKey_name,
    keySigningKey_digestAlgorithmMnemonic,
    keySigningKey_signingAlgorithmType,
    keySigningKey_flag,
    keySigningKey_kmsArn,
    keySigningKey_keyTag,
    keySigningKey_dSRecord,

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
    resourceRecordSet_healthCheckId,
    resourceRecordSet_multiValueAnswer,
    resourceRecordSet_geoLocation,
    resourceRecordSet_weight,
    resourceRecordSet_aliasTarget,
    resourceRecordSet_failover,
    resourceRecordSet_resourceRecords,
    resourceRecordSet_ttl,
    resourceRecordSet_setIdentifier,
    resourceRecordSet_trafficPolicyInstanceId,
    resourceRecordSet_region,
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
    tag_key,
    tag_value,

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

import Network.AWS.Route53.ActivateKeySigningKey
import Network.AWS.Route53.AssociateVPCWithHostedZone
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.ChangeTagsForResource
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateKeySigningKey
import Network.AWS.Route53.CreateQueryLoggingConfig
import Network.AWS.Route53.CreateReusableDelegationSet
import Network.AWS.Route53.CreateTrafficPolicy
import Network.AWS.Route53.CreateTrafficPolicyInstance
import Network.AWS.Route53.CreateTrafficPolicyVersion
import Network.AWS.Route53.CreateVPCAssociationAuthorization
import Network.AWS.Route53.DeactivateKeySigningKey
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.DeleteHostedZone
import Network.AWS.Route53.DeleteKeySigningKey
import Network.AWS.Route53.DeleteQueryLoggingConfig
import Network.AWS.Route53.DeleteReusableDelegationSet
import Network.AWS.Route53.DeleteTrafficPolicy
import Network.AWS.Route53.DeleteTrafficPolicyInstance
import Network.AWS.Route53.DeleteVPCAssociationAuthorization
import Network.AWS.Route53.DisableHostedZoneDNSSEC
import Network.AWS.Route53.DisassociateVPCFromHostedZone
import Network.AWS.Route53.EnableHostedZoneDNSSEC
import Network.AWS.Route53.GetAccountLimit
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.GetCheckerIpRanges
import Network.AWS.Route53.GetDNSSEC
import Network.AWS.Route53.GetGeoLocation
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.GetHealthCheckCount
import Network.AWS.Route53.GetHealthCheckLastFailureReason
import Network.AWS.Route53.GetHealthCheckStatus
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetHostedZoneCount
import Network.AWS.Route53.GetHostedZoneLimit
import Network.AWS.Route53.GetQueryLoggingConfig
import Network.AWS.Route53.GetReusableDelegationSet
import Network.AWS.Route53.GetReusableDelegationSetLimit
import Network.AWS.Route53.GetTrafficPolicy
import Network.AWS.Route53.GetTrafficPolicyInstance
import Network.AWS.Route53.GetTrafficPolicyInstanceCount
import Network.AWS.Route53.ListGeoLocations
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.ListHostedZonesByName
import Network.AWS.Route53.ListHostedZonesByVPC
import Network.AWS.Route53.ListQueryLoggingConfigs
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.ListReusableDelegationSets
import Network.AWS.Route53.ListTagsForResource
import Network.AWS.Route53.ListTagsForResources
import Network.AWS.Route53.ListTrafficPolicies
import Network.AWS.Route53.ListTrafficPolicyInstances
import Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
import Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
import Network.AWS.Route53.ListTrafficPolicyVersions
import Network.AWS.Route53.ListVPCAssociationAuthorizations
import Network.AWS.Route53.TestDNSAnswer
import Network.AWS.Route53.Types.AccountLimit
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.AliasTarget
import Network.AWS.Route53.Types.Change
import Network.AWS.Route53.Types.ChangeBatch
import Network.AWS.Route53.Types.ChangeInfo
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.DNSSECStatus
import Network.AWS.Route53.Types.DelegationSet
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.GeoLocation
import Network.AWS.Route53.Types.GeoLocationDetails
import Network.AWS.Route53.Types.HealthCheck
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.HealthCheckObservation
import Network.AWS.Route53.Types.HostedZone
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.HostedZoneLimit
import Network.AWS.Route53.Types.HostedZoneOwner
import Network.AWS.Route53.Types.HostedZoneSummary
import Network.AWS.Route53.Types.KeySigningKey
import Network.AWS.Route53.Types.LinkedService
import Network.AWS.Route53.Types.QueryLoggingConfig
import Network.AWS.Route53.Types.ResourceRecord
import Network.AWS.Route53.Types.ResourceRecordSet
import Network.AWS.Route53.Types.ResourceTagSet
import Network.AWS.Route53.Types.ReusableDelegationSetLimit
import Network.AWS.Route53.Types.StatusReport
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TrafficPolicy
import Network.AWS.Route53.Types.TrafficPolicyInstance
import Network.AWS.Route53.Types.TrafficPolicySummary
import Network.AWS.Route53.Types.VPC
import Network.AWS.Route53.UpdateHealthCheck
import Network.AWS.Route53.UpdateHostedZoneComment
import Network.AWS.Route53.UpdateTrafficPolicyComment
import Network.AWS.Route53.UpdateTrafficPolicyInstance
