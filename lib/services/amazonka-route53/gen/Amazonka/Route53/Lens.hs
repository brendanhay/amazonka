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

    -- ** ActivateKeySigningKey
    activateKeySigningKey_hostedZoneId,
    activateKeySigningKey_name,
    activateKeySigningKeyResponse_httpStatus,
    activateKeySigningKeyResponse_changeInfo,

    -- ** AssociateVPCWithHostedZone
    associateVPCWithHostedZone_comment,
    associateVPCWithHostedZone_hostedZoneId,
    associateVPCWithHostedZone_vpc,
    associateVPCWithHostedZoneResponse_httpStatus,
    associateVPCWithHostedZoneResponse_changeInfo,

    -- ** ChangeCidrCollection
    changeCidrCollection_collectionVersion,
    changeCidrCollection_id,
    changeCidrCollection_changes,
    changeCidrCollectionResponse_httpStatus,
    changeCidrCollectionResponse_id,

    -- ** ChangeResourceRecordSets
    changeResourceRecordSets_hostedZoneId,
    changeResourceRecordSets_changeBatch,
    changeResourceRecordSetsResponse_httpStatus,
    changeResourceRecordSetsResponse_changeInfo,

    -- ** ChangeTagsForResource
    changeTagsForResource_addTags,
    changeTagsForResource_removeTagKeys,
    changeTagsForResource_resourceType,
    changeTagsForResource_resourceId,
    changeTagsForResourceResponse_httpStatus,

    -- ** CreateCidrCollection
    createCidrCollection_name,
    createCidrCollection_callerReference,
    createCidrCollectionResponse_collection,
    createCidrCollectionResponse_location,
    createCidrCollectionResponse_httpStatus,

    -- ** CreateHealthCheck
    createHealthCheck_callerReference,
    createHealthCheck_healthCheckConfig,
    createHealthCheckResponse_httpStatus,
    createHealthCheckResponse_healthCheck,
    createHealthCheckResponse_location,

    -- ** CreateHostedZone
    createHostedZone_hostedZoneConfig,
    createHostedZone_vpc,
    createHostedZone_delegationSetId,
    createHostedZone_name,
    createHostedZone_callerReference,
    createHostedZoneResponse_vpc,
    createHostedZoneResponse_httpStatus,
    createHostedZoneResponse_hostedZone,
    createHostedZoneResponse_changeInfo,
    createHostedZoneResponse_delegationSet,
    createHostedZoneResponse_location,

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

    -- ** CreateQueryLoggingConfig
    createQueryLoggingConfig_hostedZoneId,
    createQueryLoggingConfig_cloudWatchLogsLogGroupArn,
    createQueryLoggingConfigResponse_httpStatus,
    createQueryLoggingConfigResponse_queryLoggingConfig,
    createQueryLoggingConfigResponse_location,

    -- ** CreateReusableDelegationSet
    createReusableDelegationSet_hostedZoneId,
    createReusableDelegationSet_callerReference,
    createReusableDelegationSetResponse_httpStatus,
    createReusableDelegationSetResponse_delegationSet,
    createReusableDelegationSetResponse_location,

    -- ** CreateTrafficPolicy
    createTrafficPolicy_comment,
    createTrafficPolicy_name,
    createTrafficPolicy_document,
    createTrafficPolicyResponse_httpStatus,
    createTrafficPolicyResponse_trafficPolicy,
    createTrafficPolicyResponse_location,

    -- ** CreateTrafficPolicyInstance
    createTrafficPolicyInstance_hostedZoneId,
    createTrafficPolicyInstance_name,
    createTrafficPolicyInstance_ttl,
    createTrafficPolicyInstance_trafficPolicyId,
    createTrafficPolicyInstance_trafficPolicyVersion,
    createTrafficPolicyInstanceResponse_httpStatus,
    createTrafficPolicyInstanceResponse_trafficPolicyInstance,
    createTrafficPolicyInstanceResponse_location,

    -- ** CreateTrafficPolicyVersion
    createTrafficPolicyVersion_comment,
    createTrafficPolicyVersion_id,
    createTrafficPolicyVersion_document,
    createTrafficPolicyVersionResponse_httpStatus,
    createTrafficPolicyVersionResponse_trafficPolicy,
    createTrafficPolicyVersionResponse_location,

    -- ** CreateVPCAssociationAuthorization
    createVPCAssociationAuthorization_hostedZoneId,
    createVPCAssociationAuthorization_vpc,
    createVPCAssociationAuthorizationResponse_httpStatus,
    createVPCAssociationAuthorizationResponse_hostedZoneId,
    createVPCAssociationAuthorizationResponse_vpc,

    -- ** DeactivateKeySigningKey
    deactivateKeySigningKey_hostedZoneId,
    deactivateKeySigningKey_name,
    deactivateKeySigningKeyResponse_httpStatus,
    deactivateKeySigningKeyResponse_changeInfo,

    -- ** DeleteCidrCollection
    deleteCidrCollection_id,
    deleteCidrCollectionResponse_httpStatus,

    -- ** DeleteHealthCheck
    deleteHealthCheck_healthCheckId,
    deleteHealthCheckResponse_httpStatus,

    -- ** DeleteHostedZone
    deleteHostedZone_id,
    deleteHostedZoneResponse_httpStatus,
    deleteHostedZoneResponse_changeInfo,

    -- ** DeleteKeySigningKey
    deleteKeySigningKey_hostedZoneId,
    deleteKeySigningKey_name,
    deleteKeySigningKeyResponse_httpStatus,
    deleteKeySigningKeyResponse_changeInfo,

    -- ** DeleteQueryLoggingConfig
    deleteQueryLoggingConfig_id,
    deleteQueryLoggingConfigResponse_httpStatus,

    -- ** DeleteReusableDelegationSet
    deleteReusableDelegationSet_id,
    deleteReusableDelegationSetResponse_httpStatus,

    -- ** DeleteTrafficPolicy
    deleteTrafficPolicy_id,
    deleteTrafficPolicy_version,
    deleteTrafficPolicyResponse_httpStatus,

    -- ** DeleteTrafficPolicyInstance
    deleteTrafficPolicyInstance_id,
    deleteTrafficPolicyInstanceResponse_httpStatus,

    -- ** DeleteVPCAssociationAuthorization
    deleteVPCAssociationAuthorization_hostedZoneId,
    deleteVPCAssociationAuthorization_vpc,
    deleteVPCAssociationAuthorizationResponse_httpStatus,

    -- ** DisableHostedZoneDNSSEC
    disableHostedZoneDNSSEC_hostedZoneId,
    disableHostedZoneDNSSECResponse_httpStatus,
    disableHostedZoneDNSSECResponse_changeInfo,

    -- ** DisassociateVPCFromHostedZone
    disassociateVPCFromHostedZone_comment,
    disassociateVPCFromHostedZone_hostedZoneId,
    disassociateVPCFromHostedZone_vpc,
    disassociateVPCFromHostedZoneResponse_httpStatus,
    disassociateVPCFromHostedZoneResponse_changeInfo,

    -- ** EnableHostedZoneDNSSEC
    enableHostedZoneDNSSEC_hostedZoneId,
    enableHostedZoneDNSSECResponse_httpStatus,
    enableHostedZoneDNSSECResponse_changeInfo,

    -- ** GetAccountLimit
    getAccountLimit_type,
    getAccountLimitResponse_httpStatus,
    getAccountLimitResponse_limit,
    getAccountLimitResponse_count,

    -- ** GetChange
    getChange_id,
    getChangeResponse_httpStatus,
    getChangeResponse_changeInfo,

    -- ** GetCheckerIpRanges
    getCheckerIpRangesResponse_httpStatus,
    getCheckerIpRangesResponse_checkerIpRanges,

    -- ** GetDNSSEC
    getDNSSEC_hostedZoneId,
    getDNSSECResponse_httpStatus,
    getDNSSECResponse_status,
    getDNSSECResponse_keySigningKeys,

    -- ** GetGeoLocation
    getGeoLocation_subdivisionCode,
    getGeoLocation_countryCode,
    getGeoLocation_continentCode,
    getGeoLocationResponse_httpStatus,
    getGeoLocationResponse_geoLocationDetails,

    -- ** GetHealthCheck
    getHealthCheck_healthCheckId,
    getHealthCheckResponse_httpStatus,
    getHealthCheckResponse_healthCheck,

    -- ** GetHealthCheckCount
    getHealthCheckCountResponse_httpStatus,
    getHealthCheckCountResponse_healthCheckCount,

    -- ** GetHealthCheckLastFailureReason
    getHealthCheckLastFailureReason_healthCheckId,
    getHealthCheckLastFailureReasonResponse_httpStatus,
    getHealthCheckLastFailureReasonResponse_healthCheckObservations,

    -- ** GetHealthCheckStatus
    getHealthCheckStatus_healthCheckId,
    getHealthCheckStatusResponse_httpStatus,
    getHealthCheckStatusResponse_healthCheckObservations,

    -- ** GetHostedZone
    getHostedZone_id,
    getHostedZoneResponse_vPCs,
    getHostedZoneResponse_delegationSet,
    getHostedZoneResponse_httpStatus,
    getHostedZoneResponse_hostedZone,

    -- ** GetHostedZoneCount
    getHostedZoneCountResponse_httpStatus,
    getHostedZoneCountResponse_hostedZoneCount,

    -- ** GetHostedZoneLimit
    getHostedZoneLimit_type,
    getHostedZoneLimit_hostedZoneId,
    getHostedZoneLimitResponse_httpStatus,
    getHostedZoneLimitResponse_limit,
    getHostedZoneLimitResponse_count,

    -- ** GetQueryLoggingConfig
    getQueryLoggingConfig_id,
    getQueryLoggingConfigResponse_httpStatus,
    getQueryLoggingConfigResponse_queryLoggingConfig,

    -- ** GetReusableDelegationSet
    getReusableDelegationSet_id,
    getReusableDelegationSetResponse_httpStatus,
    getReusableDelegationSetResponse_delegationSet,

    -- ** GetReusableDelegationSetLimit
    getReusableDelegationSetLimit_type,
    getReusableDelegationSetLimit_delegationSetId,
    getReusableDelegationSetLimitResponse_httpStatus,
    getReusableDelegationSetLimitResponse_limit,
    getReusableDelegationSetLimitResponse_count,

    -- ** GetTrafficPolicy
    getTrafficPolicy_id,
    getTrafficPolicy_version,
    getTrafficPolicyResponse_httpStatus,
    getTrafficPolicyResponse_trafficPolicy,

    -- ** GetTrafficPolicyInstance
    getTrafficPolicyInstance_id,
    getTrafficPolicyInstanceResponse_httpStatus,
    getTrafficPolicyInstanceResponse_trafficPolicyInstance,

    -- ** GetTrafficPolicyInstanceCount
    getTrafficPolicyInstanceCountResponse_httpStatus,
    getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount,

    -- ** ListCidrBlocks
    listCidrBlocks_nextToken,
    listCidrBlocks_maxResults,
    listCidrBlocks_locationName,
    listCidrBlocks_collectionId,
    listCidrBlocksResponse_nextToken,
    listCidrBlocksResponse_cidrBlocks,
    listCidrBlocksResponse_httpStatus,

    -- ** ListCidrCollections
    listCidrCollections_nextToken,
    listCidrCollections_maxResults,
    listCidrCollectionsResponse_nextToken,
    listCidrCollectionsResponse_cidrCollections,
    listCidrCollectionsResponse_httpStatus,

    -- ** ListCidrLocations
    listCidrLocations_nextToken,
    listCidrLocations_maxResults,
    listCidrLocations_collectionId,
    listCidrLocationsResponse_nextToken,
    listCidrLocationsResponse_cidrLocations,
    listCidrLocationsResponse_httpStatus,

    -- ** ListGeoLocations
    listGeoLocations_startCountryCode,
    listGeoLocations_startSubdivisionCode,
    listGeoLocations_maxItems,
    listGeoLocations_startContinentCode,
    listGeoLocationsResponse_nextSubdivisionCode,
    listGeoLocationsResponse_nextCountryCode,
    listGeoLocationsResponse_nextContinentCode,
    listGeoLocationsResponse_httpStatus,
    listGeoLocationsResponse_geoLocationDetailsList,
    listGeoLocationsResponse_isTruncated,
    listGeoLocationsResponse_maxItems,

    -- ** ListHealthChecks
    listHealthChecks_marker,
    listHealthChecks_maxItems,
    listHealthChecksResponse_nextMarker,
    listHealthChecksResponse_httpStatus,
    listHealthChecksResponse_healthChecks,
    listHealthChecksResponse_marker,
    listHealthChecksResponse_isTruncated,
    listHealthChecksResponse_maxItems,

    -- ** ListHostedZones
    listHostedZones_marker,
    listHostedZones_delegationSetId,
    listHostedZones_maxItems,
    listHostedZonesResponse_marker,
    listHostedZonesResponse_nextMarker,
    listHostedZonesResponse_httpStatus,
    listHostedZonesResponse_hostedZones,
    listHostedZonesResponse_isTruncated,
    listHostedZonesResponse_maxItems,

    -- ** ListHostedZonesByName
    listHostedZonesByName_hostedZoneId,
    listHostedZonesByName_maxItems,
    listHostedZonesByName_dNSName,
    listHostedZonesByNameResponse_hostedZoneId,
    listHostedZonesByNameResponse_nextDNSName,
    listHostedZonesByNameResponse_nextHostedZoneId,
    listHostedZonesByNameResponse_dNSName,
    listHostedZonesByNameResponse_httpStatus,
    listHostedZonesByNameResponse_hostedZones,
    listHostedZonesByNameResponse_isTruncated,
    listHostedZonesByNameResponse_maxItems,

    -- ** ListHostedZonesByVPC
    listHostedZonesByVPC_nextToken,
    listHostedZonesByVPC_maxItems,
    listHostedZonesByVPC_vPCId,
    listHostedZonesByVPC_vPCRegion,
    listHostedZonesByVPCResponse_nextToken,
    listHostedZonesByVPCResponse_httpStatus,
    listHostedZonesByVPCResponse_hostedZoneSummaries,
    listHostedZonesByVPCResponse_maxItems,

    -- ** ListQueryLoggingConfigs
    listQueryLoggingConfigs_hostedZoneId,
    listQueryLoggingConfigs_nextToken,
    listQueryLoggingConfigs_maxResults,
    listQueryLoggingConfigsResponse_nextToken,
    listQueryLoggingConfigsResponse_httpStatus,
    listQueryLoggingConfigsResponse_queryLoggingConfigs,

    -- ** ListResourceRecordSets
    listResourceRecordSets_startRecordType,
    listResourceRecordSets_maxItems,
    listResourceRecordSets_startRecordIdentifier,
    listResourceRecordSets_startRecordName,
    listResourceRecordSets_hostedZoneId,
    listResourceRecordSetsResponse_nextRecordIdentifier,
    listResourceRecordSetsResponse_nextRecordName,
    listResourceRecordSetsResponse_nextRecordType,
    listResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_resourceRecordSets,
    listResourceRecordSetsResponse_isTruncated,
    listResourceRecordSetsResponse_maxItems,

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

    -- ** ListTagsForResources
    listTagsForResources_resourceType,
    listTagsForResources_resourceIds,
    listTagsForResourcesResponse_httpStatus,
    listTagsForResourcesResponse_resourceTagSets,

    -- ** ListTrafficPolicies
    listTrafficPolicies_trafficPolicyIdMarker,
    listTrafficPolicies_maxItems,
    listTrafficPoliciesResponse_httpStatus,
    listTrafficPoliciesResponse_trafficPolicySummaries,
    listTrafficPoliciesResponse_isTruncated,
    listTrafficPoliciesResponse_trafficPolicyIdMarker,
    listTrafficPoliciesResponse_maxItems,

    -- ** ListTrafficPolicyInstances
    listTrafficPolicyInstances_hostedZoneIdMarker,
    listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstances_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstances_maxItems,
    listTrafficPolicyInstancesResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesResponse_httpStatus,
    listTrafficPolicyInstancesResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesResponse_isTruncated,
    listTrafficPolicyInstancesResponse_maxItems,

    -- ** ListTrafficPolicyInstancesByHostedZone
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZone_maxItems,
    listTrafficPolicyInstancesByHostedZone_hostedZoneId,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_httpStatus,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByHostedZoneResponse_isTruncated,
    listTrafficPolicyInstancesByHostedZoneResponse_maxItems,

    -- ** ListTrafficPolicyInstancesByPolicy
    listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicy_maxItems,
    listTrafficPolicyInstancesByPolicy_trafficPolicyId,
    listTrafficPolicyInstancesByPolicy_trafficPolicyVersion,
    listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker,
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

    -- ** ListVPCAssociationAuthorizations
    listVPCAssociationAuthorizations_nextToken,
    listVPCAssociationAuthorizations_maxResults,
    listVPCAssociationAuthorizations_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_nextToken,
    listVPCAssociationAuthorizationsResponse_httpStatus,
    listVPCAssociationAuthorizationsResponse_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_vPCs,

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

    -- ** UpdateHealthCheck
    updateHealthCheck_port,
    updateHealthCheck_healthThreshold,
    updateHealthCheck_healthCheckVersion,
    updateHealthCheck_failureThreshold,
    updateHealthCheck_alarmIdentifier,
    updateHealthCheck_regions,
    updateHealthCheck_resourcePath,
    updateHealthCheck_childHealthChecks,
    updateHealthCheck_searchString,
    updateHealthCheck_resetElements,
    updateHealthCheck_disabled,
    updateHealthCheck_inverted,
    updateHealthCheck_fullyQualifiedDomainName,
    updateHealthCheck_insufficientDataHealthStatus,
    updateHealthCheck_enableSNI,
    updateHealthCheck_iPAddress,
    updateHealthCheck_healthCheckId,
    updateHealthCheckResponse_httpStatus,
    updateHealthCheckResponse_healthCheck,

    -- ** UpdateHostedZoneComment
    updateHostedZoneComment_comment,
    updateHostedZoneComment_id,
    updateHostedZoneCommentResponse_httpStatus,
    updateHostedZoneCommentResponse_hostedZone,

    -- ** UpdateTrafficPolicyComment
    updateTrafficPolicyComment_id,
    updateTrafficPolicyComment_version,
    updateTrafficPolicyComment_comment,
    updateTrafficPolicyCommentResponse_httpStatus,
    updateTrafficPolicyCommentResponse_trafficPolicy,

    -- ** UpdateTrafficPolicyInstance
    updateTrafficPolicyInstance_id,
    updateTrafficPolicyInstance_ttl,
    updateTrafficPolicyInstance_trafficPolicyId,
    updateTrafficPolicyInstance_trafficPolicyVersion,
    updateTrafficPolicyInstanceResponse_httpStatus,
    updateTrafficPolicyInstanceResponse_trafficPolicyInstance,

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

    -- ** CidrBlockSummary
    cidrBlockSummary_cidrBlock,
    cidrBlockSummary_locationName,

    -- ** CidrCollection
    cidrCollection_name,
    cidrCollection_arn,
    cidrCollection_id,
    cidrCollection_version,

    -- ** CidrCollectionChange
    cidrCollectionChange_locationName,
    cidrCollectionChange_action,
    cidrCollectionChange_cidrList,

    -- ** CidrRoutingConfig
    cidrRoutingConfig_collectionId,
    cidrRoutingConfig_locationName,

    -- ** CloudWatchAlarmConfiguration
    cloudWatchAlarmConfiguration_dimensions,
    cloudWatchAlarmConfiguration_evaluationPeriods,
    cloudWatchAlarmConfiguration_threshold,
    cloudWatchAlarmConfiguration_comparisonOperator,
    cloudWatchAlarmConfiguration_period,
    cloudWatchAlarmConfiguration_metricName,
    cloudWatchAlarmConfiguration_namespace,
    cloudWatchAlarmConfiguration_statistic,

    -- ** CollectionSummary
    collectionSummary_name,
    collectionSummary_arn,
    collectionSummary_id,
    collectionSummary_version,

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
    geoLocationDetails_subdivisionCode,
    geoLocationDetails_countryName,
    geoLocationDetails_countryCode,
    geoLocationDetails_subdivisionName,
    geoLocationDetails_continentCode,
    geoLocationDetails_continentName,

    -- ** HealthCheck
    healthCheck_cloudWatchAlarmConfiguration,
    healthCheck_linkedService,
    healthCheck_id,
    healthCheck_callerReference,
    healthCheck_healthCheckConfig,
    healthCheck_healthCheckVersion,

    -- ** HealthCheckConfig
    healthCheckConfig_port,
    healthCheckConfig_healthThreshold,
    healthCheckConfig_failureThreshold,
    healthCheckConfig_alarmIdentifier,
    healthCheckConfig_regions,
    healthCheckConfig_resourcePath,
    healthCheckConfig_childHealthChecks,
    healthCheckConfig_searchString,
    healthCheckConfig_requestInterval,
    healthCheckConfig_routingControlArn,
    healthCheckConfig_disabled,
    healthCheckConfig_measureLatency,
    healthCheckConfig_inverted,
    healthCheckConfig_fullyQualifiedDomainName,
    healthCheckConfig_insufficientDataHealthStatus,
    healthCheckConfig_enableSNI,
    healthCheckConfig_iPAddress,
    healthCheckConfig_type,

    -- ** HealthCheckObservation
    healthCheckObservation_statusReport,
    healthCheckObservation_region,
    healthCheckObservation_iPAddress,

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
    keySigningKey_kmsArn,
    keySigningKey_digestAlgorithmType,
    keySigningKey_name,
    keySigningKey_signingAlgorithmMnemonic,
    keySigningKey_publicKey,
    keySigningKey_lastModifiedDate,
    keySigningKey_flag,
    keySigningKey_dSRecord,
    keySigningKey_status,
    keySigningKey_dNSKEYRecord,
    keySigningKey_signingAlgorithmType,
    keySigningKey_createdDate,
    keySigningKey_keyTag,
    keySigningKey_statusMessage,
    keySigningKey_digestAlgorithmMnemonic,
    keySigningKey_digestValue,

    -- ** LinkedService
    linkedService_description,
    linkedService_servicePrincipal,

    -- ** LocationSummary
    locationSummary_locationName,

    -- ** QueryLoggingConfig
    queryLoggingConfig_id,
    queryLoggingConfig_hostedZoneId,
    queryLoggingConfig_cloudWatchLogsLogGroupArn,

    -- ** ResourceRecord
    resourceRecord_value,

    -- ** ResourceRecordSet
    resourceRecordSet_cidrRoutingConfig,
    resourceRecordSet_ttl,
    resourceRecordSet_multiValueAnswer,
    resourceRecordSet_trafficPolicyInstanceId,
    resourceRecordSet_region,
    resourceRecordSet_aliasTarget,
    resourceRecordSet_resourceRecords,
    resourceRecordSet_setIdentifier,
    resourceRecordSet_weight,
    resourceRecordSet_healthCheckId,
    resourceRecordSet_failover,
    resourceRecordSet_geoLocation,
    resourceRecordSet_name,
    resourceRecordSet_type,

    -- ** ResourceTagSet
    resourceTagSet_resourceId,
    resourceTagSet_tags,
    resourceTagSet_resourceType,

    -- ** ReusableDelegationSetLimit
    reusableDelegationSetLimit_type,
    reusableDelegationSetLimit_value,

    -- ** StatusReport
    statusReport_checkedTime,
    statusReport_status,

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
    vpc_vPCId,
    vpc_vPCRegion,
  )
where

import Amazonka.Route53.ActivateKeySigningKey
import Amazonka.Route53.AssociateVPCWithHostedZone
import Amazonka.Route53.ChangeCidrCollection
import Amazonka.Route53.ChangeResourceRecordSets
import Amazonka.Route53.ChangeTagsForResource
import Amazonka.Route53.CreateCidrCollection
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
import Amazonka.Route53.DeleteCidrCollection
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
import Amazonka.Route53.ListCidrBlocks
import Amazonka.Route53.ListCidrCollections
import Amazonka.Route53.ListCidrLocations
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
import Amazonka.Route53.Types.CidrBlockSummary
import Amazonka.Route53.Types.CidrCollection
import Amazonka.Route53.Types.CidrCollectionChange
import Amazonka.Route53.Types.CidrRoutingConfig
import Amazonka.Route53.Types.CloudWatchAlarmConfiguration
import Amazonka.Route53.Types.CollectionSummary
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
import Amazonka.Route53.Types.LocationSummary
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
