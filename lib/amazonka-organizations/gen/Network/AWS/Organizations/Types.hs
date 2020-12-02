{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types
  ( -- * Service Configuration
    organizations,

    -- * Errors

    -- * AccountJoinedMethod
    AccountJoinedMethod (..),

    -- * AccountStatus
    AccountStatus (..),

    -- * ActionType
    ActionType (..),

    -- * ChildType
    ChildType (..),

    -- * CreateAccountFailureReason
    CreateAccountFailureReason (..),

    -- * CreateAccountState
    CreateAccountState (..),

    -- * EffectivePolicyType
    EffectivePolicyType (..),

    -- * HandshakePartyType
    HandshakePartyType (..),

    -- * HandshakeResourceType
    HandshakeResourceType (..),

    -- * HandshakeState
    HandshakeState (..),

    -- * IAMUserAccessToBilling
    IAMUserAccessToBilling (..),

    -- * OrganizationFeatureSet
    OrganizationFeatureSet (..),

    -- * ParentType
    ParentType (..),

    -- * PolicyType
    PolicyType (..),

    -- * PolicyTypeStatus
    PolicyTypeStatus (..),

    -- * TargetType
    TargetType (..),

    -- * Account
    Account,
    account,
    aStatus,
    aJoinedMethod,
    aEmail,
    aARN,
    aJoinedTimestamp,
    aName,
    aId,

    -- * Child
    Child,
    child,
    cId,
    cType,

    -- * CreateAccountStatus
    CreateAccountStatus,
    createAccountStatus,
    casFailureReason,
    casState,
    casCompletedTimestamp,
    casAccountName,
    casAccountId,
    casId,
    casGovCloudAccountId,
    casRequestedTimestamp,

    -- * DelegatedAdministrator
    DelegatedAdministrator,
    delegatedAdministrator,
    daStatus,
    daJoinedMethod,
    daEmail,
    daARN,
    daJoinedTimestamp,
    daDelegationEnabledDate,
    daName,
    daId,

    -- * DelegatedService
    DelegatedService,
    delegatedService,
    dsServicePrincipal,
    dsDelegationEnabledDate,

    -- * EffectivePolicy
    EffectivePolicy,
    effectivePolicy,
    epTargetId,
    epPolicyType,
    epLastUpdatedTimestamp,
    epPolicyContent,

    -- * EnabledServicePrincipal
    EnabledServicePrincipal,
    enabledServicePrincipal,
    espServicePrincipal,
    espDateEnabled,

    -- * Handshake
    Handshake,
    handshake,
    hState,
    hARN,
    hAction,
    hResources,
    hId,
    hExpirationTimestamp,
    hParties,
    hRequestedTimestamp,

    -- * HandshakeFilter
    HandshakeFilter,
    handshakeFilter,
    hfParentHandshakeId,
    hfActionType,

    -- * HandshakeParty
    HandshakeParty,
    handshakeParty,
    hpId,
    hpType,

    -- * HandshakeResource
    HandshakeResource,
    handshakeResource,
    hrValue,
    hrResources,
    hrType,

    -- * Organization
    Organization,
    organization,
    oARN,
    oMasterAccountId,
    oMasterAccountARN,
    oMasterAccountEmail,
    oAvailablePolicyTypes,
    oId,
    oFeatureSet,

    -- * OrganizationalUnit
    OrganizationalUnit,
    organizationalUnit,
    ouARN,
    ouName,
    ouId,

    -- * Parent
    Parent,
    parent,
    pId,
    pType,

    -- * Policy
    Policy,
    policy,
    pContent,
    pPolicySummary,

    -- * PolicySummary
    PolicySummary,
    policySummary,
    psARN,
    psName,
    psId,
    psAWSManaged,
    psType,
    psDescription,

    -- * PolicyTargetSummary
    PolicyTargetSummary,
    policyTargetSummary,
    polTargetId,
    polARN,
    polName,
    polType,

    -- * PolicyTypeSummary
    PolicyTypeSummary,
    policyTypeSummary,
    ptsStatus,
    ptsType,

    -- * Root
    Root,
    root,
    rARN,
    rName,
    rId,
    rPolicyTypes,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.Account
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Organizations.Types.Child
import Network.AWS.Organizations.Types.ChildType
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountState
import Network.AWS.Organizations.Types.CreateAccountStatus
import Network.AWS.Organizations.Types.DelegatedAdministrator
import Network.AWS.Organizations.Types.DelegatedService
import Network.AWS.Organizations.Types.EffectivePolicy
import Network.AWS.Organizations.Types.EffectivePolicyType
import Network.AWS.Organizations.Types.EnabledServicePrincipal
import Network.AWS.Organizations.Types.Handshake
import Network.AWS.Organizations.Types.HandshakeFilter
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakePartyType
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.HandshakeResourceType
import Network.AWS.Organizations.Types.HandshakeState
import Network.AWS.Organizations.Types.IAMUserAccessToBilling
import Network.AWS.Organizations.Types.Organization
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.OrganizationalUnit
import Network.AWS.Organizations.Types.Parent
import Network.AWS.Organizations.Types.ParentType
import Network.AWS.Organizations.Types.Policy
import Network.AWS.Organizations.Types.PolicySummary
import Network.AWS.Organizations.Types.PolicyTargetSummary
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Organizations.Types.Root
import Network.AWS.Organizations.Types.Tag
import Network.AWS.Organizations.Types.TargetType
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
organizations :: Service
organizations =
  Service
    { _svcAbbrev = "Organizations",
      _svcSigner = v4,
      _svcPrefix = "organizations",
      _svcVersion = "2016-11-28",
      _svcEndpoint = defaultEndpoint organizations,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Organizations",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
