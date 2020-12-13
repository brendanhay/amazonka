-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types
  ( -- * Service configuration
    organizationsService,

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
    Account (..),
    mkAccount,
    aStatus,
    aJoinedMethod,
    aEmail,
    aARN,
    aJoinedTimestamp,
    aName,
    aId,

    -- * Child
    Child (..),
    mkChild,
    cId,
    cType,

    -- * CreateAccountStatus
    CreateAccountStatus (..),
    mkCreateAccountStatus,
    casFailureReason,
    casState,
    casCompletedTimestamp,
    casAccountName,
    casAccountId,
    casId,
    casGovCloudAccountId,
    casRequestedTimestamp,

    -- * DelegatedAdministrator
    DelegatedAdministrator (..),
    mkDelegatedAdministrator,
    daStatus,
    daJoinedMethod,
    daEmail,
    daARN,
    daJoinedTimestamp,
    daDelegationEnabledDate,
    daName,
    daId,

    -- * DelegatedService
    DelegatedService (..),
    mkDelegatedService,
    dsServicePrincipal,
    dsDelegationEnabledDate,

    -- * EffectivePolicy
    EffectivePolicy (..),
    mkEffectivePolicy,
    epTargetId,
    epPolicyType,
    epLastUpdatedTimestamp,
    epPolicyContent,

    -- * EnabledServicePrincipal
    EnabledServicePrincipal (..),
    mkEnabledServicePrincipal,
    espServicePrincipal,
    espDateEnabled,

    -- * Handshake
    Handshake (..),
    mkHandshake,
    hState,
    hARN,
    hAction,
    hResources,
    hId,
    hExpirationTimestamp,
    hParties,
    hRequestedTimestamp,

    -- * HandshakeFilter
    HandshakeFilter (..),
    mkHandshakeFilter,
    hfParentHandshakeId,
    hfActionType,

    -- * HandshakeParty
    HandshakeParty (..),
    mkHandshakeParty,
    hpId,
    hpType,

    -- * HandshakeResource
    HandshakeResource (..),
    mkHandshakeResource,
    hrValue,
    hrResources,
    hrType,

    -- * Organization
    Organization (..),
    mkOrganization,
    oARN,
    oMasterAccountId,
    oMasterAccountARN,
    oMasterAccountEmail,
    oAvailablePolicyTypes,
    oId,
    oFeatureSet,

    -- * OrganizationalUnit
    OrganizationalUnit (..),
    mkOrganizationalUnit,
    ouARN,
    ouName,
    ouId,

    -- * Parent
    Parent (..),
    mkParent,
    pId,
    pType,

    -- * Policy
    Policy (..),
    mkPolicy,
    pContent,
    pPolicySummary,

    -- * PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psARN,
    psName,
    psId,
    psAWSManaged,
    psType,
    psDescription,

    -- * PolicyTargetSummary
    PolicyTargetSummary (..),
    mkPolicyTargetSummary,
    ptsfTargetId,
    ptsfARN,
    ptsfName,
    ptsfType,

    -- * PolicyTypeSummary
    PolicyTypeSummary (..),
    mkPolicyTypeSummary,
    ptsStatus,
    ptsType,

    -- * Root
    Root (..),
    mkRoot,
    rARN,
    rName,
    rId,
    rPolicyTypes,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

import qualified Network.AWS.Lens as Lens
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
organizationsService :: Lude.Service
organizationsService =
  Lude.Service
    { Lude._svcAbbrev = "Organizations",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "organizations",
      Lude._svcVersion = "2016-11-28",
      Lude._svcEndpoint = Lude.defaultEndpoint organizationsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Organizations",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
