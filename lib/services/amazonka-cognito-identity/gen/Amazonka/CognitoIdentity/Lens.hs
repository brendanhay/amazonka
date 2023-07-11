{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentity.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Lens
  ( -- * Operations

    -- ** CreateIdentityPool
    createIdentityPool_allowClassicFlow,
    createIdentityPool_cognitoIdentityProviders,
    createIdentityPool_developerProviderName,
    createIdentityPool_identityPoolTags,
    createIdentityPool_openIdConnectProviderARNs,
    createIdentityPool_samlProviderARNs,
    createIdentityPool_supportedLoginProviders,
    createIdentityPool_identityPoolName,
    createIdentityPool_allowUnauthenticatedIdentities,
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** DeleteIdentities
    deleteIdentities_identityIdsToDelete,
    deleteIdentitiesResponse_unprocessedIdentityIds,
    deleteIdentitiesResponse_httpStatus,

    -- ** DeleteIdentityPool
    deleteIdentityPool_identityPoolId,

    -- ** DescribeIdentity
    describeIdentity_identityId,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_logins,

    -- ** DescribeIdentityPool
    describeIdentityPool_identityPoolId,
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** GetCredentialsForIdentity
    getCredentialsForIdentity_customRoleArn,
    getCredentialsForIdentity_logins,
    getCredentialsForIdentity_identityId,
    getCredentialsForIdentityResponse_credentials,
    getCredentialsForIdentityResponse_identityId,
    getCredentialsForIdentityResponse_httpStatus,

    -- ** GetId
    getId_accountId,
    getId_logins,
    getId_identityPoolId,
    getIdResponse_identityId,
    getIdResponse_httpStatus,

    -- ** GetIdentityPoolRoles
    getIdentityPoolRoles_identityPoolId,
    getIdentityPoolRolesResponse_identityPoolId,
    getIdentityPoolRolesResponse_roleMappings,
    getIdentityPoolRolesResponse_roles,
    getIdentityPoolRolesResponse_httpStatus,

    -- ** GetOpenIdToken
    getOpenIdToken_logins,
    getOpenIdToken_identityId,
    getOpenIdTokenResponse_identityId,
    getOpenIdTokenResponse_token,
    getOpenIdTokenResponse_httpStatus,

    -- ** GetOpenIdTokenForDeveloperIdentity
    getOpenIdTokenForDeveloperIdentity_identityId,
    getOpenIdTokenForDeveloperIdentity_principalTags,
    getOpenIdTokenForDeveloperIdentity_tokenDuration,
    getOpenIdTokenForDeveloperIdentity_identityPoolId,
    getOpenIdTokenForDeveloperIdentity_logins,
    getOpenIdTokenForDeveloperIdentityResponse_identityId,
    getOpenIdTokenForDeveloperIdentityResponse_token,
    getOpenIdTokenForDeveloperIdentityResponse_httpStatus,

    -- ** GetPrincipalTagAttributeMap
    getPrincipalTagAttributeMap_identityPoolId,
    getPrincipalTagAttributeMap_identityProviderName,
    getPrincipalTagAttributeMapResponse_identityPoolId,
    getPrincipalTagAttributeMapResponse_identityProviderName,
    getPrincipalTagAttributeMapResponse_principalTags,
    getPrincipalTagAttributeMapResponse_useDefaults,
    getPrincipalTagAttributeMapResponse_httpStatus,

    -- ** ListIdentities
    listIdentities_hideDisabled,
    listIdentities_nextToken,
    listIdentities_identityPoolId,
    listIdentities_maxResults,
    listIdentitiesResponse_identities,
    listIdentitiesResponse_identityPoolId,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_httpStatus,

    -- ** ListIdentityPools
    listIdentityPools_nextToken,
    listIdentityPools_maxResults,
    listIdentityPoolsResponse_identityPools,
    listIdentityPoolsResponse_nextToken,
    listIdentityPoolsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** LookupDeveloperIdentity
    lookupDeveloperIdentity_developerUserIdentifier,
    lookupDeveloperIdentity_identityId,
    lookupDeveloperIdentity_maxResults,
    lookupDeveloperIdentity_nextToken,
    lookupDeveloperIdentity_identityPoolId,
    lookupDeveloperIdentityResponse_developerUserIdentifierList,
    lookupDeveloperIdentityResponse_identityId,
    lookupDeveloperIdentityResponse_nextToken,
    lookupDeveloperIdentityResponse_httpStatus,

    -- ** MergeDeveloperIdentities
    mergeDeveloperIdentities_sourceUserIdentifier,
    mergeDeveloperIdentities_destinationUserIdentifier,
    mergeDeveloperIdentities_developerProviderName,
    mergeDeveloperIdentities_identityPoolId,
    mergeDeveloperIdentitiesResponse_identityId,
    mergeDeveloperIdentitiesResponse_httpStatus,

    -- ** SetIdentityPoolRoles
    setIdentityPoolRoles_roleMappings,
    setIdentityPoolRoles_identityPoolId,
    setIdentityPoolRoles_roles,

    -- ** SetPrincipalTagAttributeMap
    setPrincipalTagAttributeMap_principalTags,
    setPrincipalTagAttributeMap_useDefaults,
    setPrincipalTagAttributeMap_identityPoolId,
    setPrincipalTagAttributeMap_identityProviderName,
    setPrincipalTagAttributeMapResponse_identityPoolId,
    setPrincipalTagAttributeMapResponse_identityProviderName,
    setPrincipalTagAttributeMapResponse_principalTags,
    setPrincipalTagAttributeMapResponse_useDefaults,
    setPrincipalTagAttributeMapResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UnlinkDeveloperIdentity
    unlinkDeveloperIdentity_identityId,
    unlinkDeveloperIdentity_identityPoolId,
    unlinkDeveloperIdentity_developerProviderName,
    unlinkDeveloperIdentity_developerUserIdentifier,

    -- ** UnlinkIdentity
    unlinkIdentity_identityId,
    unlinkIdentity_logins,
    unlinkIdentity_loginsToRemove,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateIdentityPool
    updateIdentityPool_allowClassicFlow,
    updateIdentityPool_cognitoIdentityProviders,
    updateIdentityPool_developerProviderName,
    updateIdentityPool_identityPoolTags,
    updateIdentityPool_openIdConnectProviderARNs,
    updateIdentityPool_samlProviderARNs,
    updateIdentityPool_supportedLoginProviders,
    updateIdentityPool_identityPoolId,
    updateIdentityPool_identityPoolName,
    updateIdentityPool_allowUnauthenticatedIdentities,
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- * Types

    -- ** CognitoIdentityProvider
    cognitoIdentityProvider_clientId,
    cognitoIdentityProvider_providerName,
    cognitoIdentityProvider_serverSideTokenCheck,

    -- ** Credentials
    credentials_accessKeyId,
    credentials_expiration,
    credentials_secretKey,
    credentials_sessionToken,

    -- ** IdentityDescription
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_logins,

    -- ** IdentityPool
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** IdentityPoolShortDescription
    identityPoolShortDescription_identityPoolId,
    identityPoolShortDescription_identityPoolName,

    -- ** MappingRule
    mappingRule_claim,
    mappingRule_matchType,
    mappingRule_value,
    mappingRule_roleARN,

    -- ** RoleMapping
    roleMapping_ambiguousRoleResolution,
    roleMapping_rulesConfiguration,
    roleMapping_type,

    -- ** RulesConfigurationType
    rulesConfigurationType_rules,

    -- ** UnprocessedIdentityId
    unprocessedIdentityId_errorCode,
    unprocessedIdentityId_identityId,
  )
where

import Amazonka.CognitoIdentity.CreateIdentityPool
import Amazonka.CognitoIdentity.DeleteIdentities
import Amazonka.CognitoIdentity.DeleteIdentityPool
import Amazonka.CognitoIdentity.DescribeIdentity
import Amazonka.CognitoIdentity.DescribeIdentityPool
import Amazonka.CognitoIdentity.GetCredentialsForIdentity
import Amazonka.CognitoIdentity.GetId
import Amazonka.CognitoIdentity.GetIdentityPoolRoles
import Amazonka.CognitoIdentity.GetOpenIdToken
import Amazonka.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
import Amazonka.CognitoIdentity.GetPrincipalTagAttributeMap
import Amazonka.CognitoIdentity.ListIdentities
import Amazonka.CognitoIdentity.ListIdentityPools
import Amazonka.CognitoIdentity.ListTagsForResource
import Amazonka.CognitoIdentity.LookupDeveloperIdentity
import Amazonka.CognitoIdentity.MergeDeveloperIdentities
import Amazonka.CognitoIdentity.SetIdentityPoolRoles
import Amazonka.CognitoIdentity.SetPrincipalTagAttributeMap
import Amazonka.CognitoIdentity.TagResource
import Amazonka.CognitoIdentity.Types.CognitoIdentityProvider
import Amazonka.CognitoIdentity.Types.Credentials
import Amazonka.CognitoIdentity.Types.IdentityDescription
import Amazonka.CognitoIdentity.Types.IdentityPool
import Amazonka.CognitoIdentity.Types.IdentityPoolShortDescription
import Amazonka.CognitoIdentity.Types.MappingRule
import Amazonka.CognitoIdentity.Types.RoleMapping
import Amazonka.CognitoIdentity.Types.RulesConfigurationType
import Amazonka.CognitoIdentity.Types.UnprocessedIdentityId
import Amazonka.CognitoIdentity.UnlinkDeveloperIdentity
import Amazonka.CognitoIdentity.UnlinkIdentity
import Amazonka.CognitoIdentity.UntagResource
import Amazonka.CognitoIdentity.UpdateIdentityPool
