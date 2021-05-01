{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Lens
  ( -- * Operations

    -- ** DescribeIdentityPool
    describeIdentityPool_identityPoolId,
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** GetOpenIdTokenForDeveloperIdentity
    getOpenIdTokenForDeveloperIdentity_tokenDuration,
    getOpenIdTokenForDeveloperIdentity_identityId,
    getOpenIdTokenForDeveloperIdentity_principalTags,
    getOpenIdTokenForDeveloperIdentity_identityPoolId,
    getOpenIdTokenForDeveloperIdentity_logins,
    getOpenIdTokenForDeveloperIdentityResponse_identityId,
    getOpenIdTokenForDeveloperIdentityResponse_token,
    getOpenIdTokenForDeveloperIdentityResponse_httpStatus,

    -- ** GetOpenIdToken
    getOpenIdToken_logins,
    getOpenIdToken_identityId,
    getOpenIdTokenResponse_identityId,
    getOpenIdTokenResponse_token,
    getOpenIdTokenResponse_httpStatus,

    -- ** DeleteIdentities
    deleteIdentities_identityIdsToDelete,
    deleteIdentitiesResponse_unprocessedIdentityIds,
    deleteIdentitiesResponse_httpStatus,

    -- ** MergeDeveloperIdentities
    mergeDeveloperIdentities_sourceUserIdentifier,
    mergeDeveloperIdentities_destinationUserIdentifier,
    mergeDeveloperIdentities_developerProviderName,
    mergeDeveloperIdentities_identityPoolId,
    mergeDeveloperIdentitiesResponse_identityId,
    mergeDeveloperIdentitiesResponse_httpStatus,

    -- ** CreateIdentityPool
    createIdentityPool_allowClassicFlow,
    createIdentityPool_samlProviderARNs,
    createIdentityPool_identityPoolTags,
    createIdentityPool_openIdConnectProviderARNs,
    createIdentityPool_supportedLoginProviders,
    createIdentityPool_cognitoIdentityProviders,
    createIdentityPool_developerProviderName,
    createIdentityPool_identityPoolName,
    createIdentityPool_allowUnauthenticatedIdentities,
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** GetPrincipalTagAttributeMap
    getPrincipalTagAttributeMap_identityPoolId,
    getPrincipalTagAttributeMap_identityProviderName,
    getPrincipalTagAttributeMapResponse_identityPoolId,
    getPrincipalTagAttributeMapResponse_identityProviderName,
    getPrincipalTagAttributeMapResponse_principalTags,
    getPrincipalTagAttributeMapResponse_useDefaults,
    getPrincipalTagAttributeMapResponse_httpStatus,

    -- ** UpdateIdentityPool
    updateIdentityPool_allowClassicFlow,
    updateIdentityPool_samlProviderARNs,
    updateIdentityPool_identityPoolTags,
    updateIdentityPool_openIdConnectProviderARNs,
    updateIdentityPool_supportedLoginProviders,
    updateIdentityPool_cognitoIdentityProviders,
    updateIdentityPool_developerProviderName,
    updateIdentityPool_identityPoolId,
    updateIdentityPool_identityPoolName,
    updateIdentityPool_allowUnauthenticatedIdentities,
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteIdentityPool
    deleteIdentityPool_identityPoolId,

    -- ** GetIdentityPoolRoles
    getIdentityPoolRoles_identityPoolId,
    getIdentityPoolRolesResponse_identityPoolId,
    getIdentityPoolRolesResponse_roles,
    getIdentityPoolRolesResponse_roleMappings,
    getIdentityPoolRolesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UnlinkIdentity
    unlinkIdentity_identityId,
    unlinkIdentity_logins,
    unlinkIdentity_loginsToRemove,

    -- ** LookupDeveloperIdentity
    lookupDeveloperIdentity_nextToken,
    lookupDeveloperIdentity_maxResults,
    lookupDeveloperIdentity_developerUserIdentifier,
    lookupDeveloperIdentity_identityId,
    lookupDeveloperIdentity_identityPoolId,
    lookupDeveloperIdentityResponse_nextToken,
    lookupDeveloperIdentityResponse_developerUserIdentifierList,
    lookupDeveloperIdentityResponse_identityId,
    lookupDeveloperIdentityResponse_httpStatus,

    -- ** SetIdentityPoolRoles
    setIdentityPoolRoles_roleMappings,
    setIdentityPoolRoles_identityPoolId,
    setIdentityPoolRoles_roles,

    -- ** ListIdentityPools
    listIdentityPools_nextToken,
    listIdentityPools_maxResults,
    listIdentityPoolsResponse_nextToken,
    listIdentityPoolsResponse_identityPools,
    listIdentityPoolsResponse_httpStatus,

    -- ** DescribeIdentity
    describeIdentity_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_logins,

    -- ** GetCredentialsForIdentity
    getCredentialsForIdentity_logins,
    getCredentialsForIdentity_customRoleArn,
    getCredentialsForIdentity_identityId,
    getCredentialsForIdentityResponse_identityId,
    getCredentialsForIdentityResponse_credentials,
    getCredentialsForIdentityResponse_httpStatus,

    -- ** UnlinkDeveloperIdentity
    unlinkDeveloperIdentity_identityId,
    unlinkDeveloperIdentity_identityPoolId,
    unlinkDeveloperIdentity_developerProviderName,
    unlinkDeveloperIdentity_developerUserIdentifier,

    -- ** GetId
    getId_accountId,
    getId_logins,
    getId_identityPoolId,
    getIdResponse_identityId,
    getIdResponse_httpStatus,

    -- ** ListIdentities
    listIdentities_nextToken,
    listIdentities_hideDisabled,
    listIdentities_identityPoolId,
    listIdentities_maxResults,
    listIdentitiesResponse_identityPoolId,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_identities,
    listIdentitiesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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

    -- * Types

    -- ** CognitoIdentityProvider
    cognitoIdentityProvider_clientId,
    cognitoIdentityProvider_providerName,
    cognitoIdentityProvider_serverSideTokenCheck,

    -- ** Credentials
    credentials_expiration,
    credentials_secretKey,
    credentials_accessKeyId,
    credentials_sessionToken,

    -- ** IdentityDescription
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_logins,

    -- ** IdentityPool
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
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
    unprocessedIdentityId_identityId,
    unprocessedIdentityId_errorCode,
  )
where

import Network.AWS.CognitoIdentity.CreateIdentityPool
import Network.AWS.CognitoIdentity.DeleteIdentities
import Network.AWS.CognitoIdentity.DeleteIdentityPool
import Network.AWS.CognitoIdentity.DescribeIdentity
import Network.AWS.CognitoIdentity.DescribeIdentityPool
import Network.AWS.CognitoIdentity.GetCredentialsForIdentity
import Network.AWS.CognitoIdentity.GetId
import Network.AWS.CognitoIdentity.GetIdentityPoolRoles
import Network.AWS.CognitoIdentity.GetOpenIdToken
import Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
import Network.AWS.CognitoIdentity.GetPrincipalTagAttributeMap
import Network.AWS.CognitoIdentity.ListIdentities
import Network.AWS.CognitoIdentity.ListIdentityPools
import Network.AWS.CognitoIdentity.ListTagsForResource
import Network.AWS.CognitoIdentity.LookupDeveloperIdentity
import Network.AWS.CognitoIdentity.MergeDeveloperIdentities
import Network.AWS.CognitoIdentity.SetIdentityPoolRoles
import Network.AWS.CognitoIdentity.SetPrincipalTagAttributeMap
import Network.AWS.CognitoIdentity.TagResource
import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import Network.AWS.CognitoIdentity.Types.Credentials
import Network.AWS.CognitoIdentity.Types.IdentityDescription
import Network.AWS.CognitoIdentity.Types.IdentityPool
import Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
import Network.AWS.CognitoIdentity.Types.MappingRule
import Network.AWS.CognitoIdentity.Types.RoleMapping
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
import Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
import Network.AWS.CognitoIdentity.UnlinkIdentity
import Network.AWS.CognitoIdentity.UntagResource
import Network.AWS.CognitoIdentity.UpdateIdentityPool
