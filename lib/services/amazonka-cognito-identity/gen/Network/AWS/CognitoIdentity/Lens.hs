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

    -- ** GetOpenIdToken
    getOpenIdToken_logins,
    getOpenIdToken_identityId,
    getOpenIdTokenResponse_token,
    getOpenIdTokenResponse_identityId,
    getOpenIdTokenResponse_httpStatus,

    -- ** GetOpenIdTokenForDeveloperIdentity
    getOpenIdTokenForDeveloperIdentity_tokenDuration,
    getOpenIdTokenForDeveloperIdentity_principalTags,
    getOpenIdTokenForDeveloperIdentity_identityId,
    getOpenIdTokenForDeveloperIdentity_identityPoolId,
    getOpenIdTokenForDeveloperIdentity_logins,
    getOpenIdTokenForDeveloperIdentityResponse_token,
    getOpenIdTokenForDeveloperIdentityResponse_identityId,
    getOpenIdTokenForDeveloperIdentityResponse_httpStatus,

    -- ** DescribeIdentityPool
    describeIdentityPool_identityPoolId,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_allowClassicFlow,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_cognitoIdentityProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

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

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetId
    getId_accountId,
    getId_logins,
    getId_identityPoolId,
    getIdResponse_identityId,
    getIdResponse_httpStatus,

    -- ** DeleteIdentityPool
    deleteIdentityPool_identityPoolId,

    -- ** UpdateIdentityPool
    updateIdentityPool_samlProviderARNs,
    updateIdentityPool_supportedLoginProviders,
    updateIdentityPool_allowClassicFlow,
    updateIdentityPool_developerProviderName,
    updateIdentityPool_identityPoolTags,
    updateIdentityPool_openIdConnectProviderARNs,
    updateIdentityPool_cognitoIdentityProviders,
    updateIdentityPool_identityPoolId,
    updateIdentityPool_identityPoolName,
    updateIdentityPool_allowUnauthenticatedIdentities,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_allowClassicFlow,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_cognitoIdentityProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** UnlinkDeveloperIdentity
    unlinkDeveloperIdentity_identityId,
    unlinkDeveloperIdentity_identityPoolId,
    unlinkDeveloperIdentity_developerProviderName,
    unlinkDeveloperIdentity_developerUserIdentifier,

    -- ** GetIdentityPoolRoles
    getIdentityPoolRoles_identityPoolId,
    getIdentityPoolRolesResponse_roles,
    getIdentityPoolRolesResponse_identityPoolId,
    getIdentityPoolRolesResponse_roleMappings,
    getIdentityPoolRolesResponse_httpStatus,

    -- ** ListIdentityPools
    listIdentityPools_nextToken,
    listIdentityPools_maxResults,
    listIdentityPoolsResponse_identityPools,
    listIdentityPoolsResponse_nextToken,
    listIdentityPoolsResponse_httpStatus,

    -- ** GetCredentialsForIdentity
    getCredentialsForIdentity_customRoleArn,
    getCredentialsForIdentity_logins,
    getCredentialsForIdentity_identityId,
    getCredentialsForIdentityResponse_credentials,
    getCredentialsForIdentityResponse_identityId,
    getCredentialsForIdentityResponse_httpStatus,

    -- ** GetPrincipalTagAttributeMap
    getPrincipalTagAttributeMap_identityPoolId,
    getPrincipalTagAttributeMap_identityProviderName,
    getPrincipalTagAttributeMapResponse_identityPoolId,
    getPrincipalTagAttributeMapResponse_identityProviderName,
    getPrincipalTagAttributeMapResponse_principalTags,
    getPrincipalTagAttributeMapResponse_useDefaults,
    getPrincipalTagAttributeMapResponse_httpStatus,

    -- ** DeleteIdentities
    deleteIdentities_identityIdsToDelete,
    deleteIdentitiesResponse_unprocessedIdentityIds,
    deleteIdentitiesResponse_httpStatus,

    -- ** SetIdentityPoolRoles
    setIdentityPoolRoles_roleMappings,
    setIdentityPoolRoles_identityPoolId,
    setIdentityPoolRoles_roles,

    -- ** ListIdentities
    listIdentities_hideDisabled,
    listIdentities_nextToken,
    listIdentities_identityPoolId,
    listIdentities_maxResults,
    listIdentitiesResponse_identityPoolId,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_identities,
    listIdentitiesResponse_httpStatus,

    -- ** LookupDeveloperIdentity
    lookupDeveloperIdentity_developerUserIdentifier,
    lookupDeveloperIdentity_nextToken,
    lookupDeveloperIdentity_identityId,
    lookupDeveloperIdentity_maxResults,
    lookupDeveloperIdentity_identityPoolId,
    lookupDeveloperIdentityResponse_nextToken,
    lookupDeveloperIdentityResponse_identityId,
    lookupDeveloperIdentityResponse_developerUserIdentifierList,
    lookupDeveloperIdentityResponse_httpStatus,

    -- ** UnlinkIdentity
    unlinkIdentity_identityId,
    unlinkIdentity_logins,
    unlinkIdentity_loginsToRemove,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeIdentity
    describeIdentity_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_logins,
    identityDescription_identityId,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateIdentityPool
    createIdentityPool_samlProviderARNs,
    createIdentityPool_supportedLoginProviders,
    createIdentityPool_allowClassicFlow,
    createIdentityPool_developerProviderName,
    createIdentityPool_identityPoolTags,
    createIdentityPool_openIdConnectProviderARNs,
    createIdentityPool_cognitoIdentityProviders,
    createIdentityPool_identityPoolName,
    createIdentityPool_allowUnauthenticatedIdentities,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_allowClassicFlow,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_cognitoIdentityProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- ** MergeDeveloperIdentities
    mergeDeveloperIdentities_sourceUserIdentifier,
    mergeDeveloperIdentities_destinationUserIdentifier,
    mergeDeveloperIdentities_developerProviderName,
    mergeDeveloperIdentities_identityPoolId,
    mergeDeveloperIdentitiesResponse_identityId,
    mergeDeveloperIdentitiesResponse_httpStatus,

    -- * Types

    -- ** CognitoIdentityProvider
    cognitoIdentityProvider_clientId,
    cognitoIdentityProvider_serverSideTokenCheck,
    cognitoIdentityProvider_providerName,

    -- ** Credentials
    credentials_sessionToken,
    credentials_expiration,
    credentials_secretKey,
    credentials_accessKeyId,

    -- ** IdentityDescription
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_logins,
    identityDescription_identityId,

    -- ** IdentityPool
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_allowClassicFlow,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_cognitoIdentityProviders,
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
    roleMapping_rulesConfiguration,
    roleMapping_ambiguousRoleResolution,
    roleMapping_type,

    -- ** RulesConfigurationType
    rulesConfigurationType_rules,

    -- ** UnprocessedIdentityId
    unprocessedIdentityId_errorCode,
    unprocessedIdentityId_identityId,
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
