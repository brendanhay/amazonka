{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentity.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Lens
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
