{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Lens
  ( -- * Operations

    -- ** AssumeRoleWithWebIdentity
    assumeRoleWithWebIdentity_providerId,
    assumeRoleWithWebIdentity_policyArns,
    assumeRoleWithWebIdentity_policy,
    assumeRoleWithWebIdentity_durationSeconds,
    assumeRoleWithWebIdentity_roleArn,
    assumeRoleWithWebIdentity_roleSessionName,
    assumeRoleWithWebIdentity_webIdentityToken,
    assumeRoleWithWebIdentityResponse_audience,
    assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken,
    assumeRoleWithWebIdentityResponse_provider,
    assumeRoleWithWebIdentityResponse_credentials,
    assumeRoleWithWebIdentityResponse_assumedRoleUser,
    assumeRoleWithWebIdentityResponse_packedPolicySize,
    assumeRoleWithWebIdentityResponse_httpStatus,

    -- ** GetAccessKeyInfo
    getAccessKeyInfo_accessKeyId,
    getAccessKeyInfoResponse_account,
    getAccessKeyInfoResponse_httpStatus,

    -- ** GetSessionToken
    getSessionToken_tokenCode,
    getSessionToken_serialNumber,
    getSessionToken_durationSeconds,
    getSessionTokenResponse_credentials,
    getSessionTokenResponse_httpStatus,

    -- ** AssumeRole
    assumeRole_tokenCode,
    assumeRole_tags,
    assumeRole_policyArns,
    assumeRole_transitiveTagKeys,
    assumeRole_serialNumber,
    assumeRole_policy,
    assumeRole_externalId,
    assumeRole_durationSeconds,
    assumeRole_roleArn,
    assumeRole_roleSessionName,
    assumeRoleResponse_credentials,
    assumeRoleResponse_assumedRoleUser,
    assumeRoleResponse_packedPolicySize,
    assumeRoleResponse_httpStatus,

    -- ** GetCallerIdentity
    getCallerIdentityResponse_arn,
    getCallerIdentityResponse_userId,
    getCallerIdentityResponse_account,
    getCallerIdentityResponse_httpStatus,

    -- ** DecodeAuthorizationMessage
    decodeAuthorizationMessage_encodedMessage,
    decodeAuthorizationMessageResponse_decodedMessage,
    decodeAuthorizationMessageResponse_httpStatus,

    -- ** AssumeRoleWithSAML
    assumeRoleWithSAML_policyArns,
    assumeRoleWithSAML_policy,
    assumeRoleWithSAML_durationSeconds,
    assumeRoleWithSAML_roleArn,
    assumeRoleWithSAML_principalArn,
    assumeRoleWithSAML_sAMLAssertion,
    assumeRoleWithSAMLResponse_nameQualifier,
    assumeRoleWithSAMLResponse_audience,
    assumeRoleWithSAMLResponse_subjectType,
    assumeRoleWithSAMLResponse_subject,
    assumeRoleWithSAMLResponse_issuer,
    assumeRoleWithSAMLResponse_credentials,
    assumeRoleWithSAMLResponse_assumedRoleUser,
    assumeRoleWithSAMLResponse_packedPolicySize,
    assumeRoleWithSAMLResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_tags,
    getFederationToken_policyArns,
    getFederationToken_policy,
    getFederationToken_durationSeconds,
    getFederationToken_name,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_federatedUser,
    getFederationTokenResponse_packedPolicySize,
    getFederationTokenResponse_httpStatus,

    -- * Types

    -- ** AssumedRoleUser
    assumedRoleUser_assumedRoleId,
    assumedRoleUser_arn,

    -- ** FederatedUser
    federatedUser_federatedUserId,
    federatedUser_arn,

    -- ** PolicyDescriptorType
    policyDescriptorType_arn,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetAccessKeyInfo
import Network.AWS.STS.GetCallerIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.Types.AssumedRoleUser
import Network.AWS.STS.Types.FederatedUser
import Network.AWS.STS.Types.PolicyDescriptorType
import Network.AWS.STS.Types.Tag
