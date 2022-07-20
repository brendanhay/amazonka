{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.STS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.STS.Lens
  ( -- * Operations

    -- ** AssumeRole
    assumeRole_tags,
    assumeRole_policy,
    assumeRole_transitiveTagKeys,
    assumeRole_policyArns,
    assumeRole_externalId,
    assumeRole_durationSeconds,
    assumeRole_tokenCode,
    assumeRole_serialNumber,
    assumeRole_sourceIdentity,
    assumeRole_roleArn,
    assumeRole_roleSessionName,
    assumeRoleResponse_assumedRoleUser,
    assumeRoleResponse_sourceIdentity,
    assumeRoleResponse_packedPolicySize,
    assumeRoleResponse_httpStatus,
    assumeRoleResponse_credentials,

    -- ** AssumeRoleWithSAML
    assumeRoleWithSAML_policy,
    assumeRoleWithSAML_policyArns,
    assumeRoleWithSAML_durationSeconds,
    assumeRoleWithSAML_roleArn,
    assumeRoleWithSAML_principalArn,
    assumeRoleWithSAML_sAMLAssertion,
    assumeRoleWithSAMLResponse_issuer,
    assumeRoleWithSAMLResponse_nameQualifier,
    assumeRoleWithSAMLResponse_assumedRoleUser,
    assumeRoleWithSAMLResponse_audience,
    assumeRoleWithSAMLResponse_credentials,
    assumeRoleWithSAMLResponse_sourceIdentity,
    assumeRoleWithSAMLResponse_packedPolicySize,
    assumeRoleWithSAMLResponse_subjectType,
    assumeRoleWithSAMLResponse_subject,
    assumeRoleWithSAMLResponse_httpStatus,

    -- ** AssumeRoleWithWebIdentity
    assumeRoleWithWebIdentity_policy,
    assumeRoleWithWebIdentity_policyArns,
    assumeRoleWithWebIdentity_durationSeconds,
    assumeRoleWithWebIdentity_providerId,
    assumeRoleWithWebIdentity_roleArn,
    assumeRoleWithWebIdentity_roleSessionName,
    assumeRoleWithWebIdentity_webIdentityToken,
    assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken,
    assumeRoleWithWebIdentityResponse_provider,
    assumeRoleWithWebIdentityResponse_assumedRoleUser,
    assumeRoleWithWebIdentityResponse_audience,
    assumeRoleWithWebIdentityResponse_sourceIdentity,
    assumeRoleWithWebIdentityResponse_packedPolicySize,
    assumeRoleWithWebIdentityResponse_httpStatus,
    assumeRoleWithWebIdentityResponse_credentials,

    -- ** DecodeAuthorizationMessage
    decodeAuthorizationMessage_encodedMessage,
    decodeAuthorizationMessageResponse_decodedMessage,
    decodeAuthorizationMessageResponse_httpStatus,

    -- ** GetAccessKeyInfo
    getAccessKeyInfo_accessKeyId,
    getAccessKeyInfoResponse_account,
    getAccessKeyInfoResponse_httpStatus,

    -- ** GetCallerIdentity
    getCallerIdentityResponse_arn,
    getCallerIdentityResponse_account,
    getCallerIdentityResponse_userId,
    getCallerIdentityResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_tags,
    getFederationToken_policy,
    getFederationToken_policyArns,
    getFederationToken_durationSeconds,
    getFederationToken_name,
    getFederationTokenResponse_federatedUser,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_packedPolicySize,
    getFederationTokenResponse_httpStatus,

    -- ** GetSessionToken
    getSessionToken_durationSeconds,
    getSessionToken_tokenCode,
    getSessionToken_serialNumber,
    getSessionTokenResponse_credentials,
    getSessionTokenResponse_httpStatus,

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

import Amazonka.STS.AssumeRole
import Amazonka.STS.AssumeRoleWithSAML
import Amazonka.STS.AssumeRoleWithWebIdentity
import Amazonka.STS.DecodeAuthorizationMessage
import Amazonka.STS.GetAccessKeyInfo
import Amazonka.STS.GetCallerIdentity
import Amazonka.STS.GetFederationToken
import Amazonka.STS.GetSessionToken
import Amazonka.STS.Types.AssumedRoleUser
import Amazonka.STS.Types.FederatedUser
import Amazonka.STS.Types.PolicyDescriptorType
import Amazonka.STS.Types.Tag
