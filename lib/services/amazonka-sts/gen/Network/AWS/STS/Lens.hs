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

    -- ** GetCallerIdentity
    getCallerIdentityResponse_arn,
    getCallerIdentityResponse_account,
    getCallerIdentityResponse_userId,
    getCallerIdentityResponse_httpStatus,

    -- ** AssumeRole
    assumeRole_transitiveTagKeys,
    assumeRole_tokenCode,
    assumeRole_policyArns,
    assumeRole_durationSeconds,
    assumeRole_policy,
    assumeRole_externalId,
    assumeRole_sourceIdentity,
    assumeRole_serialNumber,
    assumeRole_tags,
    assumeRole_roleArn,
    assumeRole_roleSessionName,
    assumeRoleResponse_packedPolicySize,
    assumeRoleResponse_credentials,
    assumeRoleResponse_assumedRoleUser,
    assumeRoleResponse_sourceIdentity,
    assumeRoleResponse_httpStatus,

    -- ** GetAccessKeyInfo
    getAccessKeyInfo_accessKeyId,
    getAccessKeyInfoResponse_account,
    getAccessKeyInfoResponse_httpStatus,

    -- ** DecodeAuthorizationMessage
    decodeAuthorizationMessage_encodedMessage,
    decodeAuthorizationMessageResponse_decodedMessage,
    decodeAuthorizationMessageResponse_httpStatus,

    -- ** AssumeRoleWithWebIdentity
    assumeRoleWithWebIdentity_providerId,
    assumeRoleWithWebIdentity_policyArns,
    assumeRoleWithWebIdentity_durationSeconds,
    assumeRoleWithWebIdentity_policy,
    assumeRoleWithWebIdentity_roleArn,
    assumeRoleWithWebIdentity_roleSessionName,
    assumeRoleWithWebIdentity_webIdentityToken,
    assumeRoleWithWebIdentityResponse_audience,
    assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken,
    assumeRoleWithWebIdentityResponse_packedPolicySize,
    assumeRoleWithWebIdentityResponse_credentials,
    assumeRoleWithWebIdentityResponse_assumedRoleUser,
    assumeRoleWithWebIdentityResponse_sourceIdentity,
    assumeRoleWithWebIdentityResponse_provider,
    assumeRoleWithWebIdentityResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_policyArns,
    getFederationToken_durationSeconds,
    getFederationToken_policy,
    getFederationToken_tags,
    getFederationToken_name,
    getFederationTokenResponse_packedPolicySize,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_federatedUser,
    getFederationTokenResponse_httpStatus,

    -- ** GetSessionToken
    getSessionToken_tokenCode,
    getSessionToken_durationSeconds,
    getSessionToken_serialNumber,
    getSessionTokenResponse_credentials,
    getSessionTokenResponse_httpStatus,

    -- ** AssumeRoleWithSAML
    assumeRoleWithSAML_policyArns,
    assumeRoleWithSAML_durationSeconds,
    assumeRoleWithSAML_policy,
    assumeRoleWithSAML_roleArn,
    assumeRoleWithSAML_principalArn,
    assumeRoleWithSAML_sAMLAssertion,
    assumeRoleWithSAMLResponse_subject,
    assumeRoleWithSAMLResponse_audience,
    assumeRoleWithSAMLResponse_packedPolicySize,
    assumeRoleWithSAMLResponse_credentials,
    assumeRoleWithSAMLResponse_subjectType,
    assumeRoleWithSAMLResponse_nameQualifier,
    assumeRoleWithSAMLResponse_assumedRoleUser,
    assumeRoleWithSAMLResponse_sourceIdentity,
    assumeRoleWithSAMLResponse_issuer,
    assumeRoleWithSAMLResponse_httpStatus,

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
