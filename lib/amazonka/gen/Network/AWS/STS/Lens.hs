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

    -- * Types

    -- ** AssumedRoleUser
    assumedRoleUser_assumedRoleId,
    assumedRoleUser_arn,

    -- ** PolicyDescriptorType
    policyDescriptorType_arn,
  )
where

import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.Types.AssumedRoleUser
import Network.AWS.STS.Types.PolicyDescriptorType
