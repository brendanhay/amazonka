{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FinSpace.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FinSpace.Lens
  ( -- * Operations

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_environments,
    listEnvironmentsResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_federationParameters,
    updateEnvironment_federationMode,
    updateEnvironment_name,
    updateEnvironment_description,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_environment,
    updateEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentId,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_federationParameters,
    createEnvironment_federationMode,
    createEnvironment_kmsKeyId,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_httpStatus,

    -- * Types

    -- ** Environment
    environment_status,
    environment_federationParameters,
    environment_dedicatedServiceAccountId,
    environment_environmentUrl,
    environment_federationMode,
    environment_awsAccountId,
    environment_name,
    environment_kmsKeyId,
    environment_environmentId,
    environment_environmentArn,
    environment_sageMakerStudioDomainUrl,
    environment_description,

    -- ** FederationParameters
    federationParameters_samlMetadataURL,
    federationParameters_applicationCallBackURL,
    federationParameters_federationURN,
    federationParameters_attributeMap,
    federationParameters_federationProviderName,
    federationParameters_samlMetadataDocument,
  )
where

import Network.AWS.FinSpace.CreateEnvironment
import Network.AWS.FinSpace.DeleteEnvironment
import Network.AWS.FinSpace.GetEnvironment
import Network.AWS.FinSpace.ListEnvironments
import Network.AWS.FinSpace.ListTagsForResource
import Network.AWS.FinSpace.TagResource
import Network.AWS.FinSpace.Types.Environment
import Network.AWS.FinSpace.Types.FederationParameters
import Network.AWS.FinSpace.UntagResource
import Network.AWS.FinSpace.UpdateEnvironment
