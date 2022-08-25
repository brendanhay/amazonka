{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Lens
  ( -- * Operations

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_dataBundles,
    createEnvironment_superuserParameters,
    createEnvironment_federationParameters,
    createEnvironment_description,
    createEnvironment_federationMode,
    createEnvironment_kmsKeyId,
    createEnvironment_name,
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentId,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_environments,
    listEnvironmentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_name,
    updateEnvironment_federationParameters,
    updateEnvironment_description,
    updateEnvironment_federationMode,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_environment,
    updateEnvironmentResponse_httpStatus,

    -- * Types

    -- ** Environment
    environment_awsAccountId,
    environment_name,
    environment_federationParameters,
    environment_status,
    environment_description,
    environment_federationMode,
    environment_environmentUrl,
    environment_kmsKeyId,
    environment_dedicatedServiceAccountId,
    environment_environmentArn,
    environment_environmentId,
    environment_sageMakerStudioDomainUrl,

    -- ** FederationParameters
    federationParameters_federationProviderName,
    federationParameters_samlMetadataDocument,
    federationParameters_federationURN,
    federationParameters_samlMetadataURL,
    federationParameters_applicationCallBackURL,
    federationParameters_attributeMap,

    -- ** SuperuserParameters
    superuserParameters_emailAddress,
    superuserParameters_firstName,
    superuserParameters_lastName,
  )
where

import Amazonka.FinSpace.CreateEnvironment
import Amazonka.FinSpace.DeleteEnvironment
import Amazonka.FinSpace.GetEnvironment
import Amazonka.FinSpace.ListEnvironments
import Amazonka.FinSpace.ListTagsForResource
import Amazonka.FinSpace.TagResource
import Amazonka.FinSpace.Types.Environment
import Amazonka.FinSpace.Types.FederationParameters
import Amazonka.FinSpace.Types.SuperuserParameters
import Amazonka.FinSpace.UntagResource
import Amazonka.FinSpace.UpdateEnvironment
