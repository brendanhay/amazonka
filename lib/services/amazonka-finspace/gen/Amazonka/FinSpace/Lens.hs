{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Lens
  ( -- * Operations

    -- ** CreateEnvironment
    createEnvironment_dataBundles,
    createEnvironment_description,
    createEnvironment_federationMode,
    createEnvironment_federationParameters,
    createEnvironment_kmsKeyId,
    createEnvironment_superuserParameters,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentId,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_environments,
    listEnvironmentsResponse_nextToken,
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
    updateEnvironment_description,
    updateEnvironment_federationMode,
    updateEnvironment_federationParameters,
    updateEnvironment_name,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_environment,
    updateEnvironmentResponse_httpStatus,

    -- * Types

    -- ** Environment
    environment_awsAccountId,
    environment_dedicatedServiceAccountId,
    environment_description,
    environment_environmentArn,
    environment_environmentId,
    environment_environmentUrl,
    environment_federationMode,
    environment_federationParameters,
    environment_kmsKeyId,
    environment_name,
    environment_sageMakerStudioDomainUrl,
    environment_status,

    -- ** FederationParameters
    federationParameters_applicationCallBackURL,
    federationParameters_attributeMap,
    federationParameters_federationProviderName,
    federationParameters_federationURN,
    federationParameters_samlMetadataDocument,
    federationParameters_samlMetadataURL,

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
