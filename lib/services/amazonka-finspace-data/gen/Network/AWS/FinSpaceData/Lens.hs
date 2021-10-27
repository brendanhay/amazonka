{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FinSpaceData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FinSpaceData.Lens
  ( -- * Operations

    -- ** CreateChangeset
    createChangeset_formatParams,
    createChangeset_formatType,
    createChangeset_tags,
    createChangeset_datasetId,
    createChangeset_changeType,
    createChangeset_sourceType,
    createChangeset_sourceParams,
    createChangesetResponse_changeset,
    createChangesetResponse_httpStatus,

    -- ** GetProgrammaticAccessCredentials
    getProgrammaticAccessCredentials_durationInMinutes,
    getProgrammaticAccessCredentials_environmentId,
    getProgrammaticAccessCredentialsResponse_credentials,
    getProgrammaticAccessCredentialsResponse_durationInMinutes,
    getProgrammaticAccessCredentialsResponse_httpStatus,

    -- ** GetWorkingLocation
    getWorkingLocation_locationType,
    getWorkingLocationResponse_s3Path,
    getWorkingLocationResponse_s3Uri,
    getWorkingLocationResponse_s3Bucket,
    getWorkingLocationResponse_httpStatus,

    -- * Types

    -- ** ChangesetInfo
    changesetInfo_status,
    changesetInfo_sourceType,
    changesetInfo_sourceParams,
    changesetInfo_changesetLabels,
    changesetInfo_updatedByChangesetId,
    changesetInfo_datasetId,
    changesetInfo_formatParams,
    changesetInfo_createTimestamp,
    changesetInfo_id,
    changesetInfo_formatType,
    changesetInfo_updatesChangesetId,
    changesetInfo_changeType,
    changesetInfo_errorInfo,
    changesetInfo_changesetArn,

    -- ** Credentials
    credentials_secretAccessKey,
    credentials_sessionToken,
    credentials_accessKeyId,

    -- ** ErrorInfo
    errorInfo_errorCategory,
    errorInfo_errorMessage,
  )
where

import Network.AWS.FinSpaceData.CreateChangeset
import Network.AWS.FinSpaceData.GetProgrammaticAccessCredentials
import Network.AWS.FinSpaceData.GetWorkingLocation
import Network.AWS.FinSpaceData.Types.ChangesetInfo
import Network.AWS.FinSpaceData.Types.Credentials
import Network.AWS.FinSpaceData.Types.ErrorInfo
