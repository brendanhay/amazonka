{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Lens
  ( -- * Operations

    -- ** CreateChangeset
    createChangeset_tags,
    createChangeset_formatParams,
    createChangeset_formatType,
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
    getWorkingLocationResponse_s3Bucket,
    getWorkingLocationResponse_s3Path,
    getWorkingLocationResponse_s3Uri,
    getWorkingLocationResponse_httpStatus,

    -- * Types

    -- ** ChangesetInfo
    changesetInfo_sourceParams,
    changesetInfo_updatedByChangesetId,
    changesetInfo_changeType,
    changesetInfo_createTimestamp,
    changesetInfo_changesetArn,
    changesetInfo_formatParams,
    changesetInfo_formatType,
    changesetInfo_status,
    changesetInfo_sourceType,
    changesetInfo_id,
    changesetInfo_changesetLabels,
    changesetInfo_updatesChangesetId,
    changesetInfo_datasetId,
    changesetInfo_errorInfo,

    -- ** Credentials
    credentials_sessionToken,
    credentials_secretAccessKey,
    credentials_accessKeyId,

    -- ** ErrorInfo
    errorInfo_errorCategory,
    errorInfo_errorMessage,
  )
where

import Amazonka.FinSpaceData.CreateChangeset
import Amazonka.FinSpaceData.GetProgrammaticAccessCredentials
import Amazonka.FinSpaceData.GetWorkingLocation
import Amazonka.FinSpaceData.Types.ChangesetInfo
import Amazonka.FinSpaceData.Types.Credentials
import Amazonka.FinSpaceData.Types.ErrorInfo
