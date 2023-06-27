{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.HealthLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon HealthLake is a HIPAA eligibile service that allows customers to
-- store, transform, query, and analyze their FHIR-formatted data in a
-- consistent fashion in the cloud.
module Amazonka.HealthLake
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateFHIRDatastore
    CreateFHIRDatastore (CreateFHIRDatastore'),
    newCreateFHIRDatastore,
    CreateFHIRDatastoreResponse (CreateFHIRDatastoreResponse'),
    newCreateFHIRDatastoreResponse,

    -- ** DeleteFHIRDatastore
    DeleteFHIRDatastore (DeleteFHIRDatastore'),
    newDeleteFHIRDatastore,
    DeleteFHIRDatastoreResponse (DeleteFHIRDatastoreResponse'),
    newDeleteFHIRDatastoreResponse,

    -- ** DescribeFHIRDatastore
    DescribeFHIRDatastore (DescribeFHIRDatastore'),
    newDescribeFHIRDatastore,
    DescribeFHIRDatastoreResponse (DescribeFHIRDatastoreResponse'),
    newDescribeFHIRDatastoreResponse,

    -- ** DescribeFHIRExportJob
    DescribeFHIRExportJob (DescribeFHIRExportJob'),
    newDescribeFHIRExportJob,
    DescribeFHIRExportJobResponse (DescribeFHIRExportJobResponse'),
    newDescribeFHIRExportJobResponse,

    -- ** DescribeFHIRImportJob
    DescribeFHIRImportJob (DescribeFHIRImportJob'),
    newDescribeFHIRImportJob,
    DescribeFHIRImportJobResponse (DescribeFHIRImportJobResponse'),
    newDescribeFHIRImportJobResponse,

    -- ** ListFHIRDatastores
    ListFHIRDatastores (ListFHIRDatastores'),
    newListFHIRDatastores,
    ListFHIRDatastoresResponse (ListFHIRDatastoresResponse'),
    newListFHIRDatastoresResponse,

    -- ** ListFHIRExportJobs
    ListFHIRExportJobs (ListFHIRExportJobs'),
    newListFHIRExportJobs,
    ListFHIRExportJobsResponse (ListFHIRExportJobsResponse'),
    newListFHIRExportJobsResponse,

    -- ** ListFHIRImportJobs
    ListFHIRImportJobs (ListFHIRImportJobs'),
    newListFHIRImportJobs,
    ListFHIRImportJobsResponse (ListFHIRImportJobsResponse'),
    newListFHIRImportJobsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartFHIRExportJob
    StartFHIRExportJob (StartFHIRExportJob'),
    newStartFHIRExportJob,
    StartFHIRExportJobResponse (StartFHIRExportJobResponse'),
    newStartFHIRExportJobResponse,

    -- ** StartFHIRImportJob
    StartFHIRImportJob (StartFHIRImportJob'),
    newStartFHIRImportJob,
    StartFHIRImportJobResponse (StartFHIRImportJobResponse'),
    newStartFHIRImportJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** AuthorizationStrategy
    AuthorizationStrategy (..),

    -- ** CmkType
    CmkType (..),

    -- ** DatastoreStatus
    DatastoreStatus (..),

    -- ** FHIRVersion
    FHIRVersion (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** PreloadDataType
    PreloadDataType (..),

    -- ** DatastoreFilter
    DatastoreFilter (DatastoreFilter'),
    newDatastoreFilter,

    -- ** DatastoreProperties
    DatastoreProperties (DatastoreProperties'),
    newDatastoreProperties,

    -- ** ExportJobProperties
    ExportJobProperties (ExportJobProperties'),
    newExportJobProperties,

    -- ** IdentityProviderConfiguration
    IdentityProviderConfiguration (IdentityProviderConfiguration'),
    newIdentityProviderConfiguration,

    -- ** ImportJobProperties
    ImportJobProperties (ImportJobProperties'),
    newImportJobProperties,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** KmsEncryptionConfig
    KmsEncryptionConfig (KmsEncryptionConfig'),
    newKmsEncryptionConfig,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** PreloadDataConfig
    PreloadDataConfig (PreloadDataConfig'),
    newPreloadDataConfig,

    -- ** S3Configuration
    S3Configuration (S3Configuration'),
    newS3Configuration,

    -- ** SseConfiguration
    SseConfiguration (SseConfiguration'),
    newSseConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.HealthLake.CreateFHIRDatastore
import Amazonka.HealthLake.DeleteFHIRDatastore
import Amazonka.HealthLake.DescribeFHIRDatastore
import Amazonka.HealthLake.DescribeFHIRExportJob
import Amazonka.HealthLake.DescribeFHIRImportJob
import Amazonka.HealthLake.Lens
import Amazonka.HealthLake.ListFHIRDatastores
import Amazonka.HealthLake.ListFHIRExportJobs
import Amazonka.HealthLake.ListFHIRImportJobs
import Amazonka.HealthLake.ListTagsForResource
import Amazonka.HealthLake.StartFHIRExportJob
import Amazonka.HealthLake.StartFHIRImportJob
import Amazonka.HealthLake.TagResource
import Amazonka.HealthLake.Types
import Amazonka.HealthLake.UntagResource
import Amazonka.HealthLake.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'HealthLake'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
