{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EBS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-11-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You can use the Amazon Elastic Block Store (Amazon EBS) direct APIs to
-- create Amazon EBS snapshots, write data directly to your snapshots, read
-- data on your snapshots, and identify the differences or changes between
-- two snapshots. If you’re an independent software vendor (ISV) who offers
-- backup services for Amazon EBS, the EBS direct APIs make it more
-- efficient and cost-effective to track incremental changes on your Amazon
-- EBS volumes through snapshots. This can be done without having to create
-- new volumes from snapshots, and then use Amazon Elastic Compute Cloud
-- (Amazon EC2) instances to compare the differences.
--
-- You can create incremental snapshots directly from data on-premises into
-- volumes and the cloud to use for quick disaster recovery. With the
-- ability to write and read snapshots, you can write your on-premises data
-- to an snapshot during a disaster. Then after recovery, you can restore
-- it back to Amazon Web Services or on-premises from the snapshot. You no
-- longer need to build and maintain complex mechanisms to copy data to and
-- from Amazon EBS.
--
-- This API reference provides detailed information about the actions, data
-- types, parameters, and errors of the EBS direct APIs. For more
-- information about the elements that make up the EBS direct APIs, and
-- examples of how to use them effectively, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-accessing-snapshot.html Accessing the Contents of an Amazon EBS Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about the supported Amazon Web Services Regions, endpoints, and service
-- quotas for the EBS direct APIs, see
-- <https://docs.aws.amazon.com/general/latest/gr/ebs-service.html Amazon Elastic Block Store Endpoints and Quotas>
-- in the /Amazon Web Services General Reference/.
module Amazonka.EBS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConcurrentLimitExceededException
    _ConcurrentLimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** RequestThrottledException
    _RequestThrottledException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CompleteSnapshot
    CompleteSnapshot (CompleteSnapshot'),
    newCompleteSnapshot,
    CompleteSnapshotResponse (CompleteSnapshotResponse'),
    newCompleteSnapshotResponse,

    -- ** GetSnapshotBlock
    GetSnapshotBlock (GetSnapshotBlock'),
    newGetSnapshotBlock,
    GetSnapshotBlockResponse (GetSnapshotBlockResponse'),
    newGetSnapshotBlockResponse,

    -- ** ListChangedBlocks
    ListChangedBlocks (ListChangedBlocks'),
    newListChangedBlocks,
    ListChangedBlocksResponse (ListChangedBlocksResponse'),
    newListChangedBlocksResponse,

    -- ** ListSnapshotBlocks
    ListSnapshotBlocks (ListSnapshotBlocks'),
    newListSnapshotBlocks,
    ListSnapshotBlocksResponse (ListSnapshotBlocksResponse'),
    newListSnapshotBlocksResponse,

    -- ** PutSnapshotBlock
    PutSnapshotBlock (PutSnapshotBlock'),
    newPutSnapshotBlock,
    PutSnapshotBlockResponse (PutSnapshotBlockResponse'),
    newPutSnapshotBlockResponse,

    -- ** StartSnapshot
    StartSnapshot (StartSnapshot'),
    newStartSnapshot,
    StartSnapshotResponse (StartSnapshotResponse'),
    newStartSnapshotResponse,

    -- * Types

    -- ** ChecksumAggregationMethod
    ChecksumAggregationMethod (..),

    -- ** ChecksumAlgorithm
    ChecksumAlgorithm (..),

    -- ** Status
    Status (..),

    -- ** Block
    Block (Block'),
    newBlock,

    -- ** ChangedBlock
    ChangedBlock (ChangedBlock'),
    newChangedBlock,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.EBS.CompleteSnapshot
import Amazonka.EBS.GetSnapshotBlock
import Amazonka.EBS.Lens
import Amazonka.EBS.ListChangedBlocks
import Amazonka.EBS.ListSnapshotBlocks
import Amazonka.EBS.PutSnapshotBlock
import Amazonka.EBS.StartSnapshot
import Amazonka.EBS.Types
import Amazonka.EBS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EBS'.

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
