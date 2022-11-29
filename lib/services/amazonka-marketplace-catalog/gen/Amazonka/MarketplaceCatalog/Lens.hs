{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceCatalog.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Lens
  ( -- * Operations

    -- ** CancelChangeSet
    cancelChangeSet_catalog,
    cancelChangeSet_changeSetId,
    cancelChangeSetResponse_changeSetId,
    cancelChangeSetResponse_changeSetArn,
    cancelChangeSetResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_catalog,
    describeChangeSet_changeSetId,
    describeChangeSetResponse_failureDescription,
    describeChangeSetResponse_failureCode,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_changeSetArn,
    describeChangeSetResponse_status,
    describeChangeSetResponse_endTime,
    describeChangeSetResponse_changeSet,
    describeChangeSetResponse_startTime,
    describeChangeSetResponse_httpStatus,

    -- ** DescribeEntity
    describeEntity_catalog,
    describeEntity_entityId,
    describeEntityResponse_lastModifiedDate,
    describeEntityResponse_details,
    describeEntityResponse_entityType,
    describeEntityResponse_entityIdentifier,
    describeEntityResponse_entityArn,
    describeEntityResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_nextToken,
    listChangeSets_filterList,
    listChangeSets_sort,
    listChangeSets_maxResults,
    listChangeSets_catalog,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_changeSetSummaryList,
    listChangeSetsResponse_httpStatus,

    -- ** ListEntities
    listEntities_nextToken,
    listEntities_filterList,
    listEntities_sort,
    listEntities_maxResults,
    listEntities_catalog,
    listEntities_entityType,
    listEntitiesResponse_nextToken,
    listEntitiesResponse_entitySummaryList,
    listEntitiesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** StartChangeSet
    startChangeSet_clientRequestToken,
    startChangeSet_changeSetName,
    startChangeSet_changeSetTags,
    startChangeSet_catalog,
    startChangeSet_changeSet,
    startChangeSetResponse_changeSetId,
    startChangeSetResponse_changeSetArn,
    startChangeSetResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Change
    change_changeName,
    change_entityTags,
    change_changeType,
    change_entity,
    change_details,

    -- ** ChangeSetSummaryListItem
    changeSetSummaryListItem_failureCode,
    changeSetSummaryListItem_changeSetId,
    changeSetSummaryListItem_changeSetName,
    changeSetSummaryListItem_changeSetArn,
    changeSetSummaryListItem_status,
    changeSetSummaryListItem_endTime,
    changeSetSummaryListItem_entityIdList,
    changeSetSummaryListItem_startTime,

    -- ** ChangeSummary
    changeSummary_entity,
    changeSummary_changeName,
    changeSummary_changeType,
    changeSummary_details,
    changeSummary_errorDetailList,

    -- ** Entity
    entity_identifier,
    entity_type,

    -- ** EntitySummary
    entitySummary_entityId,
    entitySummary_name,
    entitySummary_lastModifiedDate,
    entitySummary_visibility,
    entitySummary_entityType,
    entitySummary_entityArn,

    -- ** ErrorDetail
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- ** Filter
    filter_name,
    filter_valueList,

    -- ** Sort
    sort_sortOrder,
    sort_sortBy,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.MarketplaceCatalog.CancelChangeSet
import Amazonka.MarketplaceCatalog.DescribeChangeSet
import Amazonka.MarketplaceCatalog.DescribeEntity
import Amazonka.MarketplaceCatalog.ListChangeSets
import Amazonka.MarketplaceCatalog.ListEntities
import Amazonka.MarketplaceCatalog.ListTagsForResource
import Amazonka.MarketplaceCatalog.StartChangeSet
import Amazonka.MarketplaceCatalog.TagResource
import Amazonka.MarketplaceCatalog.Types.Change
import Amazonka.MarketplaceCatalog.Types.ChangeSetSummaryListItem
import Amazonka.MarketplaceCatalog.Types.ChangeSummary
import Amazonka.MarketplaceCatalog.Types.Entity
import Amazonka.MarketplaceCatalog.Types.EntitySummary
import Amazonka.MarketplaceCatalog.Types.ErrorDetail
import Amazonka.MarketplaceCatalog.Types.Filter
import Amazonka.MarketplaceCatalog.Types.Sort
import Amazonka.MarketplaceCatalog.Types.Tag
import Amazonka.MarketplaceCatalog.UntagResource
