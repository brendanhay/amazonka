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
    cancelChangeSetResponse_changeSetArn,
    cancelChangeSetResponse_changeSetId,
    cancelChangeSetResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_catalog,
    describeChangeSet_changeSetId,
    describeChangeSetResponse_changeSet,
    describeChangeSetResponse_changeSetArn,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_endTime,
    describeChangeSetResponse_failureCode,
    describeChangeSetResponse_failureDescription,
    describeChangeSetResponse_startTime,
    describeChangeSetResponse_status,
    describeChangeSetResponse_httpStatus,

    -- ** DescribeEntity
    describeEntity_catalog,
    describeEntity_entityId,
    describeEntityResponse_details,
    describeEntityResponse_entityArn,
    describeEntityResponse_entityIdentifier,
    describeEntityResponse_entityType,
    describeEntityResponse_lastModifiedDate,
    describeEntityResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_filterList,
    listChangeSets_maxResults,
    listChangeSets_nextToken,
    listChangeSets_sort,
    listChangeSets_catalog,
    listChangeSetsResponse_changeSetSummaryList,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_httpStatus,

    -- ** ListEntities
    listEntities_filterList,
    listEntities_maxResults,
    listEntities_nextToken,
    listEntities_sort,
    listEntities_catalog,
    listEntities_entityType,
    listEntitiesResponse_entitySummaryList,
    listEntitiesResponse_nextToken,
    listEntitiesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartChangeSet
    startChangeSet_changeSetName,
    startChangeSet_changeSetTags,
    startChangeSet_clientRequestToken,
    startChangeSet_catalog,
    startChangeSet_changeSet,
    startChangeSetResponse_changeSetArn,
    startChangeSetResponse_changeSetId,
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
    changeSetSummaryListItem_changeSetArn,
    changeSetSummaryListItem_changeSetId,
    changeSetSummaryListItem_changeSetName,
    changeSetSummaryListItem_endTime,
    changeSetSummaryListItem_entityIdList,
    changeSetSummaryListItem_failureCode,
    changeSetSummaryListItem_startTime,
    changeSetSummaryListItem_status,

    -- ** ChangeSummary
    changeSummary_changeName,
    changeSummary_changeType,
    changeSummary_details,
    changeSummary_entity,
    changeSummary_errorDetailList,

    -- ** Entity
    entity_identifier,
    entity_type,

    -- ** EntitySummary
    entitySummary_entityArn,
    entitySummary_entityId,
    entitySummary_entityType,
    entitySummary_lastModifiedDate,
    entitySummary_name,
    entitySummary_visibility,

    -- ** ErrorDetail
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- ** Filter
    filter_name,
    filter_valueList,

    -- ** Sort
    sort_sortBy,
    sort_sortOrder,

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
