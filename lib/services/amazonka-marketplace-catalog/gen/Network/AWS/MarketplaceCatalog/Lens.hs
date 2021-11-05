{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceCatalog.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceCatalog.Lens
  ( -- * Operations

    -- ** ListEntities
    listEntities_nextToken,
    listEntities_filterList,
    listEntities_sort,
    listEntities_maxResults,
    listEntities_catalog,
    listEntities_entityType,
    listEntitiesResponse_entitySummaryList,
    listEntitiesResponse_nextToken,
    listEntitiesResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_nextToken,
    listChangeSets_filterList,
    listChangeSets_sort,
    listChangeSets_maxResults,
    listChangeSets_catalog,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_changeSetSummaryList,
    listChangeSetsResponse_httpStatus,

    -- ** StartChangeSet
    startChangeSet_changeSetName,
    startChangeSet_clientRequestToken,
    startChangeSet_catalog,
    startChangeSet_changeSet,
    startChangeSetResponse_changeSetId,
    startChangeSetResponse_changeSetArn,
    startChangeSetResponse_httpStatus,

    -- ** CancelChangeSet
    cancelChangeSet_catalog,
    cancelChangeSet_changeSetId,
    cancelChangeSetResponse_changeSetId,
    cancelChangeSetResponse_changeSetArn,
    cancelChangeSetResponse_httpStatus,

    -- ** DescribeEntity
    describeEntity_catalog,
    describeEntity_entityId,
    describeEntityResponse_lastModifiedDate,
    describeEntityResponse_entityType,
    describeEntityResponse_details,
    describeEntityResponse_entityIdentifier,
    describeEntityResponse_entityArn,
    describeEntityResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_catalog,
    describeChangeSet_changeSetId,
    describeChangeSetResponse_status,
    describeChangeSetResponse_startTime,
    describeChangeSetResponse_failureCode,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_failureDescription,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSet,
    describeChangeSetResponse_endTime,
    describeChangeSetResponse_changeSetArn,
    describeChangeSetResponse_httpStatus,

    -- * Types

    -- ** Change
    change_changeName,
    change_changeType,
    change_entity,
    change_details,

    -- ** ChangeSetSummaryListItem
    changeSetSummaryListItem_status,
    changeSetSummaryListItem_entityIdList,
    changeSetSummaryListItem_startTime,
    changeSetSummaryListItem_failureCode,
    changeSetSummaryListItem_changeSetName,
    changeSetSummaryListItem_changeSetId,
    changeSetSummaryListItem_endTime,
    changeSetSummaryListItem_changeSetArn,

    -- ** ChangeSummary
    changeSummary_changeName,
    changeSummary_details,
    changeSummary_errorDetailList,
    changeSummary_entity,
    changeSummary_changeType,

    -- ** Entity
    entity_identifier,
    entity_type,

    -- ** EntitySummary
    entitySummary_lastModifiedDate,
    entitySummary_entityType,
    entitySummary_visibility,
    entitySummary_name,
    entitySummary_entityId,
    entitySummary_entityArn,

    -- ** ErrorDetail
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- ** Filter
    filter_valueList,
    filter_name,

    -- ** Sort
    sort_sortOrder,
    sort_sortBy,
  )
where

import Network.AWS.MarketplaceCatalog.CancelChangeSet
import Network.AWS.MarketplaceCatalog.DescribeChangeSet
import Network.AWS.MarketplaceCatalog.DescribeEntity
import Network.AWS.MarketplaceCatalog.ListChangeSets
import Network.AWS.MarketplaceCatalog.ListEntities
import Network.AWS.MarketplaceCatalog.StartChangeSet
import Network.AWS.MarketplaceCatalog.Types.Change
import Network.AWS.MarketplaceCatalog.Types.ChangeSetSummaryListItem
import Network.AWS.MarketplaceCatalog.Types.ChangeSummary
import Network.AWS.MarketplaceCatalog.Types.Entity
import Network.AWS.MarketplaceCatalog.Types.EntitySummary
import Network.AWS.MarketplaceCatalog.Types.ErrorDetail
import Network.AWS.MarketplaceCatalog.Types.Filter
import Network.AWS.MarketplaceCatalog.Types.Sort
