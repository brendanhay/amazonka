{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Lens
  ( -- * Operations

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxNumberOfDomains,
    listDomainsResponse_nextToken,
    listDomainsResponse_domainNames,
    listDomainsResponse_httpStatus,

    -- ** DeleteAttributes
    deleteAttributes_expected,
    deleteAttributes_attributes,
    deleteAttributes_domainName,
    deleteAttributes_itemName,

    -- ** DomainMetadata
    domainMetadata_domainName,
    domainMetadataResponse_attributeNamesSizeBytes,
    domainMetadataResponse_attributeValueCount,
    domainMetadataResponse_attributeNameCount,
    domainMetadataResponse_attributeValuesSizeBytes,
    domainMetadataResponse_timestamp,
    domainMetadataResponse_itemCount,
    domainMetadataResponse_itemNamesSizeBytes,
    domainMetadataResponse_httpStatus,

    -- ** BatchPutAttributes
    batchPutAttributes_domainName,
    batchPutAttributes_items,

    -- ** CreateDomain
    createDomain_domainName,

    -- ** GetAttributes
    getAttributes_consistentRead,
    getAttributes_attributeNames,
    getAttributes_domainName,
    getAttributes_itemName,
    getAttributesResponse_attributes,
    getAttributesResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,

    -- ** PutAttributes
    putAttributes_expected,
    putAttributes_domainName,
    putAttributes_itemName,
    putAttributes_attributes,

    -- ** Select
    select_nextToken,
    select_consistentRead,
    select_selectExpression,
    selectResponse_nextToken,
    selectResponse_items,
    selectResponse_httpStatus,

    -- ** BatchDeleteAttributes
    batchDeleteAttributes_domainName,
    batchDeleteAttributes_items,

    -- * Types

    -- ** Attribute
    attribute_alternateNameEncoding,
    attribute_alternateValueEncoding,
    attribute_name,
    attribute_value,

    -- ** DeletableItem
    deletableItem_attributes,
    deletableItem_name,

    -- ** Item
    item_alternateNameEncoding,
    item_name,
    item_attributes,

    -- ** ReplaceableAttribute
    replaceableAttribute_replace,
    replaceableAttribute_name,
    replaceableAttribute_value,

    -- ** ReplaceableItem
    replaceableItem_name,
    replaceableItem_attributes,

    -- ** UpdateCondition
    updateCondition_exists,
    updateCondition_name,
    updateCondition_value,
  )
where

import Network.AWS.SDB.BatchDeleteAttributes
import Network.AWS.SDB.BatchPutAttributes
import Network.AWS.SDB.CreateDomain
import Network.AWS.SDB.DeleteAttributes
import Network.AWS.SDB.DeleteDomain
import Network.AWS.SDB.DomainMetadata
import Network.AWS.SDB.GetAttributes
import Network.AWS.SDB.ListDomains
import Network.AWS.SDB.PutAttributes
import Network.AWS.SDB.Select
import Network.AWS.SDB.Types.Attribute
import Network.AWS.SDB.Types.DeletableItem
import Network.AWS.SDB.Types.Item
import Network.AWS.SDB.Types.ReplaceableAttribute
import Network.AWS.SDB.Types.ReplaceableItem
import Network.AWS.SDB.Types.UpdateCondition
