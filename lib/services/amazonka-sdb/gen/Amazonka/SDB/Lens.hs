{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SDB.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Lens
  ( -- * Operations

    -- ** BatchDeleteAttributes
    batchDeleteAttributes_domainName,
    batchDeleteAttributes_items,

    -- ** BatchPutAttributes
    batchPutAttributes_domainName,
    batchPutAttributes_items,

    -- ** CreateDomain
    createDomain_domainName,

    -- ** DeleteAttributes
    deleteAttributes_attributes,
    deleteAttributes_expected,
    deleteAttributes_domainName,
    deleteAttributes_itemName,

    -- ** DeleteDomain
    deleteDomain_domainName,

    -- ** DomainMetadata
    domainMetadata_domainName,
    domainMetadataResponse_attributeNameCount,
    domainMetadataResponse_attributeNamesSizeBytes,
    domainMetadataResponse_attributeValueCount,
    domainMetadataResponse_attributeValuesSizeBytes,
    domainMetadataResponse_itemCount,
    domainMetadataResponse_itemNamesSizeBytes,
    domainMetadataResponse_timestamp,
    domainMetadataResponse_httpStatus,

    -- ** GetAttributes
    getAttributes_attributeNames,
    getAttributes_consistentRead,
    getAttributes_domainName,
    getAttributes_itemName,
    getAttributesResponse_attributes,
    getAttributesResponse_httpStatus,

    -- ** ListDomains
    listDomains_maxNumberOfDomains,
    listDomains_nextToken,
    listDomainsResponse_domainNames,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** PutAttributes
    putAttributes_expected,
    putAttributes_domainName,
    putAttributes_itemName,
    putAttributes_attributes,

    -- ** Select
    select_consistentRead,
    select_nextToken,
    select_selectExpression,
    selectResponse_items,
    selectResponse_nextToken,
    selectResponse_httpStatus,

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

import Amazonka.SDB.BatchDeleteAttributes
import Amazonka.SDB.BatchPutAttributes
import Amazonka.SDB.CreateDomain
import Amazonka.SDB.DeleteAttributes
import Amazonka.SDB.DeleteDomain
import Amazonka.SDB.DomainMetadata
import Amazonka.SDB.GetAttributes
import Amazonka.SDB.ListDomains
import Amazonka.SDB.PutAttributes
import Amazonka.SDB.Select
import Amazonka.SDB.Types.Attribute
import Amazonka.SDB.Types.DeletableItem
import Amazonka.SDB.Types.Item
import Amazonka.SDB.Types.ReplaceableAttribute
import Amazonka.SDB.Types.ReplaceableItem
import Amazonka.SDB.Types.UpdateCondition
