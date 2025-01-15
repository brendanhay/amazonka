{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.Types.ListIntegrationItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListIntegrationItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An integration in list of integrations.
--
-- /See:/ 'newListIntegrationItem' smart constructor.
data ListIntegrationItem = ListIntegrationItem'
  { -- | Boolean to indicate if the Flow associated with the Integration is
    -- created via Appflow console or with ObjectTypeName equals _unstructured
    -- via API\/CLI in flowDefinition
    isUnstructured :: Prelude.Maybe Prelude.Bool,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Maybe Prelude.Text,
    -- | A map in which each key is an event type from an external application
    -- such as Segment or Shopify, and each value is an @ObjectTypeName@
    -- (template) used to ingest the event. It supports the following event
    -- types: @SegmentIdentify@, @ShopifyCreateCustomers@,
    -- @ShopifyUpdateCustomers@, @ShopifyCreateDraftOrders@,
    -- @ShopifyUpdateDraftOrders@, @ShopifyCreateOrders@, and
    -- @ShopifyUpdatedOrders@.
    objectTypeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The URI of the S3 bucket or any other type of data source.
    uri :: Prelude.Text,
    -- | The timestamp of when the domain was created.
    createdAt :: Data.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIntegrationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isUnstructured', 'listIntegrationItem_isUnstructured' - Boolean to indicate if the Flow associated with the Integration is
-- created via Appflow console or with ObjectTypeName equals _unstructured
-- via API\/CLI in flowDefinition
--
-- 'objectTypeName', 'listIntegrationItem_objectTypeName' - The name of the profile object type.
--
-- 'objectTypeNames', 'listIntegrationItem_objectTypeNames' - A map in which each key is an event type from an external application
-- such as Segment or Shopify, and each value is an @ObjectTypeName@
-- (template) used to ingest the event. It supports the following event
-- types: @SegmentIdentify@, @ShopifyCreateCustomers@,
-- @ShopifyUpdateCustomers@, @ShopifyCreateDraftOrders@,
-- @ShopifyUpdateDraftOrders@, @ShopifyCreateOrders@, and
-- @ShopifyUpdatedOrders@.
--
-- 'tags', 'listIntegrationItem_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'workflowId', 'listIntegrationItem_workflowId' - Unique identifier for the workflow.
--
-- 'domainName', 'listIntegrationItem_domainName' - The unique name of the domain.
--
-- 'uri', 'listIntegrationItem_uri' - The URI of the S3 bucket or any other type of data source.
--
-- 'createdAt', 'listIntegrationItem_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'listIntegrationItem_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newListIntegrationItem ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'uri'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  ListIntegrationItem
newListIntegrationItem
  pDomainName_
  pUri_
  pCreatedAt_
  pLastUpdatedAt_ =
    ListIntegrationItem'
      { isUnstructured =
          Prelude.Nothing,
        objectTypeName = Prelude.Nothing,
        objectTypeNames = Prelude.Nothing,
        tags = Prelude.Nothing,
        workflowId = Prelude.Nothing,
        domainName = pDomainName_,
        uri = pUri_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_
      }

-- | Boolean to indicate if the Flow associated with the Integration is
-- created via Appflow console or with ObjectTypeName equals _unstructured
-- via API\/CLI in flowDefinition
listIntegrationItem_isUnstructured :: Lens.Lens' ListIntegrationItem (Prelude.Maybe Prelude.Bool)
listIntegrationItem_isUnstructured = Lens.lens (\ListIntegrationItem' {isUnstructured} -> isUnstructured) (\s@ListIntegrationItem' {} a -> s {isUnstructured = a} :: ListIntegrationItem)

-- | The name of the profile object type.
listIntegrationItem_objectTypeName :: Lens.Lens' ListIntegrationItem (Prelude.Maybe Prelude.Text)
listIntegrationItem_objectTypeName = Lens.lens (\ListIntegrationItem' {objectTypeName} -> objectTypeName) (\s@ListIntegrationItem' {} a -> s {objectTypeName = a} :: ListIntegrationItem)

-- | A map in which each key is an event type from an external application
-- such as Segment or Shopify, and each value is an @ObjectTypeName@
-- (template) used to ingest the event. It supports the following event
-- types: @SegmentIdentify@, @ShopifyCreateCustomers@,
-- @ShopifyUpdateCustomers@, @ShopifyCreateDraftOrders@,
-- @ShopifyUpdateDraftOrders@, @ShopifyCreateOrders@, and
-- @ShopifyUpdatedOrders@.
listIntegrationItem_objectTypeNames :: Lens.Lens' ListIntegrationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listIntegrationItem_objectTypeNames = Lens.lens (\ListIntegrationItem' {objectTypeNames} -> objectTypeNames) (\s@ListIntegrationItem' {} a -> s {objectTypeNames = a} :: ListIntegrationItem) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
listIntegrationItem_tags :: Lens.Lens' ListIntegrationItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listIntegrationItem_tags = Lens.lens (\ListIntegrationItem' {tags} -> tags) (\s@ListIntegrationItem' {} a -> s {tags = a} :: ListIntegrationItem) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier for the workflow.
listIntegrationItem_workflowId :: Lens.Lens' ListIntegrationItem (Prelude.Maybe Prelude.Text)
listIntegrationItem_workflowId = Lens.lens (\ListIntegrationItem' {workflowId} -> workflowId) (\s@ListIntegrationItem' {} a -> s {workflowId = a} :: ListIntegrationItem)

-- | The unique name of the domain.
listIntegrationItem_domainName :: Lens.Lens' ListIntegrationItem Prelude.Text
listIntegrationItem_domainName = Lens.lens (\ListIntegrationItem' {domainName} -> domainName) (\s@ListIntegrationItem' {} a -> s {domainName = a} :: ListIntegrationItem)

-- | The URI of the S3 bucket or any other type of data source.
listIntegrationItem_uri :: Lens.Lens' ListIntegrationItem Prelude.Text
listIntegrationItem_uri = Lens.lens (\ListIntegrationItem' {uri} -> uri) (\s@ListIntegrationItem' {} a -> s {uri = a} :: ListIntegrationItem)

-- | The timestamp of when the domain was created.
listIntegrationItem_createdAt :: Lens.Lens' ListIntegrationItem Prelude.UTCTime
listIntegrationItem_createdAt = Lens.lens (\ListIntegrationItem' {createdAt} -> createdAt) (\s@ListIntegrationItem' {} a -> s {createdAt = a} :: ListIntegrationItem) Prelude.. Data._Time

-- | The timestamp of when the domain was most recently edited.
listIntegrationItem_lastUpdatedAt :: Lens.Lens' ListIntegrationItem Prelude.UTCTime
listIntegrationItem_lastUpdatedAt = Lens.lens (\ListIntegrationItem' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListIntegrationItem' {} a -> s {lastUpdatedAt = a} :: ListIntegrationItem) Prelude.. Data._Time

instance Data.FromJSON ListIntegrationItem where
  parseJSON =
    Data.withObject
      "ListIntegrationItem"
      ( \x ->
          ListIntegrationItem'
            Prelude.<$> (x Data..:? "IsUnstructured")
            Prelude.<*> (x Data..:? "ObjectTypeName")
            Prelude.<*> ( x
                            Data..:? "ObjectTypeNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WorkflowId")
            Prelude.<*> (x Data..: "DomainName")
            Prelude.<*> (x Data..: "Uri")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "LastUpdatedAt")
      )

instance Prelude.Hashable ListIntegrationItem where
  hashWithSalt _salt ListIntegrationItem' {..} =
    _salt
      `Prelude.hashWithSalt` isUnstructured
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` objectTypeNames
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt

instance Prelude.NFData ListIntegrationItem where
  rnf ListIntegrationItem' {..} =
    Prelude.rnf isUnstructured `Prelude.seq`
      Prelude.rnf objectTypeName `Prelude.seq`
        Prelude.rnf objectTypeNames `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf workflowId `Prelude.seq`
              Prelude.rnf domainName `Prelude.seq`
                Prelude.rnf uri `Prelude.seq`
                  Prelude.rnf createdAt `Prelude.seq`
                    Prelude.rnf lastUpdatedAt
