{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.GetIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an integration for a domain.
module Amazonka.CustomerProfiles.GetIntegration
  ( -- * Creating a Request
    GetIntegration (..),
    newGetIntegration,

    -- * Request Lenses
    getIntegration_domainName,
    getIntegration_uri,

    -- * Destructuring the Response
    GetIntegrationResponse (..),
    newGetIntegrationResponse,

    -- * Response Lenses
    getIntegrationResponse_isUnstructured,
    getIntegrationResponse_objectTypeName,
    getIntegrationResponse_objectTypeNames,
    getIntegrationResponse_tags,
    getIntegrationResponse_workflowId,
    getIntegrationResponse_httpStatus,
    getIntegrationResponse_domainName,
    getIntegrationResponse_uri,
    getIntegrationResponse_createdAt,
    getIntegrationResponse_lastUpdatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The URI of the S3 bucket or any other type of data source.
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getIntegration_domainName' - The unique name of the domain.
--
-- 'uri', 'getIntegration_uri' - The URI of the S3 bucket or any other type of data source.
newGetIntegration ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'uri'
  Prelude.Text ->
  GetIntegration
newGetIntegration pDomainName_ pUri_ =
  GetIntegration'
    { domainName = pDomainName_,
      uri = pUri_
    }

-- | The unique name of the domain.
getIntegration_domainName :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_domainName = Lens.lens (\GetIntegration' {domainName} -> domainName) (\s@GetIntegration' {} a -> s {domainName = a} :: GetIntegration)

-- | The URI of the S3 bucket or any other type of data source.
getIntegration_uri :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_uri = Lens.lens (\GetIntegration' {uri} -> uri) (\s@GetIntegration' {} a -> s {uri = a} :: GetIntegration)

instance Core.AWSRequest GetIntegration where
  type
    AWSResponse GetIntegration =
      GetIntegrationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntegrationResponse'
            Prelude.<$> (x Data..?> "IsUnstructured")
            Prelude.<*> (x Data..?> "ObjectTypeName")
            Prelude.<*> ( x Data..?> "ObjectTypeNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "WorkflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
            Prelude.<*> (x Data..:> "Uri")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "LastUpdatedAt")
      )

instance Prelude.Hashable GetIntegration where
  hashWithSalt _salt GetIntegration' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` uri

instance Prelude.NFData GetIntegration where
  rnf GetIntegration' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf uri

instance Data.ToHeaders GetIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetIntegration where
  toJSON GetIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Uri" Data..= uri)]
      )

instance Data.ToPath GetIntegration where
  toPath GetIntegration' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/integrations"]

instance Data.ToQuery GetIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
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
-- Create a value of 'GetIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isUnstructured', 'getIntegrationResponse_isUnstructured' - Boolean to indicate if the Flow associated with the Integration is
-- created via Appflow console or with ObjectTypeName equals _unstructured
-- via API\/CLI in flowDefinition
--
-- 'objectTypeName', 'getIntegrationResponse_objectTypeName' - The name of the profile object type.
--
-- 'objectTypeNames', 'getIntegrationResponse_objectTypeNames' - A map in which each key is an event type from an external application
-- such as Segment or Shopify, and each value is an @ObjectTypeName@
-- (template) used to ingest the event. It supports the following event
-- types: @SegmentIdentify@, @ShopifyCreateCustomers@,
-- @ShopifyUpdateCustomers@, @ShopifyCreateDraftOrders@,
-- @ShopifyUpdateDraftOrders@, @ShopifyCreateOrders@, and
-- @ShopifyUpdatedOrders@.
--
-- 'tags', 'getIntegrationResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'workflowId', 'getIntegrationResponse_workflowId' - Unique identifier for the workflow.
--
-- 'httpStatus', 'getIntegrationResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'getIntegrationResponse_domainName' - The unique name of the domain.
--
-- 'uri', 'getIntegrationResponse_uri' - The URI of the S3 bucket or any other type of data source.
--
-- 'createdAt', 'getIntegrationResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'getIntegrationResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newGetIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'uri'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  GetIntegrationResponse
newGetIntegrationResponse
  pHttpStatus_
  pDomainName_
  pUri_
  pCreatedAt_
  pLastUpdatedAt_ =
    GetIntegrationResponse'
      { isUnstructured =
          Prelude.Nothing,
        objectTypeName = Prelude.Nothing,
        objectTypeNames = Prelude.Nothing,
        tags = Prelude.Nothing,
        workflowId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        uri = pUri_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_
      }

-- | Boolean to indicate if the Flow associated with the Integration is
-- created via Appflow console or with ObjectTypeName equals _unstructured
-- via API\/CLI in flowDefinition
getIntegrationResponse_isUnstructured :: Lens.Lens' GetIntegrationResponse (Prelude.Maybe Prelude.Bool)
getIntegrationResponse_isUnstructured = Lens.lens (\GetIntegrationResponse' {isUnstructured} -> isUnstructured) (\s@GetIntegrationResponse' {} a -> s {isUnstructured = a} :: GetIntegrationResponse)

-- | The name of the profile object type.
getIntegrationResponse_objectTypeName :: Lens.Lens' GetIntegrationResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponse_objectTypeName = Lens.lens (\GetIntegrationResponse' {objectTypeName} -> objectTypeName) (\s@GetIntegrationResponse' {} a -> s {objectTypeName = a} :: GetIntegrationResponse)

-- | A map in which each key is an event type from an external application
-- such as Segment or Shopify, and each value is an @ObjectTypeName@
-- (template) used to ingest the event. It supports the following event
-- types: @SegmentIdentify@, @ShopifyCreateCustomers@,
-- @ShopifyUpdateCustomers@, @ShopifyCreateDraftOrders@,
-- @ShopifyUpdateDraftOrders@, @ShopifyCreateOrders@, and
-- @ShopifyUpdatedOrders@.
getIntegrationResponse_objectTypeNames :: Lens.Lens' GetIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponse_objectTypeNames = Lens.lens (\GetIntegrationResponse' {objectTypeNames} -> objectTypeNames) (\s@GetIntegrationResponse' {} a -> s {objectTypeNames = a} :: GetIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
getIntegrationResponse_tags :: Lens.Lens' GetIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponse_tags = Lens.lens (\GetIntegrationResponse' {tags} -> tags) (\s@GetIntegrationResponse' {} a -> s {tags = a} :: GetIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier for the workflow.
getIntegrationResponse_workflowId :: Lens.Lens' GetIntegrationResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponse_workflowId = Lens.lens (\GetIntegrationResponse' {workflowId} -> workflowId) (\s@GetIntegrationResponse' {} a -> s {workflowId = a} :: GetIntegrationResponse)

-- | The response's http status code.
getIntegrationResponse_httpStatus :: Lens.Lens' GetIntegrationResponse Prelude.Int
getIntegrationResponse_httpStatus = Lens.lens (\GetIntegrationResponse' {httpStatus} -> httpStatus) (\s@GetIntegrationResponse' {} a -> s {httpStatus = a} :: GetIntegrationResponse)

-- | The unique name of the domain.
getIntegrationResponse_domainName :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_domainName = Lens.lens (\GetIntegrationResponse' {domainName} -> domainName) (\s@GetIntegrationResponse' {} a -> s {domainName = a} :: GetIntegrationResponse)

-- | The URI of the S3 bucket or any other type of data source.
getIntegrationResponse_uri :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_uri = Lens.lens (\GetIntegrationResponse' {uri} -> uri) (\s@GetIntegrationResponse' {} a -> s {uri = a} :: GetIntegrationResponse)

-- | The timestamp of when the domain was created.
getIntegrationResponse_createdAt :: Lens.Lens' GetIntegrationResponse Prelude.UTCTime
getIntegrationResponse_createdAt = Lens.lens (\GetIntegrationResponse' {createdAt} -> createdAt) (\s@GetIntegrationResponse' {} a -> s {createdAt = a} :: GetIntegrationResponse) Prelude.. Data._Time

-- | The timestamp of when the domain was most recently edited.
getIntegrationResponse_lastUpdatedAt :: Lens.Lens' GetIntegrationResponse Prelude.UTCTime
getIntegrationResponse_lastUpdatedAt = Lens.lens (\GetIntegrationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetIntegrationResponse' {} a -> s {lastUpdatedAt = a} :: GetIntegrationResponse) Prelude.. Data._Time

instance Prelude.NFData GetIntegrationResponse where
  rnf GetIntegrationResponse' {..} =
    Prelude.rnf isUnstructured
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf objectTypeNames
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
