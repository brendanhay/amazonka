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
-- Module      : Amazonka.SSM.PutComplianceItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a compliance type and other compliance details on a designated
-- resource. This operation lets you register custom compliance details
-- with a resource. This call overwrites existing compliance information on
-- the resource, so you must provide a full list of compliance items each
-- time that you send the request.
--
-- ComplianceType can be one of the following:
--
-- -   ExecutionId: The execution ID when the patch, association, or custom
--     compliance item was applied.
--
-- -   ExecutionType: Specify patch, association, or Custom:@string@.
--
-- -   ExecutionTime. The time the patch, association, or custom compliance
--     item was applied to the managed node.
--
-- -   Id: The patch, association, or custom compliance ID.
--
-- -   Title: A title.
--
-- -   Status: The status of the compliance item. For example, @approved@
--     for patches, or @Failed@ for associations.
--
-- -   Severity: A patch severity. For example, @Critical@.
--
-- -   DocumentName: An SSM document name. For example,
--     @AWS-RunPatchBaseline@.
--
-- -   DocumentVersion: An SSM document version number. For example, 4.
--
-- -   Classification: A patch classification. For example,
--     @security updates@.
--
-- -   PatchBaselineId: A patch baseline ID.
--
-- -   PatchSeverity: A patch severity. For example, @Critical@.
--
-- -   PatchState: A patch state. For example,
--     @InstancesWithFailedPatches@.
--
-- -   PatchGroup: The name of a patch group.
--
-- -   InstalledTime: The time the association, patch, or custom compliance
--     item was applied to the resource. Specify the time by using the
--     following format: yyyy-MM-dd\'T\'HH:mm:ss\'Z\'
module Amazonka.SSM.PutComplianceItems
  ( -- * Creating a Request
    PutComplianceItems (..),
    newPutComplianceItems,

    -- * Request Lenses
    putComplianceItems_itemContentHash,
    putComplianceItems_uploadType,
    putComplianceItems_resourceId,
    putComplianceItems_resourceType,
    putComplianceItems_complianceType,
    putComplianceItems_executionSummary,
    putComplianceItems_items,

    -- * Destructuring the Response
    PutComplianceItemsResponse (..),
    newPutComplianceItemsResponse,

    -- * Response Lenses
    putComplianceItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newPutComplianceItems' smart constructor.
data PutComplianceItems = PutComplianceItems'
  { -- | MD5 or SHA-256 content hash. The content hash is used to determine if
    -- existing information should be overwritten or ignored. If the content
    -- hashes match, the request to put compliance information is ignored.
    itemContentHash :: Prelude.Maybe Prelude.Text,
    -- | The mode for uploading compliance items. You can specify @COMPLETE@ or
    -- @PARTIAL@. In @COMPLETE@ mode, the system overwrites all existing
    -- compliance information for the resource. You must provide a full list of
    -- compliance items each time you send the request.
    --
    -- In @PARTIAL@ mode, the system overwrites compliance information for a
    -- specific association. The association must be configured with
    -- @SyncCompliance@ set to @MANUAL@. By default, all requests use
    -- @COMPLETE@ mode.
    --
    -- This attribute is only valid for association compliance.
    uploadType :: Prelude.Maybe ComplianceUploadType,
    -- | Specify an ID for this resource. For a managed node, this is the node
    -- ID.
    resourceId :: Prelude.Text,
    -- | Specify the type of resource. @ManagedInstance@ is currently the only
    -- supported resource type.
    resourceType :: Prelude.Text,
    -- | Specify the compliance type. For example, specify Association (for a
    -- State Manager association), Patch, or Custom:@string@.
    complianceType :: Prelude.Text,
    -- | A summary of the call execution that includes an execution ID, the type
    -- of execution (for example, @Command@), and the date\/time of the
    -- execution using a datetime object that is saved in the following format:
    -- yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
    executionSummary :: ComplianceExecutionSummary,
    -- | Information about the compliance as defined by the resource type. For
    -- example, for a patch compliance type, @Items@ includes information about
    -- the PatchSeverity, Classification, and so on.
    items :: [ComplianceItemEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutComplianceItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemContentHash', 'putComplianceItems_itemContentHash' - MD5 or SHA-256 content hash. The content hash is used to determine if
-- existing information should be overwritten or ignored. If the content
-- hashes match, the request to put compliance information is ignored.
--
-- 'uploadType', 'putComplianceItems_uploadType' - The mode for uploading compliance items. You can specify @COMPLETE@ or
-- @PARTIAL@. In @COMPLETE@ mode, the system overwrites all existing
-- compliance information for the resource. You must provide a full list of
-- compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a
-- specific association. The association must be configured with
-- @SyncCompliance@ set to @MANUAL@. By default, all requests use
-- @COMPLETE@ mode.
--
-- This attribute is only valid for association compliance.
--
-- 'resourceId', 'putComplianceItems_resourceId' - Specify an ID for this resource. For a managed node, this is the node
-- ID.
--
-- 'resourceType', 'putComplianceItems_resourceType' - Specify the type of resource. @ManagedInstance@ is currently the only
-- supported resource type.
--
-- 'complianceType', 'putComplianceItems_complianceType' - Specify the compliance type. For example, specify Association (for a
-- State Manager association), Patch, or Custom:@string@.
--
-- 'executionSummary', 'putComplianceItems_executionSummary' - A summary of the call execution that includes an execution ID, the type
-- of execution (for example, @Command@), and the date\/time of the
-- execution using a datetime object that is saved in the following format:
-- yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
--
-- 'items', 'putComplianceItems_items' - Information about the compliance as defined by the resource type. For
-- example, for a patch compliance type, @Items@ includes information about
-- the PatchSeverity, Classification, and so on.
newPutComplianceItems ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'complianceType'
  Prelude.Text ->
  -- | 'executionSummary'
  ComplianceExecutionSummary ->
  PutComplianceItems
newPutComplianceItems
  pResourceId_
  pResourceType_
  pComplianceType_
  pExecutionSummary_ =
    PutComplianceItems'
      { itemContentHash =
          Prelude.Nothing,
        uploadType = Prelude.Nothing,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        complianceType = pComplianceType_,
        executionSummary = pExecutionSummary_,
        items = Prelude.mempty
      }

-- | MD5 or SHA-256 content hash. The content hash is used to determine if
-- existing information should be overwritten or ignored. If the content
-- hashes match, the request to put compliance information is ignored.
putComplianceItems_itemContentHash :: Lens.Lens' PutComplianceItems (Prelude.Maybe Prelude.Text)
putComplianceItems_itemContentHash = Lens.lens (\PutComplianceItems' {itemContentHash} -> itemContentHash) (\s@PutComplianceItems' {} a -> s {itemContentHash = a} :: PutComplianceItems)

-- | The mode for uploading compliance items. You can specify @COMPLETE@ or
-- @PARTIAL@. In @COMPLETE@ mode, the system overwrites all existing
-- compliance information for the resource. You must provide a full list of
-- compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a
-- specific association. The association must be configured with
-- @SyncCompliance@ set to @MANUAL@. By default, all requests use
-- @COMPLETE@ mode.
--
-- This attribute is only valid for association compliance.
putComplianceItems_uploadType :: Lens.Lens' PutComplianceItems (Prelude.Maybe ComplianceUploadType)
putComplianceItems_uploadType = Lens.lens (\PutComplianceItems' {uploadType} -> uploadType) (\s@PutComplianceItems' {} a -> s {uploadType = a} :: PutComplianceItems)

-- | Specify an ID for this resource. For a managed node, this is the node
-- ID.
putComplianceItems_resourceId :: Lens.Lens' PutComplianceItems Prelude.Text
putComplianceItems_resourceId = Lens.lens (\PutComplianceItems' {resourceId} -> resourceId) (\s@PutComplianceItems' {} a -> s {resourceId = a} :: PutComplianceItems)

-- | Specify the type of resource. @ManagedInstance@ is currently the only
-- supported resource type.
putComplianceItems_resourceType :: Lens.Lens' PutComplianceItems Prelude.Text
putComplianceItems_resourceType = Lens.lens (\PutComplianceItems' {resourceType} -> resourceType) (\s@PutComplianceItems' {} a -> s {resourceType = a} :: PutComplianceItems)

-- | Specify the compliance type. For example, specify Association (for a
-- State Manager association), Patch, or Custom:@string@.
putComplianceItems_complianceType :: Lens.Lens' PutComplianceItems Prelude.Text
putComplianceItems_complianceType = Lens.lens (\PutComplianceItems' {complianceType} -> complianceType) (\s@PutComplianceItems' {} a -> s {complianceType = a} :: PutComplianceItems)

-- | A summary of the call execution that includes an execution ID, the type
-- of execution (for example, @Command@), and the date\/time of the
-- execution using a datetime object that is saved in the following format:
-- yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
putComplianceItems_executionSummary :: Lens.Lens' PutComplianceItems ComplianceExecutionSummary
putComplianceItems_executionSummary = Lens.lens (\PutComplianceItems' {executionSummary} -> executionSummary) (\s@PutComplianceItems' {} a -> s {executionSummary = a} :: PutComplianceItems)

-- | Information about the compliance as defined by the resource type. For
-- example, for a patch compliance type, @Items@ includes information about
-- the PatchSeverity, Classification, and so on.
putComplianceItems_items :: Lens.Lens' PutComplianceItems [ComplianceItemEntry]
putComplianceItems_items = Lens.lens (\PutComplianceItems' {items} -> items) (\s@PutComplianceItems' {} a -> s {items = a} :: PutComplianceItems) Prelude.. Lens.coerced

instance Core.AWSRequest PutComplianceItems where
  type
    AWSResponse PutComplianceItems =
      PutComplianceItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutComplianceItemsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutComplianceItems where
  hashWithSalt _salt PutComplianceItems' {..} =
    _salt `Prelude.hashWithSalt` itemContentHash
      `Prelude.hashWithSalt` uploadType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` executionSummary
      `Prelude.hashWithSalt` items

instance Prelude.NFData PutComplianceItems where
  rnf PutComplianceItems' {..} =
    Prelude.rnf itemContentHash
      `Prelude.seq` Prelude.rnf uploadType
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf executionSummary
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders PutComplianceItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.PutComplianceItems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutComplianceItems where
  toJSON PutComplianceItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ItemContentHash" Data..=)
              Prelude.<$> itemContentHash,
            ("UploadType" Data..=) Prelude.<$> uploadType,
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just
              ("ComplianceType" Data..= complianceType),
            Prelude.Just
              ("ExecutionSummary" Data..= executionSummary),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath PutComplianceItems where
  toPath = Prelude.const "/"

instance Data.ToQuery PutComplianceItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutComplianceItemsResponse' smart constructor.
data PutComplianceItemsResponse = PutComplianceItemsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutComplianceItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putComplianceItemsResponse_httpStatus' - The response's http status code.
newPutComplianceItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutComplianceItemsResponse
newPutComplianceItemsResponse pHttpStatus_ =
  PutComplianceItemsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putComplianceItemsResponse_httpStatus :: Lens.Lens' PutComplianceItemsResponse Prelude.Int
putComplianceItemsResponse_httpStatus = Lens.lens (\PutComplianceItemsResponse' {httpStatus} -> httpStatus) (\s@PutComplianceItemsResponse' {} a -> s {httpStatus = a} :: PutComplianceItemsResponse)

instance Prelude.NFData PutComplianceItemsResponse where
  rnf PutComplianceItemsResponse' {..} =
    Prelude.rnf httpStatus
