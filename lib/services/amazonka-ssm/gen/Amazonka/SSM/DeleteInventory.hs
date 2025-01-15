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
-- Module      : Amazonka.SSM.DeleteInventory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom inventory type or the data associated with a custom
-- Inventory type. Deleting a custom inventory type is also referred to as
-- deleting a custom inventory schema.
module Amazonka.SSM.DeleteInventory
  ( -- * Creating a Request
    DeleteInventory (..),
    newDeleteInventory,

    -- * Request Lenses
    deleteInventory_clientToken,
    deleteInventory_dryRun,
    deleteInventory_schemaDeleteOption,
    deleteInventory_typeName,

    -- * Destructuring the Response
    DeleteInventoryResponse (..),
    newDeleteInventoryResponse,

    -- * Response Lenses
    deleteInventoryResponse_deletionId,
    deleteInventoryResponse_deletionSummary,
    deleteInventoryResponse_typeName,
    deleteInventoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteInventory' smart constructor.
data DeleteInventory = DeleteInventory'
  { -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Use this option to view a summary of the deletion request without
    -- deleting any data or the data type. This option is useful when you only
    -- want to understand what will be deleted. Once you validate that the data
    -- to be deleted is what you intend to delete, you can run the same command
    -- without specifying the @DryRun@ option.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Use the @SchemaDeleteOption@ to delete a custom inventory type (schema).
    -- If you don\'t choose this option, the system only deletes existing
    -- inventory data associated with the custom inventory type. Choose one of
    -- the following options:
    --
    -- DisableSchema: If you choose this option, the system ignores all
    -- inventory data for the specified version, and any earlier versions. To
    -- enable this schema again, you must call the @PutInventory@ operation for
    -- a version greater than the disabled version.
    --
    -- DeleteSchema: This option deletes the specified custom type from the
    -- Inventory service. You can recreate the schema later, if you want.
    schemaDeleteOption :: Prelude.Maybe InventorySchemaDeleteOption,
    -- | The name of the custom inventory type for which you want to delete
    -- either all previously collected data or the inventory type itself.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteInventory_clientToken' - User-provided idempotency token.
--
-- 'dryRun', 'deleteInventory_dryRun' - Use this option to view a summary of the deletion request without
-- deleting any data or the data type. This option is useful when you only
-- want to understand what will be deleted. Once you validate that the data
-- to be deleted is what you intend to delete, you can run the same command
-- without specifying the @DryRun@ option.
--
-- 'schemaDeleteOption', 'deleteInventory_schemaDeleteOption' - Use the @SchemaDeleteOption@ to delete a custom inventory type (schema).
-- If you don\'t choose this option, the system only deletes existing
-- inventory data associated with the custom inventory type. Choose one of
-- the following options:
--
-- DisableSchema: If you choose this option, the system ignores all
-- inventory data for the specified version, and any earlier versions. To
-- enable this schema again, you must call the @PutInventory@ operation for
-- a version greater than the disabled version.
--
-- DeleteSchema: This option deletes the specified custom type from the
-- Inventory service. You can recreate the schema later, if you want.
--
-- 'typeName', 'deleteInventory_typeName' - The name of the custom inventory type for which you want to delete
-- either all previously collected data or the inventory type itself.
newDeleteInventory ::
  -- | 'typeName'
  Prelude.Text ->
  DeleteInventory
newDeleteInventory pTypeName_ =
  DeleteInventory'
    { clientToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      schemaDeleteOption = Prelude.Nothing,
      typeName = pTypeName_
    }

-- | User-provided idempotency token.
deleteInventory_clientToken :: Lens.Lens' DeleteInventory (Prelude.Maybe Prelude.Text)
deleteInventory_clientToken = Lens.lens (\DeleteInventory' {clientToken} -> clientToken) (\s@DeleteInventory' {} a -> s {clientToken = a} :: DeleteInventory)

-- | Use this option to view a summary of the deletion request without
-- deleting any data or the data type. This option is useful when you only
-- want to understand what will be deleted. Once you validate that the data
-- to be deleted is what you intend to delete, you can run the same command
-- without specifying the @DryRun@ option.
deleteInventory_dryRun :: Lens.Lens' DeleteInventory (Prelude.Maybe Prelude.Bool)
deleteInventory_dryRun = Lens.lens (\DeleteInventory' {dryRun} -> dryRun) (\s@DeleteInventory' {} a -> s {dryRun = a} :: DeleteInventory)

-- | Use the @SchemaDeleteOption@ to delete a custom inventory type (schema).
-- If you don\'t choose this option, the system only deletes existing
-- inventory data associated with the custom inventory type. Choose one of
-- the following options:
--
-- DisableSchema: If you choose this option, the system ignores all
-- inventory data for the specified version, and any earlier versions. To
-- enable this schema again, you must call the @PutInventory@ operation for
-- a version greater than the disabled version.
--
-- DeleteSchema: This option deletes the specified custom type from the
-- Inventory service. You can recreate the schema later, if you want.
deleteInventory_schemaDeleteOption :: Lens.Lens' DeleteInventory (Prelude.Maybe InventorySchemaDeleteOption)
deleteInventory_schemaDeleteOption = Lens.lens (\DeleteInventory' {schemaDeleteOption} -> schemaDeleteOption) (\s@DeleteInventory' {} a -> s {schemaDeleteOption = a} :: DeleteInventory)

-- | The name of the custom inventory type for which you want to delete
-- either all previously collected data or the inventory type itself.
deleteInventory_typeName :: Lens.Lens' DeleteInventory Prelude.Text
deleteInventory_typeName = Lens.lens (\DeleteInventory' {typeName} -> typeName) (\s@DeleteInventory' {} a -> s {typeName = a} :: DeleteInventory)

instance Core.AWSRequest DeleteInventory where
  type
    AWSResponse DeleteInventory =
      DeleteInventoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInventoryResponse'
            Prelude.<$> (x Data..?> "DeletionId")
            Prelude.<*> (x Data..?> "DeletionSummary")
            Prelude.<*> (x Data..?> "TypeName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInventory where
  hashWithSalt _salt DeleteInventory' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` schemaDeleteOption
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData DeleteInventory where
  rnf DeleteInventory' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf schemaDeleteOption `Prelude.seq`
          Prelude.rnf typeName

instance Data.ToHeaders DeleteInventory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DeleteInventory" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInventory where
  toJSON DeleteInventory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("SchemaDeleteOption" Data..=)
              Prelude.<$> schemaDeleteOption,
            Prelude.Just ("TypeName" Data..= typeName)
          ]
      )

instance Data.ToPath DeleteInventory where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInventory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInventoryResponse' smart constructor.
data DeleteInventoryResponse = DeleteInventoryResponse'
  { -- | Every @DeleteInventory@ operation is assigned a unique ID. This option
    -- returns a unique ID. You can use this ID to query the status of a delete
    -- operation. This option is useful for ensuring that a delete operation
    -- has completed before you begin other operations.
    deletionId :: Prelude.Maybe Prelude.Text,
    -- | A summary of the delete operation. For more information about this
    -- summary, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    deletionSummary :: Prelude.Maybe InventoryDeletionSummary,
    -- | The name of the inventory data type specified in the request.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInventoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionId', 'deleteInventoryResponse_deletionId' - Every @DeleteInventory@ operation is assigned a unique ID. This option
-- returns a unique ID. You can use this ID to query the status of a delete
-- operation. This option is useful for ensuring that a delete operation
-- has completed before you begin other operations.
--
-- 'deletionSummary', 'deleteInventoryResponse_deletionSummary' - A summary of the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'typeName', 'deleteInventoryResponse_typeName' - The name of the inventory data type specified in the request.
--
-- 'httpStatus', 'deleteInventoryResponse_httpStatus' - The response's http status code.
newDeleteInventoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInventoryResponse
newDeleteInventoryResponse pHttpStatus_ =
  DeleteInventoryResponse'
    { deletionId =
        Prelude.Nothing,
      deletionSummary = Prelude.Nothing,
      typeName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Every @DeleteInventory@ operation is assigned a unique ID. This option
-- returns a unique ID. You can use this ID to query the status of a delete
-- operation. This option is useful for ensuring that a delete operation
-- has completed before you begin other operations.
deleteInventoryResponse_deletionId :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe Prelude.Text)
deleteInventoryResponse_deletionId = Lens.lens (\DeleteInventoryResponse' {deletionId} -> deletionId) (\s@DeleteInventoryResponse' {} a -> s {deletionId = a} :: DeleteInventoryResponse)

-- | A summary of the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
-- in the /Amazon Web Services Systems Manager User Guide/.
deleteInventoryResponse_deletionSummary :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe InventoryDeletionSummary)
deleteInventoryResponse_deletionSummary = Lens.lens (\DeleteInventoryResponse' {deletionSummary} -> deletionSummary) (\s@DeleteInventoryResponse' {} a -> s {deletionSummary = a} :: DeleteInventoryResponse)

-- | The name of the inventory data type specified in the request.
deleteInventoryResponse_typeName :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe Prelude.Text)
deleteInventoryResponse_typeName = Lens.lens (\DeleteInventoryResponse' {typeName} -> typeName) (\s@DeleteInventoryResponse' {} a -> s {typeName = a} :: DeleteInventoryResponse)

-- | The response's http status code.
deleteInventoryResponse_httpStatus :: Lens.Lens' DeleteInventoryResponse Prelude.Int
deleteInventoryResponse_httpStatus = Lens.lens (\DeleteInventoryResponse' {httpStatus} -> httpStatus) (\s@DeleteInventoryResponse' {} a -> s {httpStatus = a} :: DeleteInventoryResponse)

instance Prelude.NFData DeleteInventoryResponse where
  rnf DeleteInventoryResponse' {..} =
    Prelude.rnf deletionId `Prelude.seq`
      Prelude.rnf deletionSummary `Prelude.seq`
        Prelude.rnf typeName `Prelude.seq`
          Prelude.rnf httpStatus
