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
-- Module      : Network.AWS.SSM.DeleteInventory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom inventory type or the data associated with a custom
-- Inventory type. Deleting a custom inventory type is also referred to as
-- deleting a custom inventory schema.
module Network.AWS.SSM.DeleteInventory
  ( -- * Creating a Request
    DeleteInventory (..),
    newDeleteInventory,

    -- * Request Lenses
    deleteInventory_dryRun,
    deleteInventory_schemaDeleteOption,
    deleteInventory_clientToken,
    deleteInventory_typeName,

    -- * Destructuring the Response
    DeleteInventoryResponse (..),
    newDeleteInventoryResponse,

    -- * Response Lenses
    deleteInventoryResponse_typeName,
    deleteInventoryResponse_deletionId,
    deleteInventoryResponse_deletionSummary,
    deleteInventoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteInventory' smart constructor.
data DeleteInventory = DeleteInventory'
  { -- | Use this option to view a summary of the deletion request without
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
    -- enable this schema again, you must call the @PutInventory@ action for a
    -- version greater than the disabled version.
    --
    -- DeleteSchema: This option deletes the specified custom type from the
    -- Inventory service. You can recreate the schema later, if you want.
    schemaDeleteOption :: Prelude.Maybe InventorySchemaDeleteOption,
    -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
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
-- enable this schema again, you must call the @PutInventory@ action for a
-- version greater than the disabled version.
--
-- DeleteSchema: This option deletes the specified custom type from the
-- Inventory service. You can recreate the schema later, if you want.
--
-- 'clientToken', 'deleteInventory_clientToken' - User-provided idempotency token.
--
-- 'typeName', 'deleteInventory_typeName' - The name of the custom inventory type for which you want to delete
-- either all previously collected data or the inventory type itself.
newDeleteInventory ::
  -- | 'typeName'
  Prelude.Text ->
  DeleteInventory
newDeleteInventory pTypeName_ =
  DeleteInventory'
    { dryRun = Prelude.Nothing,
      schemaDeleteOption = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      typeName = pTypeName_
    }

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
-- enable this schema again, you must call the @PutInventory@ action for a
-- version greater than the disabled version.
--
-- DeleteSchema: This option deletes the specified custom type from the
-- Inventory service. You can recreate the schema later, if you want.
deleteInventory_schemaDeleteOption :: Lens.Lens' DeleteInventory (Prelude.Maybe InventorySchemaDeleteOption)
deleteInventory_schemaDeleteOption = Lens.lens (\DeleteInventory' {schemaDeleteOption} -> schemaDeleteOption) (\s@DeleteInventory' {} a -> s {schemaDeleteOption = a} :: DeleteInventory)

-- | User-provided idempotency token.
deleteInventory_clientToken :: Lens.Lens' DeleteInventory (Prelude.Maybe Prelude.Text)
deleteInventory_clientToken = Lens.lens (\DeleteInventory' {clientToken} -> clientToken) (\s@DeleteInventory' {} a -> s {clientToken = a} :: DeleteInventory)

-- | The name of the custom inventory type for which you want to delete
-- either all previously collected data or the inventory type itself.
deleteInventory_typeName :: Lens.Lens' DeleteInventory Prelude.Text
deleteInventory_typeName = Lens.lens (\DeleteInventory' {typeName} -> typeName) (\s@DeleteInventory' {} a -> s {typeName = a} :: DeleteInventory)

instance Core.AWSRequest DeleteInventory where
  type
    AWSResponse DeleteInventory =
      DeleteInventoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInventoryResponse'
            Prelude.<$> (x Core..?> "TypeName")
            Prelude.<*> (x Core..?> "DeletionId")
            Prelude.<*> (x Core..?> "DeletionSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInventory

instance Prelude.NFData DeleteInventory

instance Core.ToHeaders DeleteInventory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DeleteInventory" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteInventory where
  toJSON DeleteInventory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DryRun" Core..=) Prelude.<$> dryRun,
            ("SchemaDeleteOption" Core..=)
              Prelude.<$> schemaDeleteOption,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("TypeName" Core..= typeName)
          ]
      )

instance Core.ToPath DeleteInventory where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteInventory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInventoryResponse' smart constructor.
data DeleteInventoryResponse = DeleteInventoryResponse'
  { -- | The name of the inventory data type specified in the request.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | Every @DeleteInventory@ action is assigned a unique ID. This option
    -- returns a unique ID. You can use this ID to query the status of a delete
    -- operation. This option is useful for ensuring that a delete operation
    -- has completed before you begin other actions.
    deletionId :: Prelude.Maybe Prelude.Text,
    -- | A summary of the delete operation. For more information about this
    -- summary, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
    -- in the /AWS Systems Manager User Guide/.
    deletionSummary :: Prelude.Maybe InventoryDeletionSummary,
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
-- 'typeName', 'deleteInventoryResponse_typeName' - The name of the inventory data type specified in the request.
--
-- 'deletionId', 'deleteInventoryResponse_deletionId' - Every @DeleteInventory@ action is assigned a unique ID. This option
-- returns a unique ID. You can use this ID to query the status of a delete
-- operation. This option is useful for ensuring that a delete operation
-- has completed before you begin other actions.
--
-- 'deletionSummary', 'deleteInventoryResponse_deletionSummary' - A summary of the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
-- in the /AWS Systems Manager User Guide/.
--
-- 'httpStatus', 'deleteInventoryResponse_httpStatus' - The response's http status code.
newDeleteInventoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInventoryResponse
newDeleteInventoryResponse pHttpStatus_ =
  DeleteInventoryResponse'
    { typeName =
        Prelude.Nothing,
      deletionId = Prelude.Nothing,
      deletionSummary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the inventory data type specified in the request.
deleteInventoryResponse_typeName :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe Prelude.Text)
deleteInventoryResponse_typeName = Lens.lens (\DeleteInventoryResponse' {typeName} -> typeName) (\s@DeleteInventoryResponse' {} a -> s {typeName = a} :: DeleteInventoryResponse)

-- | Every @DeleteInventory@ action is assigned a unique ID. This option
-- returns a unique ID. You can use this ID to query the status of a delete
-- operation. This option is useful for ensuring that a delete operation
-- has completed before you begin other actions.
deleteInventoryResponse_deletionId :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe Prelude.Text)
deleteInventoryResponse_deletionId = Lens.lens (\DeleteInventoryResponse' {deletionId} -> deletionId) (\s@DeleteInventoryResponse' {} a -> s {deletionId = a} :: DeleteInventoryResponse)

-- | A summary of the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory>
-- in the /AWS Systems Manager User Guide/.
deleteInventoryResponse_deletionSummary :: Lens.Lens' DeleteInventoryResponse (Prelude.Maybe InventoryDeletionSummary)
deleteInventoryResponse_deletionSummary = Lens.lens (\DeleteInventoryResponse' {deletionSummary} -> deletionSummary) (\s@DeleteInventoryResponse' {} a -> s {deletionSummary = a} :: DeleteInventoryResponse)

-- | The response's http status code.
deleteInventoryResponse_httpStatus :: Lens.Lens' DeleteInventoryResponse Prelude.Int
deleteInventoryResponse_httpStatus = Lens.lens (\DeleteInventoryResponse' {httpStatus} -> httpStatus) (\s@DeleteInventoryResponse' {} a -> s {httpStatus = a} :: DeleteInventoryResponse)

instance Prelude.NFData DeleteInventoryResponse
