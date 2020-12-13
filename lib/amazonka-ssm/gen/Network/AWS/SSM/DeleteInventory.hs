{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteInventory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom inventory type or the data associated with a custom Inventory type. Deleting a custom inventory type is also referred to as deleting a custom inventory schema.
module Network.AWS.SSM.DeleteInventory
  ( -- * Creating a request
    DeleteInventory (..),
    mkDeleteInventory,

    -- ** Request lenses
    diTypeName,
    diClientToken,
    diSchemaDeleteOption,
    diDryRun,

    -- * Destructuring the response
    DeleteInventoryResponse (..),
    mkDeleteInventoryResponse,

    -- ** Response lenses
    dirsTypeName,
    dirsDeletionSummary,
    dirsDeletionId,
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteInventory' smart constructor.
data DeleteInventory = DeleteInventory'
  { -- | The name of the custom inventory type for which you want to delete either all previously collected data or the inventory type itself.
    typeName :: Lude.Text,
    -- | User-provided idempotency token.
    clientToken :: Lude.Maybe Lude.Text,
    -- | Use the @SchemaDeleteOption@ to delete a custom inventory type (schema). If you don't choose this option, the system only deletes existing inventory data associated with the custom inventory type. Choose one of the following options:
    --
    -- DisableSchema: If you choose this option, the system ignores all inventory data for the specified version, and any earlier versions. To enable this schema again, you must call the @PutInventory@ action for a version greater than the disabled version.
    -- DeleteSchema: This option deletes the specified custom type from the Inventory service. You can recreate the schema later, if you want.
    schemaDeleteOption :: Lude.Maybe InventorySchemaDeleteOption,
    -- | Use this option to view a summary of the deletion request without deleting any data or the data type. This option is useful when you only want to understand what will be deleted. Once you validate that the data to be deleted is what you intend to delete, you can run the same command without specifying the @DryRun@ option.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInventory' with the minimum fields required to make a request.
--
-- * 'typeName' - The name of the custom inventory type for which you want to delete either all previously collected data or the inventory type itself.
-- * 'clientToken' - User-provided idempotency token.
-- * 'schemaDeleteOption' - Use the @SchemaDeleteOption@ to delete a custom inventory type (schema). If you don't choose this option, the system only deletes existing inventory data associated with the custom inventory type. Choose one of the following options:
--
-- DisableSchema: If you choose this option, the system ignores all inventory data for the specified version, and any earlier versions. To enable this schema again, you must call the @PutInventory@ action for a version greater than the disabled version.
-- DeleteSchema: This option deletes the specified custom type from the Inventory service. You can recreate the schema later, if you want.
-- * 'dryRun' - Use this option to view a summary of the deletion request without deleting any data or the data type. This option is useful when you only want to understand what will be deleted. Once you validate that the data to be deleted is what you intend to delete, you can run the same command without specifying the @DryRun@ option.
mkDeleteInventory ::
  -- | 'typeName'
  Lude.Text ->
  DeleteInventory
mkDeleteInventory pTypeName_ =
  DeleteInventory'
    { typeName = pTypeName_,
      clientToken = Lude.Nothing,
      schemaDeleteOption = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The name of the custom inventory type for which you want to delete either all previously collected data or the inventory type itself.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTypeName :: Lens.Lens' DeleteInventory Lude.Text
diTypeName = Lens.lens (typeName :: DeleteInventory -> Lude.Text) (\s a -> s {typeName = a} :: DeleteInventory)
{-# DEPRECATED diTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diClientToken :: Lens.Lens' DeleteInventory (Lude.Maybe Lude.Text)
diClientToken = Lens.lens (clientToken :: DeleteInventory -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: DeleteInventory)
{-# DEPRECATED diClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Use the @SchemaDeleteOption@ to delete a custom inventory type (schema). If you don't choose this option, the system only deletes existing inventory data associated with the custom inventory type. Choose one of the following options:
--
-- DisableSchema: If you choose this option, the system ignores all inventory data for the specified version, and any earlier versions. To enable this schema again, you must call the @PutInventory@ action for a version greater than the disabled version.
-- DeleteSchema: This option deletes the specified custom type from the Inventory service. You can recreate the schema later, if you want.
--
-- /Note:/ Consider using 'schemaDeleteOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSchemaDeleteOption :: Lens.Lens' DeleteInventory (Lude.Maybe InventorySchemaDeleteOption)
diSchemaDeleteOption = Lens.lens (schemaDeleteOption :: DeleteInventory -> Lude.Maybe InventorySchemaDeleteOption) (\s a -> s {schemaDeleteOption = a} :: DeleteInventory)
{-# DEPRECATED diSchemaDeleteOption "Use generic-lens or generic-optics with 'schemaDeleteOption' instead." #-}

-- | Use this option to view a summary of the deletion request without deleting any data or the data type. This option is useful when you only want to understand what will be deleted. Once you validate that the data to be deleted is what you intend to delete, you can run the same command without specifying the @DryRun@ option.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDryRun :: Lens.Lens' DeleteInventory (Lude.Maybe Lude.Bool)
diDryRun = Lens.lens (dryRun :: DeleteInventory -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteInventory)
{-# DEPRECATED diDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteInventory where
  type Rs DeleteInventory = DeleteInventoryResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInventoryResponse'
            Lude.<$> (x Lude..?> "TypeName")
            Lude.<*> (x Lude..?> "DeletionSummary")
            Lude.<*> (x Lude..?> "DeletionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInventory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteInventory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInventory where
  toJSON DeleteInventory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TypeName" Lude..= typeName),
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("SchemaDeleteOption" Lude..=) Lude.<$> schemaDeleteOption,
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath DeleteInventory where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInventory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInventoryResponse' smart constructor.
data DeleteInventoryResponse = DeleteInventoryResponse'
  { -- | The name of the inventory data type specified in the request.
    typeName :: Lude.Maybe Lude.Text,
    -- | A summary of the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory> in the /AWS Systems Manager User Guide/ .
    deletionSummary :: Lude.Maybe InventoryDeletionSummary,
    -- | Every @DeleteInventory@ action is assigned a unique ID. This option returns a unique ID. You can use this ID to query the status of a delete operation. This option is useful for ensuring that a delete operation has completed before you begin other actions.
    deletionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInventoryResponse' with the minimum fields required to make a request.
--
-- * 'typeName' - The name of the inventory data type specified in the request.
-- * 'deletionSummary' - A summary of the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory> in the /AWS Systems Manager User Guide/ .
-- * 'deletionId' - Every @DeleteInventory@ action is assigned a unique ID. This option returns a unique ID. You can use this ID to query the status of a delete operation. This option is useful for ensuring that a delete operation has completed before you begin other actions.
-- * 'responseStatus' - The response status code.
mkDeleteInventoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInventoryResponse
mkDeleteInventoryResponse pResponseStatus_ =
  DeleteInventoryResponse'
    { typeName = Lude.Nothing,
      deletionSummary = Lude.Nothing,
      deletionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the inventory data type specified in the request.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsTypeName :: Lens.Lens' DeleteInventoryResponse (Lude.Maybe Lude.Text)
dirsTypeName = Lens.lens (typeName :: DeleteInventoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: DeleteInventoryResponse)
{-# DEPRECATED dirsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | A summary of the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'deletionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsDeletionSummary :: Lens.Lens' DeleteInventoryResponse (Lude.Maybe InventoryDeletionSummary)
dirsDeletionSummary = Lens.lens (deletionSummary :: DeleteInventoryResponse -> Lude.Maybe InventoryDeletionSummary) (\s a -> s {deletionSummary = a} :: DeleteInventoryResponse)
{-# DEPRECATED dirsDeletionSummary "Use generic-lens or generic-optics with 'deletionSummary' instead." #-}

-- | Every @DeleteInventory@ action is assigned a unique ID. This option returns a unique ID. You can use this ID to query the status of a delete operation. This option is useful for ensuring that a delete operation has completed before you begin other actions.
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsDeletionId :: Lens.Lens' DeleteInventoryResponse (Lude.Maybe Lude.Text)
dirsDeletionId = Lens.lens (deletionId :: DeleteInventoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {deletionId = a} :: DeleteInventoryResponse)
{-# DEPRECATED dirsDeletionId "Use generic-lens or generic-optics with 'deletionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteInventoryResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteInventoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInventoryResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
