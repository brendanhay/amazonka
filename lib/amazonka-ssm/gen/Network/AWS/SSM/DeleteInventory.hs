{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteInventory (..)
    , mkDeleteInventory
    -- ** Request lenses
    , diTypeName
    , diClientToken
    , diDryRun
    , diSchemaDeleteOption

    -- * Destructuring the response
    , DeleteInventoryResponse (..)
    , mkDeleteInventoryResponse
    -- ** Response lenses
    , dirrsDeletionId
    , dirrsDeletionSummary
    , dirrsTypeName
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteInventory' smart constructor.
data DeleteInventory = DeleteInventory'
  { typeName :: Types.InventoryItemTypeName
    -- ^ The name of the custom inventory type for which you want to delete either all previously collected data or the inventory type itself. 
  , clientToken :: Core.Maybe Types.UUID
    -- ^ User-provided idempotency token.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Use this option to view a summary of the deletion request without deleting any data or the data type. This option is useful when you only want to understand what will be deleted. Once you validate that the data to be deleted is what you intend to delete, you can run the same command without specifying the @DryRun@ option.
  , schemaDeleteOption :: Core.Maybe Types.InventorySchemaDeleteOption
    -- ^ Use the @SchemaDeleteOption@ to delete a custom inventory type (schema). If you don't choose this option, the system only deletes existing inventory data associated with the custom inventory type. Choose one of the following options:
--
-- DisableSchema: If you choose this option, the system ignores all inventory data for the specified version, and any earlier versions. To enable this schema again, you must call the @PutInventory@ action for a version greater than the disabled version.
-- DeleteSchema: This option deletes the specified custom type from the Inventory service. You can recreate the schema later, if you want.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInventory' value with any optional fields omitted.
mkDeleteInventory
    :: Types.InventoryItemTypeName -- ^ 'typeName'
    -> DeleteInventory
mkDeleteInventory typeName
  = DeleteInventory'{typeName, clientToken = Core.Nothing,
                     dryRun = Core.Nothing, schemaDeleteOption = Core.Nothing}

-- | The name of the custom inventory type for which you want to delete either all previously collected data or the inventory type itself. 
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTypeName :: Lens.Lens' DeleteInventory Types.InventoryItemTypeName
diTypeName = Lens.field @"typeName"
{-# INLINEABLE diTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diClientToken :: Lens.Lens' DeleteInventory (Core.Maybe Types.UUID)
diClientToken = Lens.field @"clientToken"
{-# INLINEABLE diClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Use this option to view a summary of the deletion request without deleting any data or the data type. This option is useful when you only want to understand what will be deleted. Once you validate that the data to be deleted is what you intend to delete, you can run the same command without specifying the @DryRun@ option.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDryRun :: Lens.Lens' DeleteInventory (Core.Maybe Core.Bool)
diDryRun = Lens.field @"dryRun"
{-# INLINEABLE diDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Use the @SchemaDeleteOption@ to delete a custom inventory type (schema). If you don't choose this option, the system only deletes existing inventory data associated with the custom inventory type. Choose one of the following options:
--
-- DisableSchema: If you choose this option, the system ignores all inventory data for the specified version, and any earlier versions. To enable this schema again, you must call the @PutInventory@ action for a version greater than the disabled version.
-- DeleteSchema: This option deletes the specified custom type from the Inventory service. You can recreate the schema later, if you want.
--
-- /Note:/ Consider using 'schemaDeleteOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSchemaDeleteOption :: Lens.Lens' DeleteInventory (Core.Maybe Types.InventorySchemaDeleteOption)
diSchemaDeleteOption = Lens.field @"schemaDeleteOption"
{-# INLINEABLE diSchemaDeleteOption #-}
{-# DEPRECATED schemaDeleteOption "Use generic-lens or generic-optics with 'schemaDeleteOption' instead"  #-}

instance Core.ToQuery DeleteInventory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInventory where
        toHeaders DeleteInventory{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteInventory") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteInventory where
        toJSON DeleteInventory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TypeName" Core..= typeName),
                  ("ClientToken" Core..=) Core.<$> clientToken,
                  ("DryRun" Core..=) Core.<$> dryRun,
                  ("SchemaDeleteOption" Core..=) Core.<$> schemaDeleteOption])

instance Core.AWSRequest DeleteInventory where
        type Rs DeleteInventory = DeleteInventoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteInventoryResponse' Core.<$>
                   (x Core..:? "DeletionId") Core.<*> x Core..:? "DeletionSummary"
                     Core.<*> x Core..:? "TypeName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInventoryResponse' smart constructor.
data DeleteInventoryResponse = DeleteInventoryResponse'
  { deletionId :: Core.Maybe Types.UUID
    -- ^ Every @DeleteInventory@ action is assigned a unique ID. This option returns a unique ID. You can use this ID to query the status of a delete operation. This option is useful for ensuring that a delete operation has completed before you begin other actions. 
  , deletionSummary :: Core.Maybe Types.InventoryDeletionSummary
    -- ^ A summary of the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory> in the /AWS Systems Manager User Guide/ .
  , typeName :: Core.Maybe Types.InventoryItemTypeName
    -- ^ The name of the inventory data type specified in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInventoryResponse' value with any optional fields omitted.
mkDeleteInventoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInventoryResponse
mkDeleteInventoryResponse responseStatus
  = DeleteInventoryResponse'{deletionId = Core.Nothing,
                             deletionSummary = Core.Nothing, typeName = Core.Nothing,
                             responseStatus}

-- | Every @DeleteInventory@ action is assigned a unique ID. This option returns a unique ID. You can use this ID to query the status of a delete operation. This option is useful for ensuring that a delete operation has completed before you begin other actions. 
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsDeletionId :: Lens.Lens' DeleteInventoryResponse (Core.Maybe Types.UUID)
dirrsDeletionId = Lens.field @"deletionId"
{-# INLINEABLE dirrsDeletionId #-}
{-# DEPRECATED deletionId "Use generic-lens or generic-optics with 'deletionId' instead"  #-}

-- | A summary of the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete-summary Deleting custom inventory> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'deletionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsDeletionSummary :: Lens.Lens' DeleteInventoryResponse (Core.Maybe Types.InventoryDeletionSummary)
dirrsDeletionSummary = Lens.field @"deletionSummary"
{-# INLINEABLE dirrsDeletionSummary #-}
{-# DEPRECATED deletionSummary "Use generic-lens or generic-optics with 'deletionSummary' instead"  #-}

-- | The name of the inventory data type specified in the request.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsTypeName :: Lens.Lens' DeleteInventoryResponse (Core.Maybe Types.InventoryItemTypeName)
dirrsTypeName = Lens.field @"typeName"
{-# INLINEABLE dirrsTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInventoryResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
