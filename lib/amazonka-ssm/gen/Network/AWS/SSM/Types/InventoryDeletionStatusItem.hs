{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionStatusItem
  ( InventoryDeletionStatusItem (..),

    -- * Smart constructor
    mkInventoryDeletionStatusItem,

    -- * Lenses
    idsiTypeName,
    idsiLastStatusUpdateTime,
    idsiLastStatusMessage,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiDeletionStartTime,
    idsiDeletionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryDeletionStatus
import Network.AWS.SSM.Types.InventoryDeletionSummary

-- | Status information returned by the @DeleteInventory@ action.
--
-- /See:/ 'mkInventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { -- | The name of the inventory data type.
    typeName :: Lude.Maybe Lude.Text,
    -- | The UTC timestamp of when the last status report.
    lastStatusUpdateTime :: Lude.Maybe Lude.Timestamp,
    -- | Information about the status.
    lastStatusMessage :: Lude.Maybe Lude.Text,
    -- | Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
    deletionSummary :: Lude.Maybe InventoryDeletionSummary,
    -- | The status of the operation. Possible values are InProgress and Complete.
    lastStatus :: Lude.Maybe InventoryDeletionStatus,
    -- | The UTC timestamp when the delete operation started.
    deletionStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The deletion ID returned by the @DeleteInventory@ action.
    deletionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryDeletionStatusItem' with the minimum fields required to make a request.
--
-- * 'typeName' - The name of the inventory data type.
-- * 'lastStatusUpdateTime' - The UTC timestamp of when the last status report.
-- * 'lastStatusMessage' - Information about the status.
-- * 'deletionSummary' - Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
-- * 'lastStatus' - The status of the operation. Possible values are InProgress and Complete.
-- * 'deletionStartTime' - The UTC timestamp when the delete operation started.
-- * 'deletionId' - The deletion ID returned by the @DeleteInventory@ action.
mkInventoryDeletionStatusItem ::
  InventoryDeletionStatusItem
mkInventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { typeName = Lude.Nothing,
      lastStatusUpdateTime = Lude.Nothing,
      lastStatusMessage = Lude.Nothing,
      deletionSummary = Lude.Nothing,
      lastStatus = Lude.Nothing,
      deletionStartTime = Lude.Nothing,
      deletionId = Lude.Nothing
    }

-- | The name of the inventory data type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiTypeName :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe Lude.Text)
idsiTypeName = Lens.lens (typeName :: InventoryDeletionStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The UTC timestamp of when the last status report.
--
-- /Note:/ Consider using 'lastStatusUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatusUpdateTime :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe Lude.Timestamp)
idsiLastStatusUpdateTime = Lens.lens (lastStatusUpdateTime :: InventoryDeletionStatusItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStatusUpdateTime = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiLastStatusUpdateTime "Use generic-lens or generic-optics with 'lastStatusUpdateTime' instead." #-}

-- | Information about the status.
--
-- /Note:/ Consider using 'lastStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatusMessage :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe Lude.Text)
idsiLastStatusMessage = Lens.lens (lastStatusMessage :: InventoryDeletionStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {lastStatusMessage = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiLastStatusMessage "Use generic-lens or generic-optics with 'lastStatusMessage' instead." #-}

-- | Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'deletionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionSummary :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe InventoryDeletionSummary)
idsiDeletionSummary = Lens.lens (deletionSummary :: InventoryDeletionStatusItem -> Lude.Maybe InventoryDeletionSummary) (\s a -> s {deletionSummary = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiDeletionSummary "Use generic-lens or generic-optics with 'deletionSummary' instead." #-}

-- | The status of the operation. Possible values are InProgress and Complete.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatus :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe InventoryDeletionStatus)
idsiLastStatus = Lens.lens (lastStatus :: InventoryDeletionStatusItem -> Lude.Maybe InventoryDeletionStatus) (\s a -> s {lastStatus = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The UTC timestamp when the delete operation started.
--
-- /Note:/ Consider using 'deletionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionStartTime :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe Lude.Timestamp)
idsiDeletionStartTime = Lens.lens (deletionStartTime :: InventoryDeletionStatusItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletionStartTime = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiDeletionStartTime "Use generic-lens or generic-optics with 'deletionStartTime' instead." #-}

-- | The deletion ID returned by the @DeleteInventory@ action.
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionId :: Lens.Lens' InventoryDeletionStatusItem (Lude.Maybe Lude.Text)
idsiDeletionId = Lens.lens (deletionId :: InventoryDeletionStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {deletionId = a} :: InventoryDeletionStatusItem)
{-# DEPRECATED idsiDeletionId "Use generic-lens or generic-optics with 'deletionId' instead." #-}

instance Lude.FromJSON InventoryDeletionStatusItem where
  parseJSON =
    Lude.withObject
      "InventoryDeletionStatusItem"
      ( \x ->
          InventoryDeletionStatusItem'
            Lude.<$> (x Lude..:? "TypeName")
            Lude.<*> (x Lude..:? "LastStatusUpdateTime")
            Lude.<*> (x Lude..:? "LastStatusMessage")
            Lude.<*> (x Lude..:? "DeletionSummary")
            Lude.<*> (x Lude..:? "LastStatus")
            Lude.<*> (x Lude..:? "DeletionStartTime")
            Lude.<*> (x Lude..:? "DeletionId")
      )
