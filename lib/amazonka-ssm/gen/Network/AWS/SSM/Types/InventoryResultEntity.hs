{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultEntity
  ( InventoryResultEntity (..),

    -- * Smart constructor
    mkInventoryResultEntity,

    -- * Lenses
    ireData,
    ireId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryResultItem

-- | Inventory query results.
--
-- /See:/ 'mkInventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { -- | The data section in the inventory result entity JSON.
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (InventoryResultItem)),
    -- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryResultEntity' with the minimum fields required to make a request.
--
-- * 'data'' - The data section in the inventory result entity JSON.
-- * 'id' - ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
mkInventoryResultEntity ::
  InventoryResultEntity
mkInventoryResultEntity =
  InventoryResultEntity' {data' = Lude.Nothing, id = Lude.Nothing}

-- | The data section in the inventory result entity JSON.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ireData :: Lens.Lens' InventoryResultEntity (Lude.Maybe (Lude.HashMap Lude.Text (InventoryResultItem)))
ireData = Lens.lens (data' :: InventoryResultEntity -> Lude.Maybe (Lude.HashMap Lude.Text (InventoryResultItem))) (\s a -> s {data' = a} :: InventoryResultEntity)
{-# DEPRECATED ireData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ireId :: Lens.Lens' InventoryResultEntity (Lude.Maybe Lude.Text)
ireId = Lens.lens (id :: InventoryResultEntity -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InventoryResultEntity)
{-# DEPRECATED ireId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON InventoryResultEntity where
  parseJSON =
    Lude.withObject
      "InventoryResultEntity"
      ( \x ->
          InventoryResultEntity'
            Lude.<$> (x Lude..:? "Data" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
      )
