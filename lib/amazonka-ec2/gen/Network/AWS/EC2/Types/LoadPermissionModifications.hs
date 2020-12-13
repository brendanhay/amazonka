{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionModifications
  ( LoadPermissionModifications (..),

    -- * Smart constructor
    mkLoadPermissionModifications,

    -- * Lenses
    lRemove,
    lAdd,
  )
where

import Network.AWS.EC2.Types.LoadPermissionRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes modifications to the load permissions of an Amazon FPGA image (AFI).
--
-- /See:/ 'mkLoadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { -- | The load permissions to remove.
    remove :: Lude.Maybe [LoadPermissionRequest],
    -- | The load permissions to add.
    add :: Lude.Maybe [LoadPermissionRequest]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadPermissionModifications' with the minimum fields required to make a request.
--
-- * 'remove' - The load permissions to remove.
-- * 'add' - The load permissions to add.
mkLoadPermissionModifications ::
  LoadPermissionModifications
mkLoadPermissionModifications =
  LoadPermissionModifications'
    { remove = Lude.Nothing,
      add = Lude.Nothing
    }

-- | The load permissions to remove.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRemove :: Lens.Lens' LoadPermissionModifications (Lude.Maybe [LoadPermissionRequest])
lRemove = Lens.lens (remove :: LoadPermissionModifications -> Lude.Maybe [LoadPermissionRequest]) (\s a -> s {remove = a} :: LoadPermissionModifications)
{-# DEPRECATED lRemove "Use generic-lens or generic-optics with 'remove' instead." #-}

-- | The load permissions to add.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAdd :: Lens.Lens' LoadPermissionModifications (Lude.Maybe [LoadPermissionRequest])
lAdd = Lens.lens (add :: LoadPermissionModifications -> Lude.Maybe [LoadPermissionRequest]) (\s a -> s {add = a} :: LoadPermissionModifications)
{-# DEPRECATED lAdd "Use generic-lens or generic-optics with 'add' instead." #-}

instance Lude.ToQuery LoadPermissionModifications where
  toQuery LoadPermissionModifications' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Remove" Lude.<$> remove),
        Lude.toQuery (Lude.toQueryList "Add" Lude.<$> add)
      ]
