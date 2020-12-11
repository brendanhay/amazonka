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
    lpmRemove,
    lpmAdd,
  )
where

import Network.AWS.EC2.Types.LoadPermissionRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes modifications to the load permissions of an Amazon FPGA image (AFI).
--
-- /See:/ 'mkLoadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { remove ::
      Lude.Maybe [LoadPermissionRequest],
    add ::
      Lude.Maybe [LoadPermissionRequest]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadPermissionModifications' with the minimum fields required to make a request.
--
-- * 'add' - The load permissions to add.
-- * 'remove' - The load permissions to remove.
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
lpmRemove :: Lens.Lens' LoadPermissionModifications (Lude.Maybe [LoadPermissionRequest])
lpmRemove = Lens.lens (remove :: LoadPermissionModifications -> Lude.Maybe [LoadPermissionRequest]) (\s a -> s {remove = a} :: LoadPermissionModifications)
{-# DEPRECATED lpmRemove "Use generic-lens or generic-optics with 'remove' instead." #-}

-- | The load permissions to add.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpmAdd :: Lens.Lens' LoadPermissionModifications (Lude.Maybe [LoadPermissionRequest])
lpmAdd = Lens.lens (add :: LoadPermissionModifications -> Lude.Maybe [LoadPermissionRequest]) (\s a -> s {add = a} :: LoadPermissionModifications)
{-# DEPRECATED lpmAdd "Use generic-lens or generic-optics with 'add' instead." #-}

instance Lude.ToQuery LoadPermissionModifications where
  toQuery LoadPermissionModifications' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Remove" Lude.<$> remove),
        Lude.toQuery (Lude.toQueryList "Add" Lude.<$> add)
      ]
