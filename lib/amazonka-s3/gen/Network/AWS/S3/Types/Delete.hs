{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Delete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Delete
  ( Delete (..),

    -- * Smart constructor
    mkDelete,

    -- * Lenses
    dQuiet,
    dObjects,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectIdentifier

-- | Container for the objects to delete.
--
-- /See:/ 'mkDelete' smart constructor.
data Delete = Delete'
  { -- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
    quiet :: Lude.Maybe Lude.Bool,
    -- | The objects to delete.
    objects :: [ObjectIdentifier]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- * 'quiet' - Element to enable quiet mode for the request. When you add this element, you must set its value to true.
-- * 'objects' - The objects to delete.
mkDelete ::
  Delete
mkDelete = Delete' {quiet = Lude.Nothing, objects = Lude.mempty}

-- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
--
-- /Note:/ Consider using 'quiet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dQuiet :: Lens.Lens' Delete (Lude.Maybe Lude.Bool)
dQuiet = Lens.lens (quiet :: Delete -> Lude.Maybe Lude.Bool) (\s a -> s {quiet = a} :: Delete)
{-# DEPRECATED dQuiet "Use generic-lens or generic-optics with 'quiet' instead." #-}

-- | The objects to delete.
--
-- /Note:/ Consider using 'objects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dObjects :: Lens.Lens' Delete [ObjectIdentifier]
dObjects = Lens.lens (objects :: Delete -> [ObjectIdentifier]) (\s a -> s {objects = a} :: Delete)
{-# DEPRECATED dObjects "Use generic-lens or generic-optics with 'objects' instead." #-}

instance Lude.ToXML Delete where
  toXML Delete' {..} =
    Lude.mconcat
      ["Quiet" Lude.@= quiet, Lude.toXMLList "Object" objects]
