{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Column
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Column
  ( Column (..),

    -- * Smart constructor
    mkColumn,

    -- * Lenses
    cType,
    cComment,
    cName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata for a column in a table.
--
-- /See:/ 'mkColumn' smart constructor.
data Column = Column'
  { type' :: Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Column' with the minimum fields required to make a request.
--
-- * 'comment' - Optional information about the column.
-- * 'name' - The name of the column.
-- * 'type'' - The data type of the column.
mkColumn ::
  -- | 'name'
  Lude.Text ->
  Column
mkColumn pName_ =
  Column'
    { type' = Lude.Nothing,
      comment = Lude.Nothing,
      name = pName_
    }

-- | The data type of the column.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Column (Lude.Maybe Lude.Text)
cType = Lens.lens (type' :: Column -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Column)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Optional information about the column.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' Column (Lude.Maybe Lude.Text)
cComment = Lens.lens (comment :: Column -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: Column)
{-# DEPRECATED cComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Column Lude.Text
cName = Lens.lens (name :: Column -> Lude.Text) (\s a -> s {name = a} :: Column)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Column where
  parseJSON =
    Lude.withObject
      "Column"
      ( \x ->
          Column'
            Lude.<$> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Comment")
            Lude.<*> (x Lude..: "Name")
      )
