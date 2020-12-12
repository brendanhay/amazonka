{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SortCriterion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SortCriterion
  ( SortCriterion (..),

    -- * Smart constructor
    mkSortCriterion,

    -- * Lenses
    scSort,
    scFieldName,
  )
where

import Network.AWS.Glue.Types.Sort
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a field to sort by and a sort order.
--
-- /See:/ 'mkSortCriterion' smart constructor.
data SortCriterion = SortCriterion'
  { sort :: Lude.Maybe Sort,
    fieldName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SortCriterion' with the minimum fields required to make a request.
--
-- * 'fieldName' - The name of the field on which to sort.
-- * 'sort' - An ascending or descending sort.
mkSortCriterion ::
  SortCriterion
mkSortCriterion =
  SortCriterion' {sort = Lude.Nothing, fieldName = Lude.Nothing}

-- | An ascending or descending sort.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSort :: Lens.Lens' SortCriterion (Lude.Maybe Sort)
scSort = Lens.lens (sort :: SortCriterion -> Lude.Maybe Sort) (\s a -> s {sort = a} :: SortCriterion)
{-# DEPRECATED scSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The name of the field on which to sort.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFieldName :: Lens.Lens' SortCriterion (Lude.Maybe Lude.Text)
scFieldName = Lens.lens (fieldName :: SortCriterion -> Lude.Maybe Lude.Text) (\s a -> s {fieldName = a} :: SortCriterion)
{-# DEPRECATED scFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.ToJSON SortCriterion where
  toJSON SortCriterion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Sort" Lude..=) Lude.<$> sort,
            ("FieldName" Lude..=) Lude.<$> fieldName
          ]
      )
