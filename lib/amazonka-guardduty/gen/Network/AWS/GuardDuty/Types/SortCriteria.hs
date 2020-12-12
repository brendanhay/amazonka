{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.SortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.SortCriteria
  ( SortCriteria (..),

    -- * Smart constructor
    mkSortCriteria,

    -- * Lenses
    scOrderBy,
    scAttributeName,
  )
where

import Network.AWS.GuardDuty.Types.OrderBy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the criteria used for sorting findings.
--
-- /See:/ 'mkSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { orderBy :: Lude.Maybe OrderBy,
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SortCriteria' with the minimum fields required to make a request.
--
-- * 'attributeName' - Represents the finding attribute (for example, accountId) to sort findings by.
-- * 'orderBy' - The order by which the sorted findings are to be displayed.
mkSortCriteria ::
  SortCriteria
mkSortCriteria =
  SortCriteria'
    { orderBy = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The order by which the sorted findings are to be displayed.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOrderBy :: Lens.Lens' SortCriteria (Lude.Maybe OrderBy)
scOrderBy = Lens.lens (orderBy :: SortCriteria -> Lude.Maybe OrderBy) (\s a -> s {orderBy = a} :: SortCriteria)
{-# DEPRECATED scOrderBy "Use generic-lens or generic-optics with 'orderBy' instead." #-}

-- | Represents the finding attribute (for example, accountId) to sort findings by.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAttributeName :: Lens.Lens' SortCriteria (Lude.Maybe Lude.Text)
scAttributeName = Lens.lens (attributeName :: SortCriteria -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: SortCriteria)
{-# DEPRECATED scAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("orderBy" Lude..=) Lude.<$> orderBy,
            ("attributeName" Lude..=) Lude.<$> attributeName
          ]
      )
