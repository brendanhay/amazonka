{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to streamline results of a search based on the property being filtered.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { name :: Lude.Text,
    values :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'name' - The property being filtered. For example, UserId.
-- * 'values' - The property values to filter on. For example, "user-123".
mkFilter ::
  -- | 'name'
  Lude.Text ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  Filter
mkFilter pName_ pValues_ =
  Filter' {name = pName_, values = pValues_}

-- | The property being filtered. For example, UserId.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Lude.Text
fName = Lens.lens (name :: Filter -> Lude.Text) (\s a -> s {name = a} :: Filter)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The property values to filter on. For example, "user-123".
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Lude.NonEmpty Lude.Text)
fValues = Lens.lens (values :: Filter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToQuery Filter where
  toQuery Filter' {..} =
    Lude.mconcat
      [ "Name" Lude.=: name,
        "Values" Lude.=: Lude.toQueryList "member" values
      ]
