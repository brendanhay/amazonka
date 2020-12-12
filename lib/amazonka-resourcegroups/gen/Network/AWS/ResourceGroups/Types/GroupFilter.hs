{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilter
  ( GroupFilter (..),

    -- * Smart constructor
    mkGroupFilter,

    -- * Lenses
    gfName,
    gfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.GroupFilterName

-- | A filter collection that you can use to restrict the results from a @List@ operation to only those you want to include.
--
-- /See:/ 'mkGroupFilter' smart constructor.
data GroupFilter = GroupFilter'
  { name :: GroupFilterName,
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

-- | Creates a value of 'GroupFilter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the filter. Filter names are case-sensitive.
-- * 'values' - One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
mkGroupFilter ::
  -- | 'name'
  GroupFilterName ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  GroupFilter
mkGroupFilter pName_ pValues_ =
  GroupFilter' {name = pName_, values = pValues_}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfName :: Lens.Lens' GroupFilter GroupFilterName
gfName = Lens.lens (name :: GroupFilter -> GroupFilterName) (\s a -> s {name = a} :: GroupFilter)
{-# DEPRECATED gfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfValues :: Lens.Lens' GroupFilter (Lude.NonEmpty Lude.Text)
gfValues = Lens.lens (values :: GroupFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: GroupFilter)
{-# DEPRECATED gfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON GroupFilter where
  toJSON GroupFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Values" Lude..= values)
          ]
      )
