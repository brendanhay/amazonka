{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilter
  ( AssociationFilter (..),

    -- * Smart constructor
    mkAssociationFilter,

    -- * Lenses
    afValue,
    afKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationFilterKey

-- | Describes a filter.
--
-- /See:/ 'mkAssociationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
  { -- | The filter value.
    value :: Lude.Text,
    -- | The name of the filter.
    key :: AssociationFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationFilter' with the minimum fields required to make a request.
--
-- * 'value' - The filter value.
-- * 'key' - The name of the filter.
mkAssociationFilter ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  AssociationFilterKey ->
  AssociationFilter
mkAssociationFilter pValue_ pKey_ =
  AssociationFilter' {value = pValue_, key = pKey_}

-- | The filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afValue :: Lens.Lens' AssociationFilter Lude.Text
afValue = Lens.lens (value :: AssociationFilter -> Lude.Text) (\s a -> s {value = a} :: AssociationFilter)
{-# DEPRECATED afValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afKey :: Lens.Lens' AssociationFilter AssociationFilterKey
afKey = Lens.lens (key :: AssociationFilter -> AssociationFilterKey) (\s a -> s {key = a} :: AssociationFilter)
{-# DEPRECATED afKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON AssociationFilter where
  toJSON AssociationFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("value" Lude..= value), Lude.Just ("key" Lude..= key)]
      )
