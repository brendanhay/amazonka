-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParametersFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParametersFilter
  ( ParametersFilter (..),

    -- * Smart constructor
    mkParametersFilter,

    -- * Lenses
    pKey,
    pValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ParametersFilterKey

-- | This data type is deprecated. Instead, use 'ParameterStringFilter' .
--
-- /See:/ 'mkParametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
  { key ::
      ParametersFilterKey,
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

-- | Creates a value of 'ParametersFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'values' - The filter values.
mkParametersFilter ::
  -- | 'key'
  ParametersFilterKey ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  ParametersFilter
mkParametersFilter pKey_ pValues_ =
  ParametersFilter' {key = pKey_, values = pValues_}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pKey :: Lens.Lens' ParametersFilter ParametersFilterKey
pKey = Lens.lens (key :: ParametersFilter -> ParametersFilterKey) (\s a -> s {key = a} :: ParametersFilter)
{-# DEPRECATED pKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValues :: Lens.Lens' ParametersFilter (Lude.NonEmpty Lude.Text)
pValues = Lens.lens (values :: ParametersFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: ParametersFilter)
{-# DEPRECATED pValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON ParametersFilter where
  toJSON ParametersFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
