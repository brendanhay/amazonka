{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Sort
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Sort
  ( Sort (..),

    -- * Smart constructor
    mkSort,

    -- * Lenses
    sKey,
    sValue,
  )
where

import Network.AWS.AlexaBusiness.Types.SortValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a sort criteria.
--
-- /See:/ 'mkSort' smart constructor.
data Sort = Sort' {key :: Lude.Text, value :: SortValue}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Sort' with the minimum fields required to make a request.
--
-- * 'key' - The sort key of a sort object.
-- * 'value' - The sort value of a sort object.
mkSort ::
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  SortValue ->
  Sort
mkSort pKey_ pValue_ = Sort' {key = pKey_, value = pValue_}

-- | The sort key of a sort object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKey :: Lens.Lens' Sort Lude.Text
sKey = Lens.lens (key :: Sort -> Lude.Text) (\s a -> s {key = a} :: Sort)
{-# DEPRECATED sKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The sort value of a sort object.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Sort SortValue
sValue = Lens.lens (value :: Sort -> SortValue) (\s a -> s {value = a} :: Sort)
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON Sort where
  toJSON Sort' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Key" Lude..= key), Lude.Just ("Value" Lude..= value)]
      )
