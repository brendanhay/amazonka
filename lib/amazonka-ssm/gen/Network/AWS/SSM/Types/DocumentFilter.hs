-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFilter
  ( DocumentFilter (..),

    -- * Smart constructor
    mkDocumentFilter,

    -- * Lenses
    dfKey,
    dfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DocumentFilterKey

-- | This data type is deprecated. Instead, use 'DocumentKeyValuesFilter' .
--
-- /See:/ 'mkDocumentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { key :: DocumentFilterKey,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'value' - The value of the filter.
mkDocumentFilter ::
  -- | 'key'
  DocumentFilterKey ->
  -- | 'value'
  Lude.Text ->
  DocumentFilter
mkDocumentFilter pKey_ pValue_ =
  DocumentFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfKey :: Lens.Lens' DocumentFilter DocumentFilterKey
dfKey = Lens.lens (key :: DocumentFilter -> DocumentFilterKey) (\s a -> s {key = a} :: DocumentFilter)
{-# DEPRECATED dfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of the filter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfValue :: Lens.Lens' DocumentFilter Lude.Text
dfValue = Lens.lens (value :: DocumentFilter -> Lude.Text) (\s a -> s {value = a} :: DocumentFilter)
{-# DEPRECATED dfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON DocumentFilter where
  toJSON DocumentFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("key" Lude..= key), Lude.Just ("value" Lude..= value)]
      )
