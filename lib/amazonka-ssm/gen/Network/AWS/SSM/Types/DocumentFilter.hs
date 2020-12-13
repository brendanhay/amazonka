{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dfValue,
    dfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DocumentFilterKey

-- | This data type is deprecated. Instead, use 'DocumentKeyValuesFilter' .
--
-- /See:/ 'mkDocumentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { -- | The value of the filter.
    value :: Lude.Text,
    -- | The name of the filter.
    key :: DocumentFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- * 'value' - The value of the filter.
-- * 'key' - The name of the filter.
mkDocumentFilter ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  DocumentFilterKey ->
  DocumentFilter
mkDocumentFilter pValue_ pKey_ =
  DocumentFilter' {value = pValue_, key = pKey_}

-- | The value of the filter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfValue :: Lens.Lens' DocumentFilter Lude.Text
dfValue = Lens.lens (value :: DocumentFilter -> Lude.Text) (\s a -> s {value = a} :: DocumentFilter)
{-# DEPRECATED dfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfKey :: Lens.Lens' DocumentFilter DocumentFilterKey
dfKey = Lens.lens (key :: DocumentFilter -> DocumentFilterKey) (\s a -> s {key = a} :: DocumentFilter)
{-# DEPRECATED dfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON DocumentFilter where
  toJSON DocumentFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("value" Lude..= value), Lude.Just ("key" Lude..= key)]
      )
