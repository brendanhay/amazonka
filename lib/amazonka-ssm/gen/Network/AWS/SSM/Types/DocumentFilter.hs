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
    dfKey,
    dfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentFilterKey as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | This data type is deprecated. Instead, use 'DocumentKeyValuesFilter' .
--
-- /See:/ 'mkDocumentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { -- | The name of the filter.
    key :: Types.DocumentFilterKey,
    -- | The value of the filter.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentFilter' value with any optional fields omitted.
mkDocumentFilter ::
  -- | 'key'
  Types.DocumentFilterKey ->
  -- | 'value'
  Types.Value ->
  DocumentFilter
mkDocumentFilter key value = DocumentFilter' {key, value}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfKey :: Lens.Lens' DocumentFilter Types.DocumentFilterKey
dfKey = Lens.field @"key"
{-# DEPRECATED dfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of the filter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfValue :: Lens.Lens' DocumentFilter Types.Value
dfValue = Lens.field @"value"
{-# DEPRECATED dfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON DocumentFilter where
  toJSON DocumentFilter {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("key" Core..= key), Core.Just ("value" Core..= value)]
      )
