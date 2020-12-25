{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
  ( BatchDetectEntitiesItemResult (..),

    -- * Smart constructor
    mkBatchDetectEntitiesItemResult,

    -- * Lenses
    bdeirEntities,
    bdeirIndex,
  )
where

import qualified Network.AWS.Comprehend.Types.Entity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectEntitiesItemResult' smart constructor.
data BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult'
  { -- | One or more 'Entity' objects, one for each entity detected in the document.
    entities :: Core.Maybe [Types.Entity],
    -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectEntitiesItemResult' value with any optional fields omitted.
mkBatchDetectEntitiesItemResult ::
  BatchDetectEntitiesItemResult
mkBatchDetectEntitiesItemResult =
  BatchDetectEntitiesItemResult'
    { entities = Core.Nothing,
      index = Core.Nothing
    }

-- | One or more 'Entity' objects, one for each entity detected in the document.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeirEntities :: Lens.Lens' BatchDetectEntitiesItemResult (Core.Maybe [Types.Entity])
bdeirEntities = Lens.field @"entities"
{-# DEPRECATED bdeirEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeirIndex :: Lens.Lens' BatchDetectEntitiesItemResult (Core.Maybe Core.Int)
bdeirIndex = Lens.field @"index"
{-# DEPRECATED bdeirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Core.FromJSON BatchDetectEntitiesItemResult where
  parseJSON =
    Core.withObject "BatchDetectEntitiesItemResult" Core.$
      \x ->
        BatchDetectEntitiesItemResult'
          Core.<$> (x Core..:? "Entities") Core.<*> (x Core..:? "Index")
