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

import Network.AWS.Comprehend.Types.Entity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectEntitiesItemResult' smart constructor.
data BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult'
  { entities ::
      Lude.Maybe [Entity],
    index :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectEntitiesItemResult' with the minimum fields required to make a request.
--
-- * 'entities' - One or more 'Entity' objects, one for each entity detected in the document.
-- * 'index' - The zero-based index of the document in the input list.
mkBatchDetectEntitiesItemResult ::
  BatchDetectEntitiesItemResult
mkBatchDetectEntitiesItemResult =
  BatchDetectEntitiesItemResult'
    { entities = Lude.Nothing,
      index = Lude.Nothing
    }

-- | One or more 'Entity' objects, one for each entity detected in the document.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeirEntities :: Lens.Lens' BatchDetectEntitiesItemResult (Lude.Maybe [Entity])
bdeirEntities = Lens.lens (entities :: BatchDetectEntitiesItemResult -> Lude.Maybe [Entity]) (\s a -> s {entities = a} :: BatchDetectEntitiesItemResult)
{-# DEPRECATED bdeirEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeirIndex :: Lens.Lens' BatchDetectEntitiesItemResult (Lude.Maybe Lude.Int)
bdeirIndex = Lens.lens (index :: BatchDetectEntitiesItemResult -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchDetectEntitiesItemResult)
{-# DEPRECATED bdeirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON BatchDetectEntitiesItemResult where
  parseJSON =
    Lude.withObject
      "BatchDetectEntitiesItemResult"
      ( \x ->
          BatchDetectEntitiesItemResult'
            Lude.<$> (x Lude..:? "Entities" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Index")
      )
