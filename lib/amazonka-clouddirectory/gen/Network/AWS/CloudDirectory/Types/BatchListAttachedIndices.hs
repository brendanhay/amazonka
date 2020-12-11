-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
  ( BatchListAttachedIndices (..),

    -- * Smart constructor
    mkBatchListAttachedIndices,

    -- * Lenses
    blaisNextToken,
    blaisMaxResults,
    blaisTargetReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists indices attached to an object inside a 'BatchRead' operation. For more information, see 'ListAttachedIndices' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListAttachedIndices' smart constructor.
data BatchListAttachedIndices = BatchListAttachedIndices'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    targetReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListAttachedIndices' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'targetReference' - A reference to the object that has indices attached.
mkBatchListAttachedIndices ::
  -- | 'targetReference'
  ObjectReference ->
  BatchListAttachedIndices
mkBatchListAttachedIndices pTargetReference_ =
  BatchListAttachedIndices'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      targetReference = pTargetReference_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaisNextToken :: Lens.Lens' BatchListAttachedIndices (Lude.Maybe Lude.Text)
blaisNextToken = Lens.lens (nextToken :: BatchListAttachedIndices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListAttachedIndices)
{-# DEPRECATED blaisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaisMaxResults :: Lens.Lens' BatchListAttachedIndices (Lude.Maybe Lude.Natural)
blaisMaxResults = Lens.lens (maxResults :: BatchListAttachedIndices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListAttachedIndices)
{-# DEPRECATED blaisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A reference to the object that has indices attached.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaisTargetReference :: Lens.Lens' BatchListAttachedIndices ObjectReference
blaisTargetReference = Lens.lens (targetReference :: BatchListAttachedIndices -> ObjectReference) (\s a -> s {targetReference = a} :: BatchListAttachedIndices)
{-# DEPRECATED blaisTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Lude.ToJSON BatchListAttachedIndices where
  toJSON BatchListAttachedIndices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TargetReference" Lude..= targetReference)
          ]
      )
