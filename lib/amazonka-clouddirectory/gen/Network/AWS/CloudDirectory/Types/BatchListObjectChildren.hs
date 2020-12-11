-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildren
  ( BatchListObjectChildren (..),

    -- * Smart constructor
    mkBatchListObjectChildren,

    -- * Lenses
    bloclNextToken,
    bloclMaxResults,
    bloclObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectChildren' operation.
--
-- /See:/ 'mkBatchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListObjectChildren' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - Reference of the object for which child objects are being listed.
mkBatchListObjectChildren ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectChildren
mkBatchListObjectChildren pObjectReference_ =
  BatchListObjectChildren'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloclNextToken :: Lens.Lens' BatchListObjectChildren (Lude.Maybe Lude.Text)
bloclNextToken = Lens.lens (nextToken :: BatchListObjectChildren -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectChildren)
{-# DEPRECATED bloclNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloclMaxResults :: Lens.Lens' BatchListObjectChildren (Lude.Maybe Lude.Natural)
bloclMaxResults = Lens.lens (maxResults :: BatchListObjectChildren -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListObjectChildren)
{-# DEPRECATED bloclMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Reference of the object for which child objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloclObjectReference :: Lens.Lens' BatchListObjectChildren ObjectReference
bloclObjectReference = Lens.lens (objectReference :: BatchListObjectChildren -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListObjectChildren)
{-# DEPRECATED bloclObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchListObjectChildren where
  toJSON BatchListObjectChildren' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
