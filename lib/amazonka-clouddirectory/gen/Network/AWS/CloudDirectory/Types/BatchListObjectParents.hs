{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParents
  ( BatchListObjectParents (..),

    -- * Smart constructor
    mkBatchListObjectParents,

    -- * Lenses
    blopfNextToken,
    blopfObjectReference,
    blopfMaxResults,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkBatchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
  { nextToken :: Lude.Maybe Lude.Text,
    objectReference :: ObjectReference,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListObjectParents' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'objectReference' -
-- * 'maxResults' -
mkBatchListObjectParents ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectParents
mkBatchListObjectParents pObjectReference_ =
  BatchListObjectParents'
    { nextToken = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopfNextToken :: Lens.Lens' BatchListObjectParents (Lude.Maybe Lude.Text)
blopfNextToken = Lens.lens (nextToken :: BatchListObjectParents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectParents)
{-# DEPRECATED blopfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopfObjectReference :: Lens.Lens' BatchListObjectParents ObjectReference
blopfObjectReference = Lens.lens (objectReference :: BatchListObjectParents -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListObjectParents)
{-# DEPRECATED blopfObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopfMaxResults :: Lens.Lens' BatchListObjectParents (Lude.Maybe Lude.Natural)
blopfMaxResults = Lens.lens (maxResults :: BatchListObjectParents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListObjectParents)
{-# DEPRECATED blopfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.ToJSON BatchListObjectParents where
  toJSON BatchListObjectParents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )
