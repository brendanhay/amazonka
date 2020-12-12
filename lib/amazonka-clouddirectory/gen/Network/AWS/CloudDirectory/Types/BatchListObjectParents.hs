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
    bloplNextToken,
    bloplMaxResults,
    bloplObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkBatchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
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

-- | Creates a value of 'BatchListObjectParents' with the minimum fields required to make a request.
--
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'objectReference' - Undocumented field.
mkBatchListObjectParents ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectParents
mkBatchListObjectParents pObjectReference_ =
  BatchListObjectParents'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      objectReference = pObjectReference_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloplNextToken :: Lens.Lens' BatchListObjectParents (Lude.Maybe Lude.Text)
bloplNextToken = Lens.lens (nextToken :: BatchListObjectParents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectParents)
{-# DEPRECATED bloplNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloplMaxResults :: Lens.Lens' BatchListObjectParents (Lude.Maybe Lude.Natural)
bloplMaxResults = Lens.lens (maxResults :: BatchListObjectParents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListObjectParents)
{-# DEPRECATED bloplMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloplObjectReference :: Lens.Lens' BatchListObjectParents ObjectReference
bloplObjectReference = Lens.lens (objectReference :: BatchListObjectParents -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListObjectParents)
{-# DEPRECATED bloplObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchListObjectParents where
  toJSON BatchListObjectParents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
