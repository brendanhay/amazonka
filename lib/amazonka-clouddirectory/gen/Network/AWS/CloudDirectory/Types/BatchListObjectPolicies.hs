{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
  ( BatchListObjectPolicies (..),

    -- * Smart constructor
    mkBatchListObjectPolicies,

    -- * Lenses
    bbNextToken,
    bbMaxResults,
    bbObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
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

-- | Creates a value of 'BatchListObjectPolicies' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object whose attributes will be listed.
mkBatchListObjectPolicies ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectPolicies
mkBatchListObjectPolicies pObjectReference_ =
  BatchListObjectPolicies'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbNextToken :: Lens.Lens' BatchListObjectPolicies (Lude.Maybe Lude.Text)
bbNextToken = Lens.lens (nextToken :: BatchListObjectPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectPolicies)
{-# DEPRECATED bbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbMaxResults :: Lens.Lens' BatchListObjectPolicies (Lude.Maybe Lude.Natural)
bbMaxResults = Lens.lens (maxResults :: BatchListObjectPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListObjectPolicies)
{-# DEPRECATED bbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbObjectReference :: Lens.Lens' BatchListObjectPolicies ObjectReference
bbObjectReference = Lens.lens (objectReference :: BatchListObjectPolicies -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListObjectPolicies)
{-# DEPRECATED bbObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchListObjectPolicies where
  toJSON BatchListObjectPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
