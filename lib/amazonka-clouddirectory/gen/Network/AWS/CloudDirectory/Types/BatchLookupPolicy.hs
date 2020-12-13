{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicy
  ( BatchLookupPolicy (..),

    -- * Smart constructor
    mkBatchLookupPolicy,

    -- * Lenses
    blpfNextToken,
    blpfObjectReference,
    blpfMaxResults,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists all policies from the root of the Directory to the object specified inside a 'BatchRead' operation. For more information, see 'LookupPolicy' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchLookupPolicy' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - Reference that identifies the object whose policies will be looked up.
-- * 'maxResults' - The maximum number of results to retrieve.
mkBatchLookupPolicy ::
  -- | 'objectReference'
  ObjectReference ->
  BatchLookupPolicy
mkBatchLookupPolicy pObjectReference_ =
  BatchLookupPolicy'
    { nextToken = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpfNextToken :: Lens.Lens' BatchLookupPolicy (Lude.Maybe Lude.Text)
blpfNextToken = Lens.lens (nextToken :: BatchLookupPolicy -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchLookupPolicy)
{-# DEPRECATED blpfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Reference that identifies the object whose policies will be looked up.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpfObjectReference :: Lens.Lens' BatchLookupPolicy ObjectReference
blpfObjectReference = Lens.lens (objectReference :: BatchLookupPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: BatchLookupPolicy)
{-# DEPRECATED blpfObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpfMaxResults :: Lens.Lens' BatchLookupPolicy (Lude.Maybe Lude.Natural)
blpfMaxResults = Lens.lens (maxResults :: BatchLookupPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchLookupPolicy)
{-# DEPRECATED blpfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.ToJSON BatchLookupPolicy where
  toJSON BatchLookupPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )
