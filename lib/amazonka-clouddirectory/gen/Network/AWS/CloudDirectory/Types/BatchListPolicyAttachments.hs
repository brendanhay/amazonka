{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
  ( BatchListPolicyAttachments (..),

    -- * Smart constructor
    mkBatchListPolicyAttachments,

    -- * Lenses
    blpasNextToken,
    blpasMaxResults,
    blpasPolicyReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached inside a 'BatchRead' operation. For more information, see 'ListPolicyAttachments' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    policyReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListPolicyAttachments' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'policyReference' - The reference that identifies the policy object.
mkBatchListPolicyAttachments ::
  -- | 'policyReference'
  ObjectReference ->
  BatchListPolicyAttachments
mkBatchListPolicyAttachments pPolicyReference_ =
  BatchListPolicyAttachments'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      policyReference = pPolicyReference_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpasNextToken :: Lens.Lens' BatchListPolicyAttachments (Lude.Maybe Lude.Text)
blpasNextToken = Lens.lens (nextToken :: BatchListPolicyAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListPolicyAttachments)
{-# DEPRECATED blpasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpasMaxResults :: Lens.Lens' BatchListPolicyAttachments (Lude.Maybe Lude.Natural)
blpasMaxResults = Lens.lens (maxResults :: BatchListPolicyAttachments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListPolicyAttachments)
{-# DEPRECATED blpasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpasPolicyReference :: Lens.Lens' BatchListPolicyAttachments ObjectReference
blpasPolicyReference = Lens.lens (policyReference :: BatchListPolicyAttachments -> ObjectReference) (\s a -> s {policyReference = a} :: BatchListPolicyAttachments)
{-# DEPRECATED blpasPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

instance Lude.ToJSON BatchListPolicyAttachments where
  toJSON BatchListPolicyAttachments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("PolicyReference" Lude..= policyReference)
          ]
      )
