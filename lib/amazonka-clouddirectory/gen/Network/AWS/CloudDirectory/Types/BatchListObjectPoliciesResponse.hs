{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
  ( BatchListObjectPoliciesResponse (..),

    -- * Smart constructor
    mkBatchListObjectPoliciesResponse,

    -- * Lenses
    blopsNextToken,
    blopsAttachedPolicyIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectPolicies' response operation.
--
-- /See:/ 'mkBatchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    attachedPolicyIds ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListObjectPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'attachedPolicyIds' - A list of policy @ObjectIdentifiers@ , that are attached to the object.
-- * 'nextToken' - The pagination token.
mkBatchListObjectPoliciesResponse ::
  BatchListObjectPoliciesResponse
mkBatchListObjectPoliciesResponse =
  BatchListObjectPoliciesResponse'
    { nextToken = Lude.Nothing,
      attachedPolicyIds = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsNextToken :: Lens.Lens' BatchListObjectPoliciesResponse (Lude.Maybe Lude.Text)
blopsNextToken = Lens.lens (nextToken :: BatchListObjectPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectPoliciesResponse)
{-# DEPRECATED blopsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
--
-- /Note:/ Consider using 'attachedPolicyIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsAttachedPolicyIds :: Lens.Lens' BatchListObjectPoliciesResponse (Lude.Maybe [Lude.Text])
blopsAttachedPolicyIds = Lens.lens (attachedPolicyIds :: BatchListObjectPoliciesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {attachedPolicyIds = a} :: BatchListObjectPoliciesResponse)
{-# DEPRECATED blopsAttachedPolicyIds "Use generic-lens or generic-optics with 'attachedPolicyIds' instead." #-}

instance Lude.FromJSON BatchListObjectPoliciesResponse where
  parseJSON =
    Lude.withObject
      "BatchListObjectPoliciesResponse"
      ( \x ->
          BatchListObjectPoliciesResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "AttachedPolicyIds" Lude..!= Lude.mempty)
      )
