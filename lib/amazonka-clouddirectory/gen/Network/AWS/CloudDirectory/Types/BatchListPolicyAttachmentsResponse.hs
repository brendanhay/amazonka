-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
  ( BatchListPolicyAttachmentsResponse (..),

    -- * Smart constructor
    mkBatchListPolicyAttachmentsResponse,

    -- * Lenses
    blpaObjectIdentifiers,
    blpaNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListPolicyAttachments' response operation.
--
-- /See:/ 'mkBatchListPolicyAttachmentsResponse' smart constructor.
data BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse'
  { objectIdentifiers ::
      Lude.Maybe
        [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListPolicyAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'objectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
mkBatchListPolicyAttachmentsResponse ::
  BatchListPolicyAttachmentsResponse
mkBatchListPolicyAttachmentsResponse =
  BatchListPolicyAttachmentsResponse'
    { objectIdentifiers =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpaObjectIdentifiers :: Lens.Lens' BatchListPolicyAttachmentsResponse (Lude.Maybe [Lude.Text])
blpaObjectIdentifiers = Lens.lens (objectIdentifiers :: BatchListPolicyAttachmentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {objectIdentifiers = a} :: BatchListPolicyAttachmentsResponse)
{-# DEPRECATED blpaObjectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpaNextToken :: Lens.Lens' BatchListPolicyAttachmentsResponse (Lude.Maybe Lude.Text)
blpaNextToken = Lens.lens (nextToken :: BatchListPolicyAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListPolicyAttachmentsResponse)
{-# DEPRECATED blpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListPolicyAttachmentsResponse where
  parseJSON =
    Lude.withObject
      "BatchListPolicyAttachmentsResponse"
      ( \x ->
          BatchListPolicyAttachmentsResponse'
            Lude.<$> (x Lude..:? "ObjectIdentifiers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
