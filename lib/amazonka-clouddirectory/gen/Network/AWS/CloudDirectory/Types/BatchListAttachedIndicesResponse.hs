{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
  ( BatchListAttachedIndicesResponse (..),

    -- * Smart constructor
    mkBatchListAttachedIndicesResponse,

    -- * Lenses
    blaiIndexAttachments,
    blaiNextToken,
  )
where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListAttachedIndices' response operation.
--
-- /See:/ 'mkBatchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { -- | The indices attached to the specified object.
    indexAttachments :: Lude.Maybe [IndexAttachment],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListAttachedIndicesResponse' with the minimum fields required to make a request.
--
-- * 'indexAttachments' - The indices attached to the specified object.
-- * 'nextToken' - The pagination token.
mkBatchListAttachedIndicesResponse ::
  BatchListAttachedIndicesResponse
mkBatchListAttachedIndicesResponse =
  BatchListAttachedIndicesResponse'
    { indexAttachments =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The indices attached to the specified object.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaiIndexAttachments :: Lens.Lens' BatchListAttachedIndicesResponse (Lude.Maybe [IndexAttachment])
blaiIndexAttachments = Lens.lens (indexAttachments :: BatchListAttachedIndicesResponse -> Lude.Maybe [IndexAttachment]) (\s a -> s {indexAttachments = a} :: BatchListAttachedIndicesResponse)
{-# DEPRECATED blaiIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaiNextToken :: Lens.Lens' BatchListAttachedIndicesResponse (Lude.Maybe Lude.Text)
blaiNextToken = Lens.lens (nextToken :: BatchListAttachedIndicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListAttachedIndicesResponse)
{-# DEPRECATED blaiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListAttachedIndicesResponse where
  parseJSON =
    Lude.withObject
      "BatchListAttachedIndicesResponse"
      ( \x ->
          BatchListAttachedIndicesResponse'
            Lude.<$> (x Lude..:? "IndexAttachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
