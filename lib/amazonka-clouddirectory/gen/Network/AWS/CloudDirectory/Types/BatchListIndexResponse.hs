{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndexResponse
  ( BatchListIndexResponse (..),

    -- * Smart constructor
    mkBatchListIndexResponse,

    -- * Lenses
    bliIndexAttachments,
    bliNextToken,
  )
where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListIndex' response operation.
--
-- /See:/ 'mkBatchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { -- | The objects and indexed values attached to the index.
    indexAttachments :: Lude.Maybe [IndexAttachment],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListIndexResponse' with the minimum fields required to make a request.
--
-- * 'indexAttachments' - The objects and indexed values attached to the index.
-- * 'nextToken' - The pagination token.
mkBatchListIndexResponse ::
  BatchListIndexResponse
mkBatchListIndexResponse =
  BatchListIndexResponse'
    { indexAttachments = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The objects and indexed values attached to the index.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliIndexAttachments :: Lens.Lens' BatchListIndexResponse (Lude.Maybe [IndexAttachment])
bliIndexAttachments = Lens.lens (indexAttachments :: BatchListIndexResponse -> Lude.Maybe [IndexAttachment]) (\s a -> s {indexAttachments = a} :: BatchListIndexResponse)
{-# DEPRECATED bliIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliNextToken :: Lens.Lens' BatchListIndexResponse (Lude.Maybe Lude.Text)
bliNextToken = Lens.lens (nextToken :: BatchListIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListIndexResponse)
{-# DEPRECATED bliNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListIndexResponse where
  parseJSON =
    Lude.withObject
      "BatchListIndexResponse"
      ( \x ->
          BatchListIndexResponse'
            Lude.<$> (x Lude..:? "IndexAttachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
