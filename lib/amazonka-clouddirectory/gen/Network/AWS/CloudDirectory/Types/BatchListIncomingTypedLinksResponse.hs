-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
  ( BatchListIncomingTypedLinksResponse (..),

    -- * Smart constructor
    mkBatchListIncomingTypedLinksResponse,

    -- * Lenses
    blitlLinkSpecifiers,
    blitlNextToken,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListIncomingTypedLinks' response operation.
--
-- /See:/ 'mkBatchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { linkSpecifiers ::
      Lude.Maybe
        [TypedLinkSpecifier],
    nextToken ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListIncomingTypedLinksResponse' with the minimum fields required to make a request.
--
-- * 'linkSpecifiers' - Returns one or more typed link specifiers as output.
-- * 'nextToken' - The pagination token.
mkBatchListIncomingTypedLinksResponse ::
  BatchListIncomingTypedLinksResponse
mkBatchListIncomingTypedLinksResponse =
  BatchListIncomingTypedLinksResponse'
    { linkSpecifiers =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Returns one or more typed link specifiers as output.
--
-- /Note:/ Consider using 'linkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlLinkSpecifiers :: Lens.Lens' BatchListIncomingTypedLinksResponse (Lude.Maybe [TypedLinkSpecifier])
blitlLinkSpecifiers = Lens.lens (linkSpecifiers :: BatchListIncomingTypedLinksResponse -> Lude.Maybe [TypedLinkSpecifier]) (\s a -> s {linkSpecifiers = a} :: BatchListIncomingTypedLinksResponse)
{-# DEPRECATED blitlLinkSpecifiers "Use generic-lens or generic-optics with 'linkSpecifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlNextToken :: Lens.Lens' BatchListIncomingTypedLinksResponse (Lude.Maybe Lude.Text)
blitlNextToken = Lens.lens (nextToken :: BatchListIncomingTypedLinksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListIncomingTypedLinksResponse)
{-# DEPRECATED blitlNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListIncomingTypedLinksResponse where
  parseJSON =
    Lude.withObject
      "BatchListIncomingTypedLinksResponse"
      ( \x ->
          BatchListIncomingTypedLinksResponse'
            Lude.<$> (x Lude..:? "LinkSpecifiers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
