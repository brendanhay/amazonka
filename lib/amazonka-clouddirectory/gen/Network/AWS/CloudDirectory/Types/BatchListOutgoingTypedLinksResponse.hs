{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
  ( BatchListOutgoingTypedLinksResponse (..),

    -- * Smart constructor
    mkBatchListOutgoingTypedLinksResponse,

    -- * Lenses
    blotlTypedLinkSpecifiers,
    blotlNextToken,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListOutgoingTypedLinks' response operation.
--
-- /See:/ 'mkBatchListOutgoingTypedLinksResponse' smart constructor.
data BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Lude.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListOutgoingTypedLinksResponse' with the minimum fields required to make a request.
--
-- * 'typedLinkSpecifiers' - Returns a typed link specifier as output.
-- * 'nextToken' - The pagination token.
mkBatchListOutgoingTypedLinksResponse ::
  BatchListOutgoingTypedLinksResponse
mkBatchListOutgoingTypedLinksResponse =
  BatchListOutgoingTypedLinksResponse'
    { typedLinkSpecifiers =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlTypedLinkSpecifiers :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Lude.Maybe [TypedLinkSpecifier])
blotlTypedLinkSpecifiers = Lens.lens (typedLinkSpecifiers :: BatchListOutgoingTypedLinksResponse -> Lude.Maybe [TypedLinkSpecifier]) (\s a -> s {typedLinkSpecifiers = a} :: BatchListOutgoingTypedLinksResponse)
{-# DEPRECATED blotlTypedLinkSpecifiers "Use generic-lens or generic-optics with 'typedLinkSpecifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlNextToken :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Lude.Maybe Lude.Text)
blotlNextToken = Lens.lens (nextToken :: BatchListOutgoingTypedLinksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListOutgoingTypedLinksResponse)
{-# DEPRECATED blotlNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListOutgoingTypedLinksResponse where
  parseJSON =
    Lude.withObject
      "BatchListOutgoingTypedLinksResponse"
      ( \x ->
          BatchListOutgoingTypedLinksResponse'
            Lude.<$> (x Lude..:? "TypedLinkSpecifiers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
