{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
  ( BatchListObjectChildrenResponse (..),

    -- * Smart constructor
    mkBatchListObjectChildrenResponse,

    -- * Lenses
    blocChildren,
    blocNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectChildren' response operation.
--
-- /See:/ 'mkBatchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { children ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'BatchListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- * 'children' - The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
-- * 'nextToken' - The pagination token.
mkBatchListObjectChildrenResponse ::
  BatchListObjectChildrenResponse
mkBatchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    { children = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocChildren :: Lens.Lens' BatchListObjectChildrenResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
blocChildren = Lens.lens (children :: BatchListObjectChildrenResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {children = a} :: BatchListObjectChildrenResponse)
{-# DEPRECATED blocChildren "Use generic-lens or generic-optics with 'children' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocNextToken :: Lens.Lens' BatchListObjectChildrenResponse (Lude.Maybe Lude.Text)
blocNextToken = Lens.lens (nextToken :: BatchListObjectChildrenResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectChildrenResponse)
{-# DEPRECATED blocNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListObjectChildrenResponse where
  parseJSON =
    Lude.withObject
      "BatchListObjectChildrenResponse"
      ( \x ->
          BatchListObjectChildrenResponse'
            Lude.<$> (x Lude..:? "Children" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
