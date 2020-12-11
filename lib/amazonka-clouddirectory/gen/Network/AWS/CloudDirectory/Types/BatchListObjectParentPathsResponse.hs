-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
  ( BatchListObjectParentPathsResponse (..),

    -- * Smart constructor
    mkBatchListObjectParentPathsResponse,

    -- * Lenses
    bloppPathToObjectIdentifiersList,
    bloppNextToken,
  )
where

import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectParentPaths' response operation.
--
-- /See:/ 'mkBatchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { pathToObjectIdentifiersList ::
      Lude.Maybe
        [PathToObjectIdentifiers],
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

-- | Creates a value of 'BatchListObjectParentPathsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'pathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
mkBatchListObjectParentPathsResponse ::
  BatchListObjectParentPathsResponse
mkBatchListObjectParentPathsResponse =
  BatchListObjectParentPathsResponse'
    { pathToObjectIdentifiersList =
        Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- /Note:/ Consider using 'pathToObjectIdentifiersList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloppPathToObjectIdentifiersList :: Lens.Lens' BatchListObjectParentPathsResponse (Lude.Maybe [PathToObjectIdentifiers])
bloppPathToObjectIdentifiersList = Lens.lens (pathToObjectIdentifiersList :: BatchListObjectParentPathsResponse -> Lude.Maybe [PathToObjectIdentifiers]) (\s a -> s {pathToObjectIdentifiersList = a} :: BatchListObjectParentPathsResponse)
{-# DEPRECATED bloppPathToObjectIdentifiersList "Use generic-lens or generic-optics with 'pathToObjectIdentifiersList' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloppNextToken :: Lens.Lens' BatchListObjectParentPathsResponse (Lude.Maybe Lude.Text)
bloppNextToken = Lens.lens (nextToken :: BatchListObjectParentPathsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectParentPathsResponse)
{-# DEPRECATED bloppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Lude.FromJSON BatchListObjectParentPathsResponse where
  parseJSON =
    Lude.withObject
      "BatchListObjectParentPathsResponse"
      ( \x ->
          BatchListObjectParentPathsResponse'
            Lude.<$> (x Lude..:? "PathToObjectIdentifiersList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextToken")
      )
