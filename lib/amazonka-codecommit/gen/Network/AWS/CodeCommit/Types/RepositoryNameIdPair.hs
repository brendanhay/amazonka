{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryNameIdPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryNameIdPair
  ( RepositoryNameIdPair (..),

    -- * Smart constructor
    mkRepositoryNameIdPair,

    -- * Lenses
    rnipRepositoryId,
    rnipRepositoryName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a repository name and ID.
--
-- /See:/ 'mkRepositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { repositoryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepositoryNameIdPair' with the minimum fields required to make a request.
--
-- * 'repositoryId' - The ID associated with the repository.
-- * 'repositoryName' - The name associated with the repository.
mkRepositoryNameIdPair ::
  RepositoryNameIdPair
mkRepositoryNameIdPair =
  RepositoryNameIdPair'
    { repositoryId = Lude.Nothing,
      repositoryName = Lude.Nothing
    }

-- | The ID associated with the repository.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnipRepositoryId :: Lens.Lens' RepositoryNameIdPair (Lude.Maybe Lude.Text)
rnipRepositoryId = Lens.lens (repositoryId :: RepositoryNameIdPair -> Lude.Maybe Lude.Text) (\s a -> s {repositoryId = a} :: RepositoryNameIdPair)
{-# DEPRECATED rnipRepositoryId "Use generic-lens or generic-optics with 'repositoryId' instead." #-}

-- | The name associated with the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnipRepositoryName :: Lens.Lens' RepositoryNameIdPair (Lude.Maybe Lude.Text)
rnipRepositoryName = Lens.lens (repositoryName :: RepositoryNameIdPair -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: RepositoryNameIdPair)
{-# DEPRECATED rnipRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.FromJSON RepositoryNameIdPair where
  parseJSON =
    Lude.withObject
      "RepositoryNameIdPair"
      ( \x ->
          RepositoryNameIdPair'
            Lude.<$> (x Lude..:? "repositoryId") Lude.<*> (x Lude..:? "repositoryName")
      )
