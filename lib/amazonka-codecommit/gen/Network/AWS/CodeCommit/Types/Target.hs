{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Target
  ( Target (..),

    -- * Smart constructor
    mkTarget,

    -- * Lenses
    tDestinationReference,
    tRepositoryName,
    tSourceReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a target for a pull request.
--
-- /See:/ 'mkTarget' smart constructor.
data Target = Target'
  { destinationReference :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    sourceReference :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- * 'destinationReference' - The branch of the repository where the pull request changes are merged. Also known as the destination branch.
-- * 'repositoryName' - The name of the repository that contains the pull request.
-- * 'sourceReference' - The branch of the repository that contains the changes for the pull request. Also known as the source branch.
mkTarget ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceReference'
  Lude.Text ->
  Target
mkTarget pRepositoryName_ pSourceReference_ =
  Target'
    { destinationReference = Lude.Nothing,
      repositoryName = pRepositoryName_,
      sourceReference = pSourceReference_
    }

-- | The branch of the repository where the pull request changes are merged. Also known as the destination branch.
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDestinationReference :: Lens.Lens' Target (Lude.Maybe Lude.Text)
tDestinationReference = Lens.lens (destinationReference :: Target -> Lude.Maybe Lude.Text) (\s a -> s {destinationReference = a} :: Target)
{-# DEPRECATED tDestinationReference "Use generic-lens or generic-optics with 'destinationReference' instead." #-}

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRepositoryName :: Lens.Lens' Target Lude.Text
tRepositoryName = Lens.lens (repositoryName :: Target -> Lude.Text) (\s a -> s {repositoryName = a} :: Target)
{-# DEPRECATED tRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
--
-- /Note:/ Consider using 'sourceReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSourceReference :: Lens.Lens' Target Lude.Text
tSourceReference = Lens.lens (sourceReference :: Target -> Lude.Text) (\s a -> s {sourceReference = a} :: Target)
{-# DEPRECATED tSourceReference "Use generic-lens or generic-optics with 'sourceReference' instead." #-}

instance Lude.ToJSON Target where
  toJSON Target' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destinationReference" Lude..=) Lude.<$> destinationReference,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceReference" Lude..= sourceReference)
          ]
      )
