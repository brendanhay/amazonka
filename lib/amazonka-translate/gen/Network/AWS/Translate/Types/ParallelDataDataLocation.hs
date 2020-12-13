{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataDataLocation
  ( ParallelDataDataLocation (..),

    -- * Smart constructor
    mkParallelDataDataLocation,

    -- * Lenses
    pddlLocation,
    pddlRepositoryType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate.
--
-- /See:/ 'mkParallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { -- | The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
    location :: Lude.Text,
    -- | Describes the repository that contains the parallel data input file.
    repositoryType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParallelDataDataLocation' with the minimum fields required to make a request.
--
-- * 'location' - The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
-- * 'repositoryType' - Describes the repository that contains the parallel data input file.
mkParallelDataDataLocation ::
  -- | 'location'
  Lude.Text ->
  -- | 'repositoryType'
  Lude.Text ->
  ParallelDataDataLocation
mkParallelDataDataLocation pLocation_ pRepositoryType_ =
  ParallelDataDataLocation'
    { location = pLocation_,
      repositoryType = pRepositoryType_
    }

-- | The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddlLocation :: Lens.Lens' ParallelDataDataLocation Lude.Text
pddlLocation = Lens.lens (location :: ParallelDataDataLocation -> Lude.Text) (\s a -> s {location = a} :: ParallelDataDataLocation)
{-# DEPRECATED pddlLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Describes the repository that contains the parallel data input file.
--
-- /Note:/ Consider using 'repositoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddlRepositoryType :: Lens.Lens' ParallelDataDataLocation Lude.Text
pddlRepositoryType = Lens.lens (repositoryType :: ParallelDataDataLocation -> Lude.Text) (\s a -> s {repositoryType = a} :: ParallelDataDataLocation)
{-# DEPRECATED pddlRepositoryType "Use generic-lens or generic-optics with 'repositoryType' instead." #-}

instance Lude.FromJSON ParallelDataDataLocation where
  parseJSON =
    Lude.withObject
      "ParallelDataDataLocation"
      ( \x ->
          ParallelDataDataLocation'
            Lude.<$> (x Lude..: "Location") Lude.<*> (x Lude..: "RepositoryType")
      )
